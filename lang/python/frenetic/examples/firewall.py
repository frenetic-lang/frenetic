import frenetic, sys, json, time
from functools import partial
from frenetic.syntax import *
import single_switch_forwarding
import array
from ryu.lib.packet import packet
from tornado.ioloop import PeriodicCallback
from tornado.concurrent import chain_future

net_size = 2
clean_delay = 3

def get(pkt,protocol):
    for p in pkt:
        if p.protocol_name == protocol:
            return p


class Allowed(object):

  def __init__(self, trusted_ip, trusted_port, untrusted_ip, untrusted_port, query_label):
    self.trusted_ip = trusted_ip
    self.trusted_port = trusted_port
    self.untrusted_ip = untrusted_ip
    self.untrusted_port = untrusted_port
    self.query_label = query_label
    self.last_count = 0
    self.time_created = time.time()

  def __eq__(self, other):
    return (isinstance(other, self.__class__) and
            self.trusted_ip == other.trusted_ip and
            self.trusted_port == other.trusted_port and
            self.untrusted_ip == other.untrusted_ip and
            self.untrusted_port == other.untrusted_port)

  def __hash__(self):
    return self.trusted_ip.__hash__()

  def to_pred(self):
    return ((Test(IP4Src(self.trusted_ip)) &
             Test(TCPSrcPort(self.trusted_port)) &
             Test(IP4Dst(self.untrusted_ip)) &
             Test(TCPDstPort(self.untrusted_port))) |
            (Test(IP4Src(self.untrusted_ip)) &
             Test(TCPSrcPort(self.untrusted_port)) &
             Test(IP4Dst(self.trusted_ip)) &
             Test(TCPDstPort(self.trusted_port))))

  def query_pol(self):
    return Filter(self.to_pred()) >> Mod(Location(Query(self.query_label)))

class Trusted(object):

  def __init__(self, ips):
    self.ips = ips
    self.srcs = Or([Test(IP4Src(ip)) for ip in self.ips])
    self.dsts = Or([Test(IP4Dst(ip)) for ip in self.ips])

  def internal_pred(self):
    return (self.srcs & self.dsts)

  def external_pred(self):
    return (self.srcs & ~self.dsts) | (~self.srcs & self.dsts)

forwarding_pol = single_switch_forwarding.policy(net_size)

class Firewall(frenetic.App):

  client_id = "stateful_firewall"

  def __init__(self, state):
    frenetic.App.__init__(self)
    self.state = state
    self.update(self.global_policy())
    if self.state.version == 3:
      PeriodicCallback(self.run_clean, clean_delay * 1000).start()

  def allowed_pred(self):
    return Or([x.to_pred() for x in self.state.allowed])

  def global_policy(self):
    # TODO(arjun): This policy floods the controller with packets from outside
    # going in if they are not allowed. This should be changed for V1. But,
    # for V2 pending connections will have to be allowed too.
    internal = self.state.trusted.internal_pred()
    external = self.state.trusted.external_pred()
    allowed = self.allowed_pred()
    queries = Union([ x.query_pol() for x in self.state.allowed])

    return internal.ite(
      forwarding_pol,
      Filter(external) >>
      allowed.ite(
        forwarding_pol | queries,
        Mod(Location(Pipe("http")))))

  def clean_callback(self, packets, curr_bytes, allowed):
    # If the connection has become stale remove it
    # NOTE(arjun): Should never be greater than, I hope!
    assert allowed.last_count <= curr_bytes
    if(allowed.last_count == curr_bytes):
        self.state.allowed.remove(allowed)
        return
    allowed.last_count = curr_bytes

  def find_pending(self, to_remove_set, pending):
    if(pending.time_created + clean_delay < time.time()):
      to_remove_set.add(pending)
    return to_remove_set

  def run_clean(self):
    # Check allowed connections
    futures = []
    for allowed in self.state.allowed:
      f = partial(self.clean_callback, allowed=allowed)
      futures.append(self.query(allowed.query_label, f))
    if (len(futures) > 0):
      future = reduce(chain_future, futures)
    # Check for expired pending
    to_remove_set = reduce(self.find_pending, self.state.pending, set())
    for pending in to_remove_set:
      self.state.pending.remove(pending)
      
    # TODO(arjun): Update policy if anything was cleaned

  def packet_in_v1(self, switch_id, port_id, payload, src_ip, dst_ip,
                   src_tcp_port, dst_tcp_port):
    if not (src_ip in self.state.trusted.ips and not (dst_ip in self.state.trusted.ips)):
      self.pkt_out(switch_id, payload, [])
      return

    # Packet is going from trusted to untrusted
    self.state.allow(Allowed(src_ip, src_tcp_port, dst_ip, dst_tcp_port, self.state.next_query_label()))
    self.update(self.global_policy())
    # TODO(arjun): Should send the packet. API should expose the Freneti
    # interpreter, or update should allow you to provide a packet to apply the
    # policy to.
    return

  def packet_in_v2(self, switch_id, port_id, payload, src_ip, dst_ip,
                   src_tcp_port, dst_tcp_port):
    allow_entry = Allowed(src_ip, src_tcp_port, dst_ip, dst_tcp_port, self.state.next_query_label())

    if src_ip in self.state.trusted.ips and not (dst_ip in self.state.trusted.ips):
      self.state.add_pending(allow_entry)
      # TODO(arjun): Terrible hack
      pt = 1 if dst_ip == "10.0.0.1" else 2 if dst_ip == "10.0.0.2" else 0
      self.pkt_out(switch_id, payload, [Output(Physical(pt))])
      self.update(self.global_policy())
      return

    # Describes the trusted -> untrusted entry that should have been created
    reverse_allow_entry = Allowed(dst_ip, dst_tcp_port, src_ip, src_tcp_port, self.state.next_query_label())
    if reverse_allow_entry in self.state.pending:
      self.state.remove_pending(reverse_allow_entry)
      self.state.allow(reverse_allow_entry)
      self.update(self.global_policy())
      pt = 1 if dst_ip == "10.0.0.1" else 2 if dst_ip == "10.0.0.2" else 0
      self.pkt_out(switch_id, payload, [Output(Physical(pt))])
      return

    self.pkt_out(switch_id, payload, [])

  def packet_in(self, switch_id, port_id, payload):
    pkt = packet.Packet(array.array('b', payload.data))
    p = get(pkt,'ethernet')
    ip = get(pkt, "ipv4")
    tcp = get(pkt, "tcp")
    print "Packet in: %s" % pkt
    if ip == None or tcp == None:
      print "Not TCP/IP packet. Dropped."
      self.pkt_out(switch = switch_id, payload = payload, actions = [])
      return

    if self.state.version == 1:
      f = self.packet_in_v1
    elif self.state.version == 2 or self.state.version == 3:
      f = self.packet_in_v2
    else:
      assert False

    f(switch_id, port_id, payload, ip.src, ip.dst, tcp.src_port, tcp.dst_port)

class State(object):

  def __init__(self, trusted_ips, version):
    self.version = version
    self.trusted = Trusted(trusted_ips)
    self.allowed = []
    # We assume that pending set is always empty in V1
    self.pending = set()
    self.__lbl = 0

  def next_query_label(self):
    n = str(self.__lbl)
    self.__lbl = self.__lbl + 1
    return n

  def allow(self, allowed):
    self.allowed += [allowed]

  def add_pending(self, allowed):
    self.pending.add(allowed)

  def remove_pending(self, allowed):
    self.pending.remove(allowed)


def main(version):
    app = Firewall(State(['10.0.0.1'], version))
    app.start_event_loop()

if __name__ == '__main__':
    if len(sys.argv) == 2:
      main(int(sys.argv[1]))
    else:
      print "Running version 1"
      main(1)

