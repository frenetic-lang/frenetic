import frenetic
import json
from frenetic.syntax import *
import single_switch_forwarding
import array
from ryu.lib.packet import packet

net_size = 2

def get(pkt,protocol):
    for p in pkt:
        if p.protocol_name == protocol:
            return p

class Allowed(object):

  def __init__(self, trusted_ip, trusted_port, untrusted_ip, untrusted_port):
    self.trusted_ip = trusted_ip
    self.trusted_port = trusted_port
    self.untrusted_ip = untrusted_ip
    self.untrusted_port = untrusted_port

  def to_pred(self):
    return ((Test(IP4Src(self.trusted_ip)) &
             Test(TCPSrcPort(self.trusted_port)) &
             Test(IP4Dst(self.untrusted_ip)) &
             Test(TCPDstPort(self.untrusted_port))) |
            (Test(IP4Src(self.untrusted_ip)) &
             Test(TCPSrcPort(self.untrusted_port)) &
             Test(IP4Dst(self.trusted_ip)) &
             Test(TCPDstPort(self.trusted_port))))

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

  def firewall_pol(self):
    allowed = Or([x.to_pred() for x in self.state.allowed])
    return (Filter(allowed) |
            (Filter(self.state.trusted.srcs & ~self.state.trusted.dsts & ~allowed) >>
             Mod(Location(Pipe("http")))))

  def global_policy(self):
    internal = Filter(self.state.trusted.internal_pred()) >> forwarding_pol
    external = (Filter(self.state.trusted.external_pred()) >>
      self.firewall_pol() >>
      (Filter(Test(Location(Pipe("http")))) |
       Filter(~Test(Location(Pipe("http")))) >> forwarding_pol))
    gp = Filter(Test(EthType(0x800))) >> (internal | external)
    return  gp

  def packet_in(self, switch_id, port_id, payload):
    pkt = packet.Packet(array.array('b', payload.data))
    p = get(pkt,'ethernet')
    ip = get(pkt, "ipv4")
    tcp = get(pkt, "tcp")
    print "Packet in: %s" % pkt
    if ip == None or tcp == None:
      print "Not TCP/IP packet"
      return # TODO(arjun): Drop?
    if not (ip.src in self.state.trusted.ips and not (ip.dst in self.state.trusted.ips)):
      return #TODO(arjun): Drop?
    self.state.allow(Allowed(ip.src, tcp.src_port, ip.dst, tcp.dst_port))
    self.update(self.global_policy())
#    pt = 1 if ip.dst == "10.0.0.1" else 2 if ip.dst == "10.0.0.2" else 0        
#    print "\n\n\nSending message to %s using port %s\n\n\n" % (ip.dst, str(pt))
#    self.pkt_out(switch = switch_id,
#                 payload = payload,
#                 actions = [Output(Physical(pt))])
    # TODO(arjun): Send packet out

class State(object):

  def __init__(self, trusted_ips):
    self.trusted = Trusted(trusted_ips)
    self.allowed = []

  def allow(self, allowed):
    self.allowed += [allowed]


def main():
    app = Firewall(State(['10.0.0.1']))
    app.start_event_loop()

if __name__ == '__main__':
    main()

