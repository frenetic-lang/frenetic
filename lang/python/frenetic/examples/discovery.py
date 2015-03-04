import frenetic, networkx, base64, binascii, array, struct
from ryu.lib.packet import packet, packet_base, ethernet, arp
from frenetic.syntax import *
from tornado.ioloop import PeriodicCallback

def get(pkt,protocol):
    for p in pkt:
        if p.protocol_name == protocol:
            return p

# Helper function to ensure the string is always the same
# when creating a node that is a port
def port_node(switch_id, port):
  return str(switch_id) + ':' + str(port)

# Packet to be encoded for sending Probes over the probocol
class ProbeData(packet_base.PacketBase):

  PROBOCOL = 0x808
  _PACK_STR = '!LH'
  _MIN_LEN = struct.calcsize(_PACK_STR)
  _TYPE = {
      'ascii': [
          'src_switch', 'src_port'
      ]
  }

  def __init__(self, src_switch, src_port):
    self.src_switch = src_switch
    self.src_port = src_port

  @classmethod
  def parser(cls, buf):
      (src_switch, src_port) = struct.unpack_from(cls._PACK_STR, buf)
      return cls(src_switch, src_port), cls._TYPES.get(cls.PROBOCOL), buf[ProbeData._MIN_LEN:]

  def serialize(self, payload, prev):
      return struct.pack(ProbeData._PACK_STR, self.src_switch, self.src_port)

ProbeData.register_packet_type(ProbeData, ProbeData.PROBOCOL)

# Helper class to store switch information
class SwitchRef(object):
  def __init__(self, id, ports):
    self.id = id
    self.ports = ports

# Create a policy that given a SwitchRef, floods all input to its ports
def flood_switch_policy(switch):
  assert isinstance(switch, SwitchRef)
  pol = False
  for src in switch.ports:
    test = Filter(Test(Location(Physical(src))))
    actions = False
    for dst in switch.ports:
      if src == dst:
        continue
      action = test >> Mod(Location(Physical(dst)))
      if not actions:
        actions = action
      else:
        actions = action | actions
    if not pol:
      pol = actions
    else:
      pol = actions | pol
  return Filter(Test(Switch(switch.id))) >> pol

class Discovery(frenetic.App):

  def __init__(self, state):
    frenetic.App.__init__(self)
    self.state = state
    self.update(self.global_policy())
    # Every 10 seconds send out probes on ports we don't know about
    PeriodicCallback(self.run_probe, 10000).start()

  def run_probe(self):
    for probe_data in self.state.probes:
      # Build a PROBOCOL packet and send it out
      print "Sending probe: (%s, %s)" % (probe_data.src_switch, probe_data.src_port)
      pkt = packet.Packet()
      pkt.add_protocol(ethernet.ethernet(ethertype=ProbeData.PROBOCOL))
      pkt.add_protocol(probe_data)
      pkt.serialize()
      payload = NotBuffered(binascii.a2b_base64(binascii.b2a_base64(pkt.data)))
      actions = [Output(Physical(probe_data.src_port))]
      self.pkt_out(probe_data.src_switch, payload, actions)

  def global_policy(self):
    # All PROBOCOL traffic is sent to the controller, otherwise flood
    probe_traffic = Filter(Test(EthType(ProbeData.PROBOCOL))) >> Mod(Location(Pipe("http")))
    pols = Filter(Not(Test(EthType(ProbeData.PROBOCOL)))) >> Union([flood_switch_policy(ps) for ps in self.state.switches.values()])
    return pols | probe_traffic

  def add_switch(self, switch_ref):
    # Add a node for a switch and each of its ports.
    # Switch Nodes are integer values
    # Port Nodes are strings '<switch>:<port>' for example switch 3 port 2 would be
    # the string '3:2'
    self.state.network.add_node(switch_ref.id)
    for port in switch_ref.ports:
      node = port_node(switch_ref.id, port)
      self.state.network.add_node(node)
      self.state.network.add_edge(node, switch_ref.id)
      self.state.network.add_edge(switch_ref.id, node)

  def remove_switch(self, switch_ref):
    # Remove a switch and each of its ports from the graph
    self.state.network.remove_node(switch_ref.id)
    for port in switch_ref.ports:
      node = port_node(switch_ref.id, port)
      self.state.network.remove_node(node)

  def create_probes(self, switch_ref):
    for port in switch_ref.ports:
      self.state.probes.add((switch_ref.id, port))

  def discard_probes(self, switch_ref):
    for port in switch_ref.ports:
      self.state.probes.discard((switch_ref.id, port))

  def switch_up(self, switch_id, ports):
    # When a switch comes up, add it to the network and create
    # probes for each of its ports
    print "switch_up(%s, %s)" % (switch_id, ports)
    self.state.switches[switch_id] = SwitchRef(switch_id, ports)
    self.add_switch(self.state.switches[switch_id])
    self.create_probes(self.state.switches[switch_id])
    self.update(self.global_policy())
    
  def switch_down(self, switch_id):
    # When a switch goes down, remove any unresolved probes and remove 
    # it from the network graph
    print "switch_down(%s)" % switch_id
    self.discard_probes(self.state.switches[switch_id])
    self.remove_switch(self.state.switches[switch_id])
    del self.state.switches[switch_id]
    self.update(self.global_policy())

  def handle_probe(self, dst_switch, dst_port, src_switch, src_port):
    # When a probe is received, add edges based on where it traveled
    print "Probe received from (%s, %s) to (%s, %s)" % (src_switch, src_port, dst_switch, dst_port)
    node0 = port_node(dst_switch, dst_port)
    node1 = port_node(src_switch, src_port)
    self.state.network.add_edge(node0, node1)
    self.state.network.add_edge(node1, node0)
    self.state.probes.discard((dst_switch, dst_port))
    self.state.probes.discard((src_switch, src_port))

  def packet_in(self, switch_id, port_id, payload):
    pkt = packet.Packet(array.array('b', payload.data))
    p = get(pkt, 'ethernet')
    if (p.ethertype == ProbeData.PROBOCOL):
      probe_data = get(pkt, 'ProbeData')
      self.handle_probe(switch_id, port_id, probe_data.src_switch, probe_data.src_port)


class State(object):

  def __init__(self):
    self.network = networkx.Graph()
    self.switches = {}
    self.probes = set()

def packet_in(self, switch_id, port_id, payload):
  print "Packet in!"

def main(version):
  app = Discovery(State())
  app.start_event_loop()

if __name__ == '__main__':
  main(1)
