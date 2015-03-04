import frenetic, networkx, base64, binascii, array, struct
from ryu.lib.packet import packet, packet_base, ethernet, arp
from frenetic.syntax import *
from tornado.ioloop import PeriodicCallback

# Probe protocol
probocol = 0x808

def get(pkt,protocol):
    for p in pkt:
        if p.protocol_name == protocol:
            return p

def to_str(switch_id, port):
  return str(switch_id) + ':' + str(port)

def from_str(src_string):
  strs = src_string.split(':')
  return (int(strs[0]), int(strs[1]))

def port_node(switch_id, port):
  return str(switch_id) + ':' + str(port)

class ProbeData(packet_base.PacketBase):

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
      return cls(src_switch, src_port), cls._TYPES.get(probocol), buf[ProbeData._MIN_LEN:]

  def serialize(self, payload, prev):
      return struct.pack(ProbeData._PACK_STR, self.src_switch, self.src_port)

ProbeData.register_packet_type(ProbeData, probocol)

class SwitchRef(object):
  
  def __init__(self, id, ports):
    self.id = id
    self.ports = ports

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
    PeriodicCallback(self.run_probe, 10000).start()

  def run_probe(self):
    for probe in self.state.probes:
      switch_id = probe[0]
      port = probe[1]
      print "Sending probe: (%s, %s)" % (switch_id, port)
      pkt = packet.Packet()
      pkt.add_protocol(ethernet.ethernet(ethertype=probocol))
      pkt.add_protocol(ProbeData(switch_id, port))
      pkt.serialize()
      payload = NotBuffered(binascii.a2b_base64(binascii.b2a_base64(pkt.data)))
      actions = [Output(Physical(port))]
      self.pkt_out(switch_id, payload, actions)

  def global_policy(self):
    probe_traffic = Filter(Test(EthType(probocol))) >> Mod(Location(Pipe("http")))
    pols = Filter(Not(Test(EthType(probocol)))) >> Union([flood_switch_policy(ps) for ps in self.state.switches.values()])
    return pols | probe_traffic

  def add_switch(self, switch_ref):
    self.state.network.add_node(switch_ref.id)
    for port in switch_ref.ports:
      node = port_node(switch_ref.id, port)
      self.state.network.add_node(node)
      self.state.network.add_edge(node, switch_ref.id)
      self.state.network.add_edge(switch_ref.id, node)

  def remove_switch(self, switch_ref):
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
    print "switch_up(%s, %s)" % (switch_id, ports)
    self.state.switches[switch_id] = SwitchRef(switch_id, ports)
    self.add_switch(self.state.switches[switch_id])
    self.create_probes(self.state.switches[switch_id])
    self.update(self.global_policy())
    
  def switch_down(self, switch_id):
    print "switch_down(%s)" % switch_id
    self.discard_probes(self.state.switches[switch_id])
    self.remove_switch(self.state.switches[switch_id])
    del self.state.switches[switch_id]
    self.update(self.global_policy())

  def handle_probe(self, dst_switch, dst_port, src_switch, src_port):
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
    if (p.ethertype == probocol):
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
