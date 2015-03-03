import frenetic
import networkx
import base64
import binascii
from ryu.lib.packet import packet
from ryu.lib.packet import ethernet
from ryu.lib.packet import arp
from frenetic.syntax import *
from tornado.ioloop import PeriodicCallback

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
    print "Running probes!"
    for switch in self.state.switches.values():
      switch_id = switch.id
      pkt = packet.Packet()
      pkt.add_protocol(ethernet.ethernet(ethertype=0x100, src=('00:00:00:00:00:0' + str(switch_id))))
#      pkt.add_protocol(arp.arp(proto=0x100))
      pkt.serialize()
      payload = NotBuffered(binascii.a2b_base64(binascii.b2a_base64(pkt.data)))
      actions = [Output(Physical(port)) for port in switch.ports]
      self.pkt_out(switch_id, payload, actions)

  def global_policy(self):
    probe_traffic = Filter(Test(EthType(0x100))) >> Mod(Location(Pipe("http")))
    pols = Union([flood_switch_policy(ps) for ps in self.state.switches.values()])
    return pols | probe_traffic

  def switch_up(self, switch_id, ports):
    print "switch_up(%s, %s)" % (switch_id, ports)
    self.state.switches[switch_id] = SwitchRef(switch_id, ports)
    self.state.network.add_node(switch_id)
    self.update(self.global_policy())
    

  def switch_down(self, switch_id):
    print "switch_down(%s)" % switch_id
    del self.state.switches[switch_id]
    self.state.network.remove_node(switch_id)
    self.update(self.global_policy())

  def packet_in(self, switch_id, port_id, payload):
    print "packet_in(%s,%s,payload)" % (switch_id, port_id)


class State(object):

  def __init__(self):
    self.network = networkx.Graph()
    self.switches = {}

def packet_in(self, switch_id, port_id, payload):
  print "Packet in!"

def main(version):
  app = Discovery(State())
  app.start_event_loop()

if __name__ == '__main__':
  main(1)
