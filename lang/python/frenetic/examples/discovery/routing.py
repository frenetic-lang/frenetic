import frenetic, networkx
from frenetic.syntax import *
from flood_switch import *
from state import *

class Routing(frenetic.App):

  client_id = "routing"

  def __init__(self, state, topo):
    frenetic.App.__init__(self)
    self.state = state
    self.state.register(self)
    self.topo = topo

  def run_update(self):
    self.update(self.policy() | self.topo.policy())

  def build_path(self, node_list, curr_switch, acc):
    if not node_list:
      print "acc len: %s" % len(acc)
      return Union(acc)
    next_switch = node_list.pop()
    out_port = self.state.network[curr_switch][next_switch]['label']
    pol = Filter(Test(Switch(curr_switch))) >> Mod(Location(Physical(out_port)))
    print "filter switch = %s; port := %s" % (curr_switch, out_port)
    acc.append(pol)
    return self.build_path(node_list, next_switch, acc)
    
  def policy(self):
    hosts = self.state.hosts()
    paths = []
 
    for src_host in hosts:
      for dst_host in hosts:
        if src_host == dst_host:
          continue

        # Build a copy of the network containing only switches and the relevant hosts
        network_prime = self.state.network.copy()
        for host in self.state.hosts():
          if not (host == src_host or host == dst_host):
            network_prime.remove_node(host)
          
        #If no path exists, skip
        if(not networkx.has_path(network_prime, src_host, dst_host)):
          continue
        # Otherwise, get the path and build that policy
        node_list = networkx.shortest_path(network_prime, src_host, dst_host)
        print "Building Path: %s" % node_list
        node_list.reverse()
        node_list.pop()
        test = Filter(Test(EthSrc(src_host)) & Test(EthDst(dst_host)))
        paths.append(test >> self.build_path(node_list, node_list.pop(), []))

    return (Union(paths) | 
            Union([flood_switch_policy(switch_ref) for switch_ref in self.state.switches().values()]))

  def packet_in(self, switch_id, port_id, payload):
    self.topo.packet_in(switch_id, port_id, payload)

  def switch_up(self, switch_id, ports):
    self.topo.switch_up(switch_id, ports)

  def switch_down(self, switch_id):
    self.topo.switch_down(switch_id)

