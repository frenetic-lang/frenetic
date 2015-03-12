import frenetic, networkx
from frenetic.syntax import *
from flood_switch import *
from state import *

class Routing(frenetic.App):

  client_id = "routing"

  def __init__(self, state, version):
    frenetic.App.__init__(self)
    self.version = version
    self.state = state
    self.state.register(self)

  def connected(self):
    self.update(drop)

  # Supresses the noisy output
  def packet_in(self, switch_id, port_id, payload):
    self.pkt_out(switch_id, payload, [])

  def run_update(self):
    # This function is invoked by State when the network changes
    self.update(self.policy())

  def build_path(self, switch_path, curr_switch, acc):
    # When there are no more switches to go through, we've
    # reached the host.
    if not switch_path:
      return Union(acc)

    # If there are more switches to go through, get the next one
    # find the port that connects the current switch to the next
    # switch, and filter from current switch along the port connecting
    # to the next_switch.
    next_switch = switch_path.pop()
    out_port = self.state.network[curr_switch][next_switch]['label']
    pol = Filter(Test(Switch(curr_switch))) >> Mod(Location(Physical(out_port)))
    acc.append(pol)
    return self.build_path(switch_path, next_switch, acc)

  def increase_weights(self, curr_node, remaining_path, network):
    if not remaining_path:
      return
    
    next_node = remaining_path.pop()
    # If this edge has a calculated moving average
    # we will add the moving_average/num_hosts to the networks
    # edge weight when we add someone to this path
    if 'weight' in self.state.network[curr_node][next_node]:
      moving_average = self.state.network[curr_node][next_node]['weight']
      curr_weight = network[curr_node][next_node]['weight']
      num_hosts = len(self.state.hosts())
      update_weight = curr_weight + (moving_average/num_hosts)
      network.add_edge(curr_node, next_node, weight = update_weight)
    self.increase_weights(next_node, remaining_path, network)

  def policy(self):
    hosts = self.state.hosts()
    paths = []

    no_weight_network = self.state.network.copy()
    for edge in no_weight_network.edges():
      no_weight_network.add_edge(edge[0], edge[1], weight=0)

    # For all (src, dst) pairs, find the shortest path
    for src_host in hosts:
      for dst_host in hosts:
        if src_host == dst_host:
          continue

        # Build a copy of the network containing only switches and the relevant hosts
        network_prime = no_weight_network.copy()
        for host in self.state.hosts():
          if not (host == src_host or host == dst_host):
            network_prime.remove_node(host)

        #If no path exists, skip
        if(not networkx.has_path(network_prime, src_host, dst_host)):
          continue

        # Otherwise, get the path and build that policy
        switch_path = networkx.shortest_path(network_prime, src_host, dst_host, 'weight')

        # If we are in version 2, we want to increase the edge weight for the selected path
        if self.version == 2:
          path_copy = list(switch_path)
          path_copy.reverse()
          self.increase_weights(path_copy.pop(), path_copy, no_weight_network)

        print "Found path %s" % switch_path
        switch_path.reverse()
        switch_path.pop()

        # Test that we are coming from the src_host and going to the dst_host
        test = Filter(Test(EthSrc(src_host)) & Test(EthDst(dst_host)))
        paths.append(test >> self.build_path(switch_path, switch_path.pop(), []))

    # If there exists a path, use it.
    return ((Filter(Test(EthType(0x800))) >> Union(paths)))
