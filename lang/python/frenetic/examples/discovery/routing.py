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

  def policy(self):
    hosts = self.state.hosts()
    paths = []

    # For all (src, dst) pairs, find the shortest path
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
        switch_path = networkx.shortest_path(network_prime, src_host, dst_host, 'weight')

        print "Found path %s" % switch_path
        switch_path.reverse()
        switch_path.pop()

        # Test that we are coming from the src_host and going to the dst_host
        test = Filter(Test(EthSrc(src_host)) & Test(EthDst(dst_host)))
        paths.append(test >> self.build_path(switch_path, switch_path.pop(), []))

    # If there exists a path, use it.
    # TODO(arjun): ARP flooding belongs in discovery, perhaps?
    return ((Filter(Test(EthType(0x800))) >> Union(paths)) |
            (Filter(Test(EthType(0x806))) >> Union([flood_switch_policy(switch_ref) for switch_ref in self.state.switches().values()])))
