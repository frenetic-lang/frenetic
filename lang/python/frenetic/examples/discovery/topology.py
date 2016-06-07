import struct, frenetic, binascii, array, time, datetime, copy
from functools import partial
from frenetic.syntax import *
from frenetic.packet import *
from state import *
from tornado.ioloop import PeriodicCallback
from tornado.ioloop import IOLoop
from tornado.concurrent import return_future
from flood_switch import *
from probe import ProbeData

weight_check_interval = 5000
num_intervals = 5

class Count(object):

  def __init__(self, count):
    self.timestamp = time.time()
    self.count = count

  def __repr__(self):
    return "{ count : %s, timestamp : %s}" % (self.count, self.timestamp)

  @classmethod
  def moving_average(cls, counts):
    # ensure the original counts is unmodified
    counts = list(counts)
    size = len(counts)
    assert size > 1
    start_point = counts.pop()
    assert isinstance(start_point, Count)
    average = 0
    while counts:
      end_point = counts.pop()
      assert isinstance(end_point, Count)
      average = average + ((end_point.count - start_point.count)/(end_point.timestamp - start_point.timestamp))
      start_point = end_point
    return average/size

class Topology(frenetic.App):

  client_id = "topology"
  frenetic_http_host = "localhost"
  frenetic_http_port = "9000"

  def __init__(self, state, version):
    frenetic.App.__init__(self)
    self.version = version
    self.state = state
    self.state.register(self)

  def connected(self):
    # This is necessary to avoid a complete screwup on restart.
    self.update(self.policy())

    IOLoop.instance().add_timeout(datetime.timedelta(seconds=2), self.run_probe)
    IOLoop.instance().add_timeout(datetime.timedelta(seconds=30), self.host_discovery)

    # The controller may already be connected to several switches on startup.
    # This ensures that we probe them too.
    def handle_current_switches(switches):
      for switch_id in switches:
        self.switch_up(switch_id, switches[switch_id])
    self.current_switches(callback=handle_current_switches)

  def host_discovery(self):
    print "Internal links finalized. Now ready to discover hosts."
    self.state.set_mode("host_discovery")
    # In version 2, read port counters every 10 seconds too.
    if self.version == 2:
      PeriodicCallback(self.update_weights, weight_check_interval).start()
    self.state.notify()

  def update_next_callback(self, ftr):
    # Pull the edges out of the future and propogate the edges along
    # Yay for monads in python!
    edges = ftr.result()
    self.update_weights_helper(edges)

  def update_callback(self, ftr, edge, edges, callback):
    # Get the Counts for this edge
    counts = []
    if 'counts' in self.state.network[edge[0]][edge[1]]:
      counts = self.state.network[edge[0]][edge[1]]['counts']

    # Add the latest count
    data = ftr.result()
    curr_count = Count(data['rx_bytes'] + data['tx_bytes'])
    counts.append(curr_count)

    # remove older counts
    while len(counts) > num_intervals + 1:
      counts.pop(0)

    # Calculate the moving_average
    weight = 0
    if len(counts) > 1:
      weight = Count.moving_average(counts)

    # Update edge
    label = self.state.network[edge[0]][edge[1]]['label']
    self.state.network.add_edge(edge[0], edge[1], label=label, weight=weight, counts=counts)
    self.state._clean = False
    callback(edges)

  @return_future
  def update_next(self, edges, callback):
    # This should never be called with an empty edge list
    assert edges
    # Take the next edge, pull out the label and ask for the port_stats
    edge = edges.pop()
    switch_id = edge[0]
    dst_id = edge[1]
    port_id = self.state.network[switch_id][dst_id]['label']
    ftr = self.port_stats(switch_id, port_id)
    f = partial(self.update_callback,
                edge = edge,
                edges = edges,
                callback = callback)
    IOLoop.instance().add_future(ftr, f)

  def update_weights_helper(self, edges):
    # If there are no edges left, call notify.
    if edges:
      ftr = self.update_next(edges)
      IOLoop.instance().add_future(ftr, self.update_next_callback)
    else:
      self.state.notify()

  def update_weights(self):
    edges = networkx.get_edge_attributes(self.state.network, 'label').keys()
    self.update_weights_helper(edges)

  def run_update(self):
    # This function is invoked by State when the network changes
    self.update(self.policy())

  def run_probe(self):
    to_remove = set()
    for switch in self.state.switches().values():
      switch_id = switch[0]
      for port_id in switch[1]:
        probe_data = ProbeData(switch_id, port_id)
        print "Sending out probe for (%s, %s)" % (switch_id, port_id)
        # Build a PROBOCOL packet and send it out.  We use bogus source and destinations
        # because the next switch will pick it up.  Everyone else will just drop it.  
        pkt = Packet(ethType=ProbeData.PROBOCOL, ethSrc="00:00:de:ad:be:ef", ethDst="ef:be:ad:de:00:00")
        payload = pkt.to_payload(ryu_packet_headers = [probe_data])
        self.pkt_out(switch_id, payload, [SetPort(port_id)])

  def policy(self):
    if self.state.mode == "internal_discovery":
      # All PROBOCOL traffic is sent to the controller, otherwise flood
      probe_traffic = Filter(Test(EthType(ProbeData.PROBOCOL))) >> Mod(Location(Pipe("http")))
      sniff = Filter(EthTypeEq(0x806)) >> SendToController("http")
      return probe_traffic | sniff
    elif self.state.mode == "host_discovery":
      sniff = Filter(EthTypeEq(0x806)) >> SendToController("http")
      return sniff
    else:
      assert False

  def switch_up(self, switch_id, ports):
    # When a switch comes up, add it to the network and create
    # probes for each of its ports
    print "switch_up(%s, %s)" % (switch_id, ports)
    self.state.add_switch(switch_id, ports)

  def switch_down(self, switch_id):
    # When a switch goes down, remove any unresolved probes and remove
    # it from the network graph
    print "switch_down(%s)" % switch_id
    self.state.remove_switch(switch_id)
    self.state.notify()

  def handle_probe(self, dst_switch, dst_port, src_switch, src_port):
    # When a probe is received, add edges based on where it traveled
    print "Internal edge (%s, %s)--(%s, %s)" % (src_switch, src_port, dst_switch, dst_port)
    self.state.add_edge(dst_switch, src_switch, label=dst_port)
    self.state.add_edge(src_switch, dst_switch, label=src_port)

  def handle_sniff(self, pkt):
    # TODO(arjun): mobility
    if self.state.network.has_node(pkt.ethSrc):
      return

    if not pkt.switch in self.state.switches():
      return

    # TODO(arjun): Security vulnerability. What if some idiot sends a packet
    # with a broadcast source?
    self.state.add_host(pkt.ethSrc)
    for edge in self.state.network.out_edges(pkt.switch, data=True):
      if edge[2]["label"] == pkt.port:
        print "SANITY ERROR: Received an ARP packet on an internal link."
        print "Please do not rewire the network."
        return

    print "Edge (%s, %s)--%s" % (pkt.switch, pkt.port, pkt.ethSrc)
    # This switch / ports probe has not been seen
    # We will tentatively assume it is connected to the src host
    self.state.add_edge(pkt.switch, pkt.ethSrc, label=pkt.port)
    self.state.add_edge(pkt.ethSrc, pkt.switch)
    self.state.notify()

  def send_arp(self, pkt):
    for loc in self.state.network_edge():
      pkt_copy = copy.copy(pkt)
      dst_switch_id, dst_port_id = loc
      if loc == (pkt.switch, pkt.port):
        continue
      self.pkt_out(switch_id=dst_switch_id,
                   payload=pkt_copy.to_payload(),
                   actions=[SetPort(dst_port_id)])

  def packet_in(self, switch_id, port_id, payload):
    pkt = Packet.from_payload(switch_id, port_id, payload)

    if (pkt.ethType == ProbeData.PROBOCOL):
      probe_data = self.packet(payload, 'ProbeData')
      self.handle_probe(switch_id, port_id, probe_data.src_switch,
                        probe_data.src_port)
      # Drop the probe packet if it's buffered.  No-Op if it's unbuffered.
      self.pkt_out(switch_id, payload, [])
      return

    elif (pkt.ethType == 0x806):  # = arp
      self.handle_sniff(pkt)
      if (self.state.mode == "host_discovery"):
        self.send_arp(pkt)
      # Drop the ARP request packet.  We got all the data we needed from it.
      self.pkt_out(switch_id, payload, [])
      return

    print "ERROR: Received non-ARP / non-PROBOCOL packet: %s" % pkt
    self.pkt_out(switch_id, payload, [])



