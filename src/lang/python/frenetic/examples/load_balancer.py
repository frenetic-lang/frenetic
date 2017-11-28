import frenetic, sys, json, time, argparse
import os.path
from frenetic.syntax import *
from frenetic.packet import *
import array

client_port = 1

class State(object):

  def __init__(self, server_ports):
    self.connections = {}
    self.server_ports = server_ports
    self.next_server_index = 0

  def next_server_port(self):
    n = self.next_server_index
    self.next_server_index = (n + 1) % len(self.server_ports)
    return self.server_ports[n]

  def new_connection(self, src_port):
    if not(src_port in self.connections):
      self.connections[src_port] = self.next_server_port()
    return self.connections[src_port]

class LoadBalancer(frenetic.App):

  client_id = "load_balancer"

  def __init__(self, client_port, state):
    frenetic.App.__init__(self)
    self.client_port = client_port
    self.state = state

  # Returns 0 as a default
  def packet_src_port(self, switch_id, port_id, payload):
    pkt = Packet.from_payload(switch_id, port_id, payload)
    return pkt.tcpSrcPort if pkt.ipProto == 6 else 0

  def policy(self):
    conns = self.state.connections
    pol = (Union(self.route(src_port) for src_port in conns) |
            self.to_controller())
    return Filter(EthTypeEq(0x800)) >> pol

  def connected(self):
      self.update(self.policy())

  def route(self, src_tcp_port):
    dst_sw_port = self.state.connections[src_tcp_port]
    client_to_server = \
      Filter(PortEq(self.client_port) & TCPSrcPortEq(src_tcp_port)) >> SetPort(dst_sw_port)
    server_to_client = \
      Filter(PortEq(dst_sw_port) & TCPDstPortEq(src_tcp_port)) >> SetPort(self.client_port)
    return Filter(IPProtoEq(6)) >> (client_to_server | server_to_client)

  def to_controller(self):
    known_src_ports = self.state.connections.keys()
    return Filter(PortEq(self.client_port) & ~TCPSrcPortEq(known_src_ports)) >> SendToController("http")

  def packet_in(self, switch_id, port_id, payload):
    src = self.packet_src_port(switch_id, port_id, payload)
    server_port = self.state.new_connection(src)
    print "Sending traffic from TCP port %s to switch port %s" % (src, server_port)
    self.update(self.policy())

    # Assumes no address-translation
    self.pkt_out(switch_id = switch_id, payload = payload,
                 actions = [SetPort(server_port)])

if __name__ == '__main__':
  parser = argparse.ArgumentParser(description="A simple load balancer")
  parser.add_argument("--client-port", type=int, default="1")
  parser.add_argument("--server-ports", metavar="P", type=int, nargs="+",
                      help="Ports on which servers are running")
  args = parser.parse_args()
  app = LoadBalancer(args.client_port, State(args.server_ports))
  app.start_event_loop()