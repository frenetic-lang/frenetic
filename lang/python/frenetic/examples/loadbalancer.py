import frenetic, sys, json, time
from frenetic.syntax import *
import array
from ryu.lib.packet import packet
from tornado.ioloop import PeriodicCallback
from tornado.concurrent import chain_future

net_size = 2

def get(pkt,protocol):
    for p in pkt:
        if p.protocol_name == protocol:
            return p

class LoadBalancer(frenetic.App):

  client_id = "load_balancer"

  def __init__(self,internal_port,external_ports):
    frenetic.App.__init__(self)
    self.internal_port = internal_port
    self.external_ports = external_ports
    self.state = []
    self.update(self.global_policy())

  def global_policy(self):
      out_in = Union([Filter(Test(Location(Physical(pe)))) >> \
                      Mod(Location(Physical(self.internal_port))) \
                         for pe in self.external_ports])
      in_out = Union([Filter(flow_pred) >> \
                      Mod(Location(Physical(flow_external))) \
                        for (flow_pred,flow_external) in self.state])
      controller = Filter(Test(Location(Physical(self.internal_port))) & \
                          Not(Or([flow_pred for (flow_pred,flow_external) in self.state]))) >> \
                   Mod(Location(Pipe("http")))
      policy = Union([out_in,in_out,controller])
      return policy

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

    if port_id == self.internal_port:
        flow_pred = Test(Location(Physical(port_id))) & \
                    Test(EthType(0x800)) & \
                    Test(IP4Src(ip.src)) & \
                    Test(IP4Dst(ip.dst)) & \
                    Test(IPProto(ip.proto)) & \
                    Test(TCPSrcPort(tcp.src_port)) & \
                    Test(TCPDstPort(tcp.dst_port)) 
        flow_external = self.external_ports[hash(flow_pred) % len(self.external_ports) - 1]
        print "Flow Pred %s" % flow_pred.to_json()
        if (flow_pred,flow_external) not in self.state:
            self.state.append((flow_pred,flow_external))
            self.update(self.global_policy())
            self.pkt_out(switch_id, payload, [Output(Physical(flow_external))])
    else:
        self.pkt_out(switch_id, payload, [Output(Physical(self.internal_port))])

def main():
    app = LoadBalancer(1,[2,3])
    app.start_event_loop()

if __name__ == '__main__':
    main()

