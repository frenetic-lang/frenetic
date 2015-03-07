import frenetic, sys, json, time
from frenetic.syntax import *
import array
from ryu.lib.packet import packet
from tornado.ioloop import PeriodicCallback
from tornado.concurrent import chain_future

def get(pkt,protocol):
    for p in pkt:
        if p.protocol_name == protocol:
            return p

class LoadBalancer(frenetic.App):

    client_id = "loadbalancer"

    def __init__(self,internal_port,external_ports):
        frenetic.App.__init__(self)
        self.internal_port = internal_port
        self.external_ports = external_ports
        self.state = []
        self.update(self.global_policy())
        
    def flow_pred(self,flow,external):
        (ip_src,ip_dst,ip_proto,tcp_src_port,tcp_dst_port) = flow
        return (Test(Location(Physical(external))) &
                Test(EthType(0x800)) &
                Test(IP4Src(ip_src)) &
                Test(IP4Dst(ip_dst)) &
                Test(IPProto(ip_proto)) &
                Test(TCPSrcPort(tcp_src_port)) &
                Test(TCPDstPort(tcp_dst_port)))
        
    def global_policy(self):
        print "--- State ---"
        for (flow,external,internal) in self.state:
            print "%s : %d => %s" % (flow,external,internal)
        ext_int = Union([Filter(Test(Location(Physical(pe)))) >> \
                        Mod(Location(Physical(self.internal_port))) \
                        for pe in self.external_ports])
        int_ext = Union([Filter(self.flow_pred(flow,external)) >> \
                        Mod(Location(Physical(internal))) \
                        for (flow,external,internal) in self.state])
        cnt = Filter(Test(Location(Physical(self.internal_port))) & \
                          Not(Or([self.flow_pred(flow,external) for (flow,external,_) in self.state]))) >> \
              Mod(Location(Pipe("http")))
        policy = Union([ext_int,int_ext,cnt])
        return policy
        
    def packet_in(self, switch_id, port_id, payload):
        pkt = packet.Packet(array.array('b', payload.data))
        ip = get(pkt, "ipv4")
        tcp = get(pkt, "tcp")
        print "Packet in: %s" % pkt
        if ip == None or tcp == None:
            print "Not TCP/IP packet. Dropped."
            self.pkt_out(switch = switch_id, payload = payload, actions = [])
            return
        if port_id == self.internal_port:
            flow = (ip.dst, ip.src, ip.proto, tcp.dst_port, tcp.src_port) #NB: reversed!
            external = self.external_ports[hash(flow) % len(self.external_ports) - 1]
            if not (flow in self.state):
                self.state.append((flow,external,port_id))
                self.update(self.global_policy())
            self.pkt_out(switch_id, payload, [Output(Physical(external))])

def main():
        app = LoadBalancer(1,[2,3])
        app.start_event_loop()

if __name__ == '__main__':
    main()

