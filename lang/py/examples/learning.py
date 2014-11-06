from ryu.lib.packet import packet
import base64
from netkat import webkat
from netkat.syntax import *

"""Ethernet Learning switch"""

def state():
    pass

table = {}
topo = {}

##
# Helper functions
##
def get_ethernet(pkt):
    for p in pkt:
        if p.protocol_name == 'ethernet':
            return p

def output(pt):
    return modify("port", pt)

def controller():
    return modify("port", "http")

def flood(sw):
    ports = topo[sw]

    def flood_port(pt):
        outs = [_pt for _pt in ports if _pt != pt]
        return filter(test("port", pt)) >> union(output(pt) for pt in outs)
    
    return union(flood_port(port) for port in ports)


##
# Learning switch functions
##

def learn(sw,pkt,pt):
    table[sw][get_ethernet(pkt).src] = pt

def switch_policy(sw):
    def f((known,unknown),mac):
        src = test("ethSrc", mac)
        dst = test("ethDst", mac)
        return (known | filter(dst) >> output(table[sw][mac]), unknown & ~src)
        
    (known_pol, unknown_pred) = reduce(f, table[sw].keys(), (drop(), true()))
    return known_pol | filter(unknown_pred) >> (controller() | flood(sw))

def policy():
    return union(switch_policy(sw) for sw in topo.keys())

class LearningApp(webkat.App):
    def switch_up(self,switch_id):
        topo[switch_id] = []
        table[switch_id] = {}
        webkat.update(policy())
    def switch_down(self,switch_id):
        del topo[switch_id]
        del table[switch_id]
        webkat.update(policy())
    def port_up(self,switch_id, port_id):
        topo[switch_id].append(port_id)
        webkat.update(policy())
    def port_down(self,switch_id, port_id):
        topo[switch_id].remove(port_id)
        webkat.update(policy())
    def packet_in(self,switch_id, port_id, packet):
        learn(switch_id,packet,port_id)
        webkat.update(policy())

if __name__ == '__main__':
    LearningApp().start()
    webkat.start()
