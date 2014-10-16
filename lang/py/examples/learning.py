
from ryu.lib.packet import packet
import base64
import netkat.flaskapp
from netkat.syntax import *
from netkat.result import *

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
    return modify("port", "python")

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

##
# Main handler
##
def handler(_, event):
    print event
    typ = event['type']
    if typ  == 'switch_up':
        sw = event['switch_id']
        topo[sw] = []
        table[sw] = {}
    elif typ == 'switch_down':
        sw = event['switch_id']
        del topo[sw]
        del table[sw]
    elif typ == 'port_up':
        sw = event['switch_id']
        pid = event['port_id']
        topo[sw].append(pid)
    elif typ == 'port_down':
        sw = event['switch_id']
        pid = event['port_id']
        topo[sw].remove(pid)
    elif typ == 'packet_in':
        sw = event['switch_id']
        pt = event['port_id']
        bits = base64.b64decode(event['payload']['buffer'])
        pkt = packet.Packet(bits)        
        learn(sw, pkt, pt)
    else:
        pass
    return PolicyResult(policy())

if __name__ == '__main__':
    app = netkat.flaskapp.create(state, handler)
    app.debug = True
    app.run()
