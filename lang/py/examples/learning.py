
from ryu.lib.packet import packet
import base64
import netkat.flaskapp
from netkat.syntax import *

"""Ethernet Learning switch"""

def state():
    pass

table = {}
topo = {}

def get_proto(pkt, proto):
    for p in pkt:
        if p.protocol_name == proto:
            return p

def flood(sw):
    ports = topo[sw]

    def flood_port(pt):
        out_ports = [port for port in ports if port != pt]
        return filter(test("port", pt)) >> reduce(lambda pol, out: pol | modify("port", out), out_ports, drop())
    
    pol = reduce(lambda pol, pt: pol | flood_port(pt), ports, drop())
    return pol

def learn(sw,pkt,pt):
    src = get_proto(pkt,"ethernet").src
    table[sw][src] = pt

def policy():
    def switch_policy(sw):
        known_pol = reduce(lambda pol, mac: pol | (filter(test("ethDst", mac)) >> modify("port", table[sw][mac])), table[sw].keys(), drop())
        known_pred = reduce(lambda pred, mac: (pred | test("ethSrc", mac)), table[sw].keys(), false())
        pol = known_pol | (filter(~known_pred) >> (modify("port", "python") | flood(sw)))
        return pol

    return reduce(lambda pol, sw: pol | switch_policy(sw), topo.keys(), drop())

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
    pol = policy ()
    return pol

if __name__ == '__main__':
    app = netkat.flaskapp.create(state, handler)
    app.debug = True
    app.run()
