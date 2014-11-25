import base64
from ryu.lib.packet import packet,ethernet,arp
from netkat import webkat
from netkat.syntax import *
import networkx as nx
from networkx.readwrite import json_graph
from ryu.ofproto.ether import ETH_TYPE_ARP

"""Topology Discovery"""

###
# Globals
#
# PROBE_INTERVAL: frequency to send LLDP-like probes in seconds
#
# nib: dictionary with switches as keys and list of ports as values
#      (jnf): this will have to change obviously when you're reprsenting
#             the actual graph, and more.
#             consider using networkx?
##

PROBE_INTERVAL = 5

# (nb): Arbitrarily assigned ethtype to (switch) discovery packets
# I will replace this and instead use LLDP
ETH_TYPE_DISCOVERY_PACKET = 0x3366

# (nb): Could we make this a Graph or do we explicitly need directed edges?
nib = nx.DiGraph()

##
# Helper functions
##

# Extract Ethernet frame from
# Ryu packet
def get_ethernet(pkt):
    for p in pkt:
        if p.protocol_name == 'ethernet':
            return p

# NetKAT policy that diverts packets to the controller
def controller():
    return modify("port", "http")

###
# Policy
###
def policy():
    return (filter(test("ethTyp", ETH_TYPE_DISCOVERY_PACKET)) >> controller()) | (filter(test("ethTyp", ETH_TYPE_ARP)) >> controller())

##
# Emit Probes
##
def probe():
    print '=== PROBE ==='
    for node in nib.nodes():
        if nib.node[node]['device'] == 'switch':
            for port in nib.node[node]['ports']:
                # (nb): NOTE: Force node,port into dst,src fields of Ethernet packet. Temporary only!
                dp = ethernet.ethernet(dst=node,src=port,ethertype=0x3366)
                p = packet.Packet()
                p.add_protocol(dp)
                p.serialize()
                webkat.pkt_out(node,port,p.data)
    print nib.edges(data=True)
    return

##
# Handle Events
##
def handler(event):
    #print event
    typ = event['type']
    if typ  == 'switch_up':
        sw = event['switch_id']
        if nib.node.has_key(sw):
            pass
        else:
            nib.add_node(sw,device='switch')
            nib.node[sw]['ports'] = set()
    elif typ == 'port_up':
        nib.node[event['switch_id']]['ports'].add(event['port_id'])
    elif typ == 'switch_down':
        sw = event['switch_id']
        nib.remove_node(sw)
    elif typ == 'packet_in':
        sw = event['switch_id']
        pt = event['port_id']
        bits = base64.b64decode(event['payload']['buffer'])
        pkt = packet.Packet(bits)
        p = get_ethernet(pkt)
        if p.ethertype == ETH_TYPE_ARP:
            # (nb): For now, assign a unique hostname concat of parent-switch:port
            host_id = "h%s" % (str(sw) + ":" + str(pt))
            nib.add_node(host_id,device='host')
            nib.add_edge(sw,host_id,outport=pt,inport=0)
            nib.add_edge(host_id,sw,outport=0,inport=pt)
        elif p.ethertype == ETH_TYPE_DISCOVERY_PACKET:
            # (nb): Remove ryu.packet's MAC string format
            src_sw = int((p.dst).replace(':', ''), 16)
            src_port = int((p.src).replace(':',''), 16)
            nib.add_edge(src_sw,sw,outport=src_port,inport=pt)
            nib.add_edge(sw,src_sw,outport=pt,inport=src_port)
        pass
    else:
        pass

    #print "NIB: %s" % json_graph.node_link_data(nib)
    return

def main():
    webkat.client_id = "discovery"
    webkat.update(policy())
    webkat.event_loop(handler)
    webkat.periodic(probe,PROBE_INTERVAL)
    webkat.start()

if __name__ == '__main__':
    main()
