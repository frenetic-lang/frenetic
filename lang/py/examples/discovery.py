import base64
from ryu.lib.packet import packet
from netkat import webkat
from netkat.syntax import *
import networkx as nx
from networkx.readwrite import json_graph

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

# NetKAT policy that diverts packets to the controllerc
def controller():
    return modify("port", "http")

###
# Policy
###
def policy():
    return (filter(test("ethTyp", 0x3366)) >> controller()) | (filter(test("ethTyp", 0x806)) >> controller())

##
# Emit Probes
##
def probe():
    for node in nib.nodes():
        for port in nib.node[node]['ports']:
            print "sending probe",node,port
    return

##
# Handle Events
##
def handler(event):
    print event
    typ = event['type']
    if typ  == 'switch_up':
        sw = event['switch_id']
        if nib.node.has_key(sw):
            pass
        else:
            nib.add_node(sw)
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
        pass
    else:
        pass

    print "NIB: %s" % json_graph.node_link_data(nib)
    return

def main():
    webkat.update(policy())
    webkat.event_loop(handler)
    webkat.periodic(probe,PROBE_INTERVAL)
    webkat.start()

if __name__ == '__main__':
    main()
