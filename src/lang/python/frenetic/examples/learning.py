import array, base64
from ryu.lib.packet import packet, ethernet
import frenetic
from frenetic.syntax import *
from frenetic.packet import *

"""Ethernet Learning switch"""

def state():
    pass

# table = {switch1 => {mac1 => pt1, mac2 => pt2}, ..., switch2 => {mac1 => pt1, ...} }
table = {}
# topo = { switch1 => [pt1, pt2, ...], switch2 => [pt1, pt2, ...] }
topo = {}
all_ethernet = set()

# For each port p, copy a incoming packet from port p to all other ports.  This will
# effectively copy it to all switches as long as there are no loops.  
def flood(sw):
    ports = topo[sw]

    def flood_port(pt):
        return Filter(PortEq(pt)) >> SetPort([_pt for _pt in ports if _pt != pt])
    
    return Union(flood_port(port) for port in ports)

##
# Learning switch functions
##

def learn(app, switch_id,payload,pt):
    if switch_id not in topo:
        topo[switch_id] = []
        table[switch_id] = {}
    # If we've already seen this source, and it's connected to the same switch and port
    # then don't bother recalculating.  This can happen if the rule hasn't been installed
    # fast enough.
    # TODO: Handle case of Ethernet moved
    pkt = Packet.from_payload(switch_id, pt, payload)
    mac = pkt.ethSrc
    if mac in all_ethernet:
        return "no_updates"
    print "Saw Ethernet For first time ", mac, " at switch ", switch_id, " port ", pt
    table[switch_id][mac] = pt
    all_ethernet.add(mac)
    return "updates_needed"

def switch_policy(sw):
    def f((known,unknown),mac):
        src = EthSrcEq(mac)
        dst = EthDstEq(mac)
        return (known | Filter(dst) >> SetPort(table[sw][mac]), unknown & ~src)
        
    (known_pol, unknown_pred) = reduce(f, table[sw].keys(), (drop, true))
    # print "Known pol: ", known_pol.to_json()
    # print "Unknown pred: ", unknown_pred.to_json()
    # print "Controller: ", controller().to_json()
    # print "Flood(sw): ", flood(sw).to_json()
    return known_pol | Filter(unknown_pred) >> (SendToController("learning_controller") | flood(sw))

def policy():
    #for sw in topo.keys():
    #    print "switch_policy: ", switch_policy(sw).to_json() 
    return Union(switch_policy(sw) for sw in topo.keys())

class LearningApp(frenetic.App):
    client_id = "learning"
    frenetic_http_host = "localhost"

    def __init__(self):
        frenetic.App.__init__(self)

    def connected(self):
        # The controller may already be connected to several switches on startup.
        # This ensures that we probe them too.
        def handle_current_switches(switches):
            for switch_id in switches:
                self.switch_up(switch_id, switches[switch_id])
        self.current_switches(callback=handle_current_switches)        

    def switch_up(self,switch_id,ports):
        print "switch_up(switch_id=%s)" % switch_id
        topo[switch_id] = ports
        table[switch_id] = {}
        self.update(policy())

    def switch_down(self,switch_id):
        print "switch_down(switch_id=%s)" % switch_id
        try:
            del topo[switch_id]
            del table[switch_id]
        except KeyError:
            pass
        self.update(policy())

    def port_up(self,switch_id, port_id):
        print "port_up(switch_id=%s, port_id=%d)" % (switch_id, port_id)
        if switch_id not in topo:
            topo[switch_id] = []
            table[switch_id] = {} 
        # TODO: If port is already up, don't bother updating
        topo[switch_id].append(port_id)
        self.update(policy())

    def port_down(self,switch_id, port_id):
        print "port_down(switch_id=%s, port_id=%d)" % (switch_id, port_id)
        try:
            topo[switch_id].remove(port_id)
        except KeyError:
            pass
        # TODO: If port was already down, don't bother updating
        self.update(policy())

    def packet_in(self,switch_id, port_id, payload):
        if learn(self, switch_id,payload,port_id) == "updates_needed":
            self.update(policy())

if __name__ == '__main__':
    app = LearningApp()
    app.start_event_loop()
