import netkat.flaskapp
from netkat.syntax import *

"""Repeater for a network with one switch and two hosts"""

def state():
    pass

topo = {}
def policy():
    return union(sw_policy(sw) for sw in topo.keys()) | modify("port", "python")

def sw_policy(sw):
    ports = topo[sw]
    return filter(test("switch", sw)) >> union(forward(port, ports) for port in ports)
   
def forward(in_port, ports):
    out_ports = [port for port in ports if port != in_port]
    return filter(test("port", in_port)) >> union(modify("port", port) for port in out_ports)

def handler(_, event):
    typ = event['type']
    if typ  == 'switch_up':
        sw = event['switch_id']
        topo[sw] = []
    elif typ == 'switch_down':
        sw = event['switch_id']
        del topo[sw]
    elif typ == 'port_up':
        pid = event['port_id']
        sw = event['switch_id']
        topo[sw].append(pid)
    elif typ == 'port_down':
        pid = event['port_id']
        sw = event['switch_id']
        topo[sw].remove(pid)
    else:
        pass
    print event
    print topo
    return policy()

if __name__ == '__main__':
    app = netkat.flaskapp.create(state, handler)
    app.debug = True
    app.run()
