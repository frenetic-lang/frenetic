# A repeater that learns the switches and ports in a network then creates a
# policy that explicitly sends packets out of all ports, for each port.

from frenetic import app
from frenetic.syntax import *

topo = {}

def policy():
    return Union(sw_policy(sw) for sw in topo.keys())

def sw_policy(sw):
    ports = topo[sw]
    p = Union(port_policy(in_port, ports) for in_port in ports)
    return Filter(Test(Switch(sw))) >> p

def port_policy(in_port, ports):
    out_ports = [port for port in ports if port != in_port]
    p = Union(Mod(Location(Physical(p))) for p in out_ports)
    return Filter(Test(Location(Physical(in_port)))) >> p


class MyApp(app.App):

    def switch_up(self,switch_id):
        topo[switch_id] = []
        app.update(policy())

    def switch_down(self,switch_id):
        del topo[switch_id]
        app.update(policy())

    def port_up(self,switch_id, port_id):
        topo[switch_id].append(port_id)
        app.update(policy())

    def port_down(self,switch_id, port_id):
        topo[switch_id].remove(port_id)
        app.update(policy())

MyApp().start()
app.start()

