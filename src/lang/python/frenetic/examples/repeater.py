# A repeater that learns the switches and ports in a network then creates a
# policy that explicitly sends packets out of all ports, for each port.
import frenetic
from frenetic.syntax import *

class MyApp(frenetic.App):

    def __init__(self):
        frenetic.App.__init__(self)
        self.topo = {}

    def connected(self):
        # The controller may already be connected to several switches on startup.
        # This ensures that we probe them too.
        def handle_current_switches(switches):
            for switch_id in switches:
                self.switch_up(switch_id, switches[switch_id])
        self.current_switches(callback=handle_current_switches)        

    def policy(self):
        return Union(self.sw_policy(sw) for sw in self.topo.keys())

    def sw_policy(self, sw):
        ports = self.topo[sw]
        p = Union(self.port_policy(in_port, ports) for in_port in ports)
        return Filter( SwitchEq(sw) ) >> p

    def port_policy(self, in_port, ports):
        p = SetPort([port for port in ports if port != in_port])
        return Filter( PortEq(in_port) ) >> p

    def switch_up(self,switch_id,ports):
        self.topo[switch_id] = ports
        app.update(self.policy())

    def switch_down(self,switch_id):
        del self.topo[switch_id]
        app.update(self.policy())

    def port_up(self,switch_id, port_id):
        self.topo[switch_id].append(port_id)
        app.update(self.policy())

    def port_down(self,switch_id, port_id):
        self.topo[switch_id].remove(port_id)
        app.update(self.policy())

app = MyApp()
app.start_event_loop()
