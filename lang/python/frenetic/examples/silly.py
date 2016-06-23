from ryu.lib.packet import packet
import base64
import frenetic
from frenetic.syntax import *

"""Silly repeater - sends packet out the port it came in on"""
class SillyRepeaterApp(frenetic.App):

    def __init__(self):
        frenetic.App.__init__(self)

    def connected(self):
        # The controller may already be connected to several switches on startup.
        # This ensures that we probe them too.
        def handle_current_switches(switches):
            for switch_id in switches:
                self.update( SendToController("http") )
        self.current_switches(callback=handle_current_switches)        

    def packet_in(self, switch_id, port_id, payload):
        self.pkt_out(switch_id = switch_id, payload = payload, actions = [SetPort(port_id)])

if __name__ == '__main__':
    app = SillyRepeaterApp()
    app.start_event_loop()
