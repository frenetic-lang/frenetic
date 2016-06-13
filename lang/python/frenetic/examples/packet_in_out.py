# A simple program that sends all packets to the controller. The controller
# sends packets out on all ports.
import frenetic
from frenetic.syntax import *

def other_port(pt):
  assert (pt == 1 or pt == 2)
  return 2 if pt == 1 else 1

class MyApp(frenetic.App):

  def packet_in(self, sw, pt, payload):
    self.pkt_out(switch_id = sw,
                 payload = payload,
                 actions = [SetPort(other_port(pt))])


print "Run mn --controller=remote --topo=single,2"
app = MyApp()
app.update(SendToController("http"))
app.start_event_loop()