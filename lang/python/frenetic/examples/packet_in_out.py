# A simple program that sends all packets to the controller. The controller
# sends packets out on all ports.
from frenetic import app
from frenetic.syntax import *

def other_port(pt):
  assert (pt == 1 or pt == 2)
  if pt == 1:
    return 2
  else:
    return 1

class SingleApp(app.App):
  def packet_in(self, sw, pt, payload):
    print payload
    app.pkt_out(switch = sw,
                   payload = payload,
                   actions = [output(physical(other_port(pt)))])


print "Run mn --controller=remote --topo=single,2"
pol = modify("port","http")
SingleApp().start()
print repr(pol)
app.update(pol)
app.start()

