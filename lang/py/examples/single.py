from ryu.lib.packet import packet
import base64
import netkat
from netkat import webkat
from netkat.syntax import output, physical, modify, packet_out, buffered

webkat.client_id = "single"

def other_port(pt):
  assert (pt == 1 or pt == 2)
  if pt == 1:
    return 2
  else:
    return 1

class SingleApp(webkat.App):
  def packet_in(self, sw, pt, payload):
    print payload
    webkat.pkt_out(switch = sw,
                   payload = payload,
                   actions = [output(physical(other_port(pt)))])


print "Run mn --controller=remote --topo=single,2"
SingleApp().start()
webkat.update(modify("port","http"))
webkat.start()

