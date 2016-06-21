import frenetic
from frenetic.syntax import * 

class MyApp(frenetic.App):
  client_id = "my_app" 

  def connected(self):
    self.update( id >> SendToController("my_app") )  

  def switch_up(self,switch_id,ports):
    print "switch_up(switch_id=%s)" % switch_id

  def switch_down(self,switch_id):
    print "switch_down(switch_id=%s)" % switch_id

  def port_up(self,switch_id, port_id):
    print "port_up(switch_id=%s, port_id=%d)" % (switch_id, port_id)

  def port_down(self,switch_id, port_id):
    print "port_down(switch_id=%s, port_id=%d)" % (switch_id, port_id)

  def packet_in(self, switch_id, port_id, payload):
    print "packet_in(switch_id=%s, port_id=%d)" % (switch_id, port_id)

app = MyApp()
app.start_event_loop()