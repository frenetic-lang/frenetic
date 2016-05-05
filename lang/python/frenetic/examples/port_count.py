import frenetic, sys
from functools import partial
from flood_switch import *
from tornado.ioloop import PeriodicCallback, IOLoop

class State(object):

  def __init__(self):
    self.switches = set()

class PortCount(frenetic.App):

  client_id = "port_count"

  def __init__(self, state):
    frenetic.App.__init__(self)
    self.state = state
    self.update(self.global_policy())

  def connected(self):
    # The controller may already be connected to several switches on startup.
    # This ensures that we probe them too.
    def handle_current_switches(switches):
      for switch_id in switches:
        self.switch_up(switch_id, switches[switch_id])
    self.current_switches(callback=handle_current_switches)        
    PeriodicCallback(self.count_ports, 5000).start()

  def print_count(self, future, switch):
    data = future.result()
    for d in data:
      print "Count %s@%s: {rx_bytes = %s, tx_bytes = %s}" % (switch, d['port_no'], d['rx_bytes'], d['tx_bytes'])

  def count_ports(self):
    for switch in self.state.switches:
      for port in switch.ports:
        ftr = self.port_stats(str(switch.id), str(port))
        f = partial(self.print_count, switch = switch.id)
        IOLoop.instance().add_future(ftr, f)

  def switch_up(self, switch_id, ports):
    print "switch_up(%s, %s)" % (switch_id, ports)
    self.state.switches.add(SwitchRef(switch_id, ports))
    self.update(self.global_policy())

  def switch_down(self, switch_id):
    print "switch_down(%s)" % switch_id
    self.state.switches.discard(SwitchRef(switch_id, ports))
    self.update(self.global_policy())

  def global_policy(self):
    return Union([flood_switch_policy(switch) for switch in self.state.switches])

def main(version):
    app = PortCount(State())
    app.start_event_loop()

if __name__ == '__main__':
    if len(sys.argv) == 2:
      main(int(sys.argv[1]))
    else:
      print "Running version 1"
      main(1)

