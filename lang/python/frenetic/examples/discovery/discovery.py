from state import State
from topology import Topology
from routing import Routing

def main(version):
  shared_state = State()
  topo = Topology(shared_state)
  app = Routing(shared_state)
  app.start_event_loop()

if __name__ == '__main__':
  main(1)
