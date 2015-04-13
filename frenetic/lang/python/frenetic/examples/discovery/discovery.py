import sys
from state import State
from topology import Topology
from routing import Routing

def main(version):
  shared_state = State()
  topo = Topology(shared_state, version)
  app = Routing(shared_state, version)
  app.start_event_loop()

if __name__ == '__main__':
  if len(sys.argv) == 2:
    main(int(sys.argv[1]))
  else:
    print "Running version 1"
    main(1)
