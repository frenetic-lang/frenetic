import networkx, struct
from networkx.readwrite import json_graph

class State(object):

  def __init__(self):
    self.network = networkx.DiGraph()
    self.mode = "internal_discovery"
    self._observers = set()
    self._clean = True

  def to_json(self):
    return json_graph.adjacency_data(self.network)

  @classmethod
  def from_json(cls, json):
    state = cls()
    state.network = json_graph.adjacency_graph(json, directed=True)
    return state

  def network_edge(self):
    edge = set()
    sws = self.switches()
    for sw in sws:
      internal_ports = set(edge[2]["label"]
                           for edge in self.network.out_edges(sw, data=True)
                           if "switch_ref" in self.network.node[edge[1]])
      all_ports = set(sws[sw][1])
      external_ports = all_ports - internal_ports
      for port in external_ports:
        edge.add((sw, port))

    return edge

  def set_mode(self, mode):
    self.mode = mode

  def switches(self):
    return dict([ (x, self.network.node[x]["switch_ref"])
                  for x in self.network.nodes()
                  if "switch_ref" in self.network.node[x] ])

  # NOTE(arjun): switches and nodes are a little hacky. I'm relying on hosts
  # being represented as strings (i.e., mac-addresses with colon-seperated
  # bytes).
  def hosts(self):
    return [node for node in self.network.nodes_iter() if type(node) == str]

  def register(self, observer):
    self._observers.add(observer)

  def notify(self):
    if not self._clean:
      for observer in self._observers:
        observer.run_update()
    self._clean = True

  def __cleanup(self, force_notify):
    self._clean = False
    if force_notify:
      self.notify()

  def add_switch(self, switch_id, switch_ports, force_notify=False):
    if switch_id in self.network.nodes_iter():
      return
    self.network.add_node(switch_id, switch_ref=(switch_id, switch_ports))
    self._clean = False
    self.__cleanup(force_notify)

  def add_host(self, host, force_notify=False):
    if host in self.network.nodes_iter():
      return
    self.network.add_node(host)
    self._clean = False
    self.__cleanup(force_notify)

  def add_edge(self, u, v, force_notify=False, **attr):
    if (u,v) in self.network.edges(u):
      return
    self.network.add_edge(u,v,attr_dict=attr)
    self._clean = False
    self.__cleanup(force_notify)

  def remove_switch(self, switch_id, force_notify=False):
    if switch_id not in self.network.nodes_iter():
      return
    self.network.remove_node(switch_id)
    self._clean = False
    self.__cleanup(force_notify)

  def remove_host(self, host, force_notify=False):
    if host not in self.network.nodes_iter():
      return
    self.network.remove_node(host)
    self._clean = False
    self.__cleanup(force_notify)

  def remove_edge(self, u, v, force_notify=False):
    if (u,v) not in self.network.edges(u):
      return
    self.network.remove_edge(u, v)
    self._clean = False
    self.__cleanup(force_notify)


