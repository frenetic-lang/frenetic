import networkx, struct

class State(object):

  def __init__(self):
    self.network = networkx.DiGraph()
    self.probes = set()
    self.probes_sent = {}
    self.tentative_edge = {}
    self.mode = "internal_discovery"
    self._observers = set()
    self._clean = True

  def network_edge(self):
    edge = set()
    sws = self.switches()
    for sw in sws:
      internal_ports = set(edge[2]["label"]
                           for edge in self.network.out_edges(sw, data=True)
                           if "switch_ref" in self.network.node[edge[1]])
      all_ports = set(sws[sw].ports)
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

  def add_switch(self, switch_ref, force_notify=False):
    if switch_ref in self.network.nodes_iter():
      return
    self.network.add_node(switch_ref.id, switch_ref=switch_ref)
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

  def probes_discard(self, probe):
    self.probes.discard(probe)
 
  def probes_add(self, probe):
    self.probes.add(probe)
  
  def probes_sent_increment(self, probe_data):
    self.probes_sent[probe_data] = self.probes_sent[probe_data] + 1
 
  def probes_sent_init(self, probe_data):
    self.probes_sent[probe_data] = 0 
 
  def del_probes_sent(self, probe_data):
    if probe_data in self.probes_sent:
      del self.probes_sent[probe_data]
 
  def set_tentative_edge(self, probe_data, mac):
    self.tentative_edge[probe_data] = mac
 
  def del_tentative_edge(self, probe_data):
    if probe_data in self.tentative_edge:
      del self.tentative_edge[probe_data]

