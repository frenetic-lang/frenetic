import networkx, struct

class State(object):

  def __init__(self):
    self.network = networkx.DiGraph()
    self._switches = {}
    self.probes = set()
    self._hosts = set()
    self._edges = set()
    self.probes_sent = {}
    self.tentative_edge = {}
    self._observers = set()
    self._clean = True

  def switches(self):
    return self._switches

  def hosts(self):
    return self._hosts

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
    if switch_ref in self._switches:
      return
    self._switches[switch_ref.id] = switch_ref
    self.network.add_node(switch_ref.id)
    self._clean = False
    self.__cleanup(force_notify)

  def add_host(self, host, force_notify=False):
    if host in self._hosts:
      return
    self._hosts.add(host)
    self.network.add_node(host)
    self._clean = False
    self.__cleanup(force_notify)

  def add_edge(self, u, v, force_notify=False, **attr):
    if (u,v) in self._edges:
      return
    self._edges.add((u,v))
    self.network.add_edge(u,v,attr_dict=attr)
    self._clean = False
    self.__cleanup(force_notify)

  def remove_switch(self, switch_id, force_notify=False):
    if switch_id not in self._switches:
      return
    del self._switches[switch_id]
    self.network.remove_node(switch_id)
    self._clean = False
    self.__cleanup(force_notify)

  def remove_host(self, host, force_notify=False):
    if host not in self._hosts:
      return
    self._hosts.remove(host)
    self.network.remove_node(host)
    self._clean = False
    self.__cleanup(force_notify)

  def remove_edge(self, u, v, force_notify=False):
    if (u,v) not in self._edges:
      return
    self._edges.remove((u,v))
    self.network.remove_edge(u, v)
    self._clean = False
    self.__cleanup(force_notify)
    
