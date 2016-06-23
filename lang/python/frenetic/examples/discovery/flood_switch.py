from frenetic.syntax import *

# Helper class to store switch information
class SwitchRef(object):
  def __init__(self, id, ports):
    self.id = id
    self.ports = ports

  def __eq__(self, other):
    return (isinstance(other, self.__class__) and
            self.id == other.id)

  def to_json(self):
    return { "id": self.id, "ports" : self.ports }

  def __hash__(self):
    return self.id.__hash__()

# Create a policy that given a SwitchRef, floods all input to its ports
def flood_switch_policy(switch):
  assert isinstance(switch, SwitchRef)
  all_policies = []
  for src in switch.ports:
    action = Filter(PortEq(src)) >> SetPort([dst for dst in switch.ports if src != dst])
    all_policies.append(action)

  if not all_policies:
    return Filter(SwitchEq(switch.id)) >> drop
  return Filter(SwitchEq(switch.id)) >> Union(all_policies)
