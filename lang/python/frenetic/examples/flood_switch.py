from frenetic.syntax import *

# Helper class to store switch information
class SwitchRef(object):
  def __init__(self, id, ports):
    self.id = id
    self.ports = ports

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
