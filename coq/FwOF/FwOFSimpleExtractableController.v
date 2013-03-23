Require Import FwOF.FwOFSignatures.
Require FwOF.FwOFSimpleController.
Require Import FwOF.FwOFNetworkAtoms.

Module Type POLICY_AND_TOPOLOGY.

  Import NetworkAtoms.

  Parameter topo : switchId * portId -> option (switchId * portId).
  Parameter abst_func : switchId -> portId -> packet -> list (portId * packet).

End POLICY_AND_TOPOLOGY.

Module Make (PolTopo : POLICY_AND_TOPOLOGY).
  
  Module NetworkAndPolicy <: NETWORK_AND_POLICY.
    Include NetworkAtoms.
    Include PolTopo.
  End NetworkAndPolicy.

  Module M := FwOF.FwOFSimpleController.Make (NetworkAndPolicy).

  Include M.

End Make.