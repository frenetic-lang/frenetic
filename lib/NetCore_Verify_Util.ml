
open NetKAT_Types
open VInt
open NetCore_Verify

let verify (description: string) (initial_state: policy) (program: policy) (final_state: policy) (desired_outcome: bool): bool = 
	check description initial_state program final_state (Some desired_outcome)

let make_vint v = VInt.Int64 (Int64.of_int v)

let make_transition switch1 port1 switch2 port2 : policy = 
  Seq (Seq (Test (Switch, make_vint switch1), Test (Header SDN_Types.InPort, make_vint port1)), 
	   (Seq (Mod (Switch ,make_vint switch2) , Mod (Header SDN_Types.InPort, make_vint port2))))

let make_simple_topology topo = Star (Seq (Id, topo))

let make_simple_policy pol = Star (Seq (pol, Id))

let make_one_hop pol topo = Star (Seq (pol, topo))

(*will take a policy, a topology, and add it to the kleene-star *)
let compose_topo p t p_t_star = Id

let make_packet switch port = 
  Seq (Test (Switch, make_vint switch), Test (Header SDN_Types.InPort, make_vint port))

