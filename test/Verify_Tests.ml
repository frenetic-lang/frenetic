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

let make_packet switch port = 
  Seq (Test (Switch, make_vint switch), Test (Header SDN_Types.InPort, make_vint port))

TEST "simple-check" = 
  verify "are tests even running" 
	(make_packet 1 1)
	(make_simple_topology (make_transition 1 1 2 1))
	(make_packet 2 1)
	true
	

