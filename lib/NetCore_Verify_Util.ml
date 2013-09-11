
open NetKAT_Types
open VInt
open NetCore_Verify

let verify (description: string) (initial_state: pred) (program: policy) (final_state: pred) (desired_outcome: bool) k : bool = 
	check_specific_k description initial_state program final_state (Some desired_outcome) k

let make_vint v = VInt.Int64 (Int64.of_int v)

let make_transition (switch1, port1) (switch2, port2) : policy = 
  Seq (Seq (Filter (Test (Switch, make_vint switch1)), Filter (Test (Header SDN_Types.InPort, make_vint port1))), 
	   (Seq (Mod (Switch ,make_vint switch2) , Mod (Header SDN_Types.InPort, make_vint port2))))

let make_simple_topology topo : policy = Star (Seq (Filter True, topo))

let rec combine_topologies (topo : policy list) : policy = 
  match topo with
	| (hd : policy)::[] -> hd
	| (hd : policy)::tl -> Par (hd, combine_topologies tl)
	| [] -> Filter True

let make_simple_policy pol: policy  = Star (Seq (pol, Filter True))

let starify pred (topo : policy) : policy = Star (Seq (Filter pred, topo))

(*will take a policy, a topology, and add it to the kleene-star *)
let compose_topo p t p_t_star : policy = Filter True

let rec make_packet (headers_values : (header * 'a) list ) = 
  match headers_values with
	| (hdr, valu)::[] -> Test (hdr, make_vint valu)
	| (hdr, valu)::tl -> And (Test (hdr, make_vint valu), 
							  make_packet tl)
	| [] -> True

let make_packet_1 switch = 
  Test (Switch, make_vint switch)

let make_packet_2 switch port  = 
  And (Test (Switch, make_vint switch), Test (Header SDN_Types.InPort, make_vint port))

let make_packet_3 switch port ethsrc  = 
  And ((make_packet_2 switch port), Test (Header SDN_Types.EthSrc, make_vint ethsrc))

let make_packet_4 switch port ethsrc ethdst  = 
  And ((make_packet_3 switch port ethsrc), Test (Header SDN_Types.EthDst, make_vint ethdst))

