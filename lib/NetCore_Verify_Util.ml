
open NetKAT_Types
open VInt
open NetCore_Verify

let verify (description: string) (initial_state: policy) (program: policy) (final_state: policy) (desired_outcome: bool): bool = 
	check description initial_state program final_state (Some desired_outcome)

let make_vint v = VInt.Int64 (Int64.of_int v)

let make_transition (switch1, port1) (switch2, port2) : policy = 
  Seq (Seq (Filter (Test (Switch, make_vint switch1)), Filter (Test (Header SDN_Types.InPort, make_vint port1))), 
	   (Seq (Mod (Switch ,make_vint switch2) , Mod (Header SDN_Types.InPort, make_vint port2))))

let make_simple_topology topo : policy= Star (Seq (Filter Id, topo))

let rec combine_topologies (topo : policy list) : policy = 
  match topo with
	| (hd : policy)::[] -> hd
	| (hd : policy)::tl -> Par (hd, combine_topologies tl)
	| [] -> Filter Id

let make_simple_policy pol: policy  = Star (Seq (pol, Filter Id))

let starify pred (topo : policy) : policy = Star (Seq (Filter pred, topo))

(*will take a policy, a topology, and add it to the kleene-star *)
let compose_topo p t p_t_star : policy = Filter Id

let rec make_packet (headers_values : (header * 'a) list ) : policy= 
  match headers_values with
	| (hdr, valu)::[] -> Filter (Test (hdr, make_vint valu))
	| (hdr, valu)::tl -> Seq (Filter (Test (hdr, make_vint valu)), 
							  make_packet tl)
	| [] -> Filter Id

let make_packet_1 switch : policy= 
  Filter (Test (Switch, make_vint switch))

let make_packet_2 switch port : policy = 
  Filter (And (Test (Switch, make_vint switch), Test (Header SDN_Types.InPort, make_vint port)))

let make_packet_3 switch port ethsrc : policy = 
  Seq ((make_packet_2 switch port), Filter (Test (Header SDN_Types.EthSrc, make_vint ethsrc)))

let make_packet_4 switch port ethsrc ethdst : policy = 
  Seq ((make_packet_3 switch port ethsrc), Filter (Test (Header SDN_Types.EthDst, make_vint ethdst)))

