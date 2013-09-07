
open NetKAT_Types
open VInt
open NetCore_Verify

let verify (description: string) (initial_state: policy) (program: policy) (final_state: policy) (desired_outcome: bool): bool = 
	check description initial_state program final_state (Some desired_outcome)

let make_vint v = VInt.Int64 (Int64.of_int v)

let make_transition (switch1, port1) (switch2, port2) : policy = 
  Seq (Seq (Test (Switch, make_vint switch1), Test (Header SDN_Types.InPort, make_vint port1)), 
	   (Seq (Mod (Switch ,make_vint switch2) , Mod (Header SDN_Types.InPort, make_vint port2))))

let make_simple_topology topo = Star (Seq (Id, topo))

let rec combine_topologies (topo : policy list) : policy = 
  match topo with
	| (hd : policy)::[] -> hd
	| (hd : policy)::tl -> Par (hd, combine_topologies tl)
	| [] -> Id

let make_simple_policy pol = Star (Seq (pol, Id))

let starify pol topo = Star (Seq (pol, topo))

(*will take a policy, a topology, and add it to the kleene-star *)
let compose_topo p t p_t_star = Id

let rec make_packet (headers_values : (header * 'a) list )= 
  match headers_values with
	| (hdr, valu)::[] -> Test (hdr, make_vint valu)
	| (hdr, valu)::tl -> Seq (Test (hdr, make_vint valu), make_packet tl)
	| [] -> Id

let make_packet_2 switch port = 
  Seq (Test (Switch, make_vint switch), Test (Header SDN_Types.InPort, make_vint port))

let make_packet_3 switch port ethsrc = 
  Seq ((make_packet_2 switch port), Test (Header SDN_Types.EthSrc, make_vint ethsrc))

let make_packet_4 switch port ethsrc ethdst = 
  Seq ((make_packet_3 switch port ethsrc), Test (Header SDN_Types.EthDst, make_vint ethdst))

