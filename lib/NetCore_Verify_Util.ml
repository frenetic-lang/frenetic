
open SDN_Types
open SDN_Headers
open NetKAT_Types
open VInt
open NetCore_Verify
open Sat

let verify (description: string) (initial_state: pred) (program: policy) (final_state: pred) (desired_outcome: bool) : bool = 
	check description initial_state program final_state (Some desired_outcome)

let verify_k k (description: string) (initial_state: pred) (program: policy) (final_state: pred) (desired_outcome: bool) : bool = 
	check_reachability_k k description initial_state program final_state (Some desired_outcome)

(* let verify_history (description: string) (initial_state: pred) (program: policy) expr (final_state: pred) (desired_outcome: bool) : bool = 
	check_with_history expr description initial_state program final_state (Some desired_outcome)

let verify_specific_k (description: string) (initial_state: pred) (program: policy) (final_state: pred) (desired_outcome: bool) k : bool = 
	check description initial_state program final_state (Some desired_outcome) *)


let make_transition (switch1, port1) (switch2, port2) : policy = 

  Seq (Filter (And (Test (Switch, make_vint switch1), Test (Header SDN_Types.InPort, make_vint port1))), 
       Seq (Mod (Switch ,make_vint switch2) , Mod (Header SDN_Types.InPort, make_vint port2)))

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

let dijkstra_test topo = Verify_Graph.longest_shortest (Verify_Graph.parse_graph (make_simple_topology topo))

let bool_to_z3 b = if b then Sat.ZTrue else Sat.ZFalse

let map_fun pred pkt = 
  Verify.forwards_pred pred pkt

let fold_pred_and pred: (Sat.zVar list -> Sat.zFormula) = 
  let ret l = 
	Sat.ZAnd (List.map (map_fun pred) l)
  in ret

let fold_pred_or pred: (Sat.zVar list -> Sat.zFormula) = 
  let ret l = 
	Sat.ZOr (List.map (map_fun pred) l)
  in ret

let matches_history preds : (Sat.zVar list -> Sat.zFormula) = 
  let ret l =  
	let rec recr pktl predl =
	  (match pktl with 
		| [] -> (match predl with 
			| [] -> bool_to_z3 true
			| _ -> bool_to_z3 false)
		| hd :: tl -> Sat.ZAnd [(Verify.forwards_pred (List.hd predl) hd); recr tl (List.tl predl) ])
	in
	recr l preds
  in
  ret

let fold_pred_or_with_counter (expr : int -> pred) : (Sat.zVar list -> Sat.zFormula) = 
  let ret l = 
	let rec recr pktl f count = 
	  match pktl with
		| hd :: [] -> Verify.forwards_pred (f count) hd
		| hd :: tl -> Sat.ZOr [Verify.forwards_pred (f count) hd; recr tl f (count + 1)]
		| [] -> Sat.ZTrue
	in
	recr l expr 0
  in
  ret

let junct_juncts_counter outer inner (exprs : (int -> pred) list ) : (Sat.zVar list -> Sat.zFormula) = 
  let ret (l : 'a list) = 
	let rec recr (count : int) (pktl : 'a list) (f : int -> pred) = 
	  match pktl with
		| hd :: [] -> Verify.forwards_pred (f count) hd
		| hd :: tl -> inner [Verify.forwards_pred (f count) hd; recr (count + 1) tl f ]
		| [] -> Sat.ZTrue
	in
	outer (List.map (recr 0 l) exprs)
  in
  ret

let conjunct_disjuncts_counter = junct_juncts_counter (fun n -> Sat.ZAnd n) (fun n -> Sat.ZOr n)

let fold_pred_and_with_counter (expr : int -> pred) : (Sat.zVar list -> Sat.zFormula) = 
  let ret l = 
	let rec recr pktl f count = 
	  match pktl with
		| hd :: [] -> Verify.forwards_pred (f count) hd
		| hd :: tl -> Sat.ZAnd [Verify.forwards_pred (f count) hd; recr tl f (count + 1)]
		| [] -> Sat.ZTrue
	in
	recr l expr 0
  in
  ret

let no_waypoint_expr waypoint_switchnum : (Sat.zVar list -> Sat.zFormula) = 
  let ret history = 
	Sat.ZNot ((fold_pred_or (Test (Switch, make_vint waypoint_switchnum))) history)
  in 
  ret

(* let equal_fields fieldname  : (Sat.zVar list -> Sat.zFormula) = 
  (Verify.equal_single_field fieldname) *)

let exists_waypoint_in_one_history waypoint_switchnum = 
  let ret history = 
	((fold_pred_or (Test (Switch, make_vint waypoint_switchnum))) history) in ret



