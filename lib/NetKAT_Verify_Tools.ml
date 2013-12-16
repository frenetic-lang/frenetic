
open SDN_Types
open Types
open VInt
open NetKAT_Verify_Reachability
open NetKAT_Sat.Sat
open NetKAT_Dehop_Graph

let make_vint v = VInt.Int64 (Int64.of_int v)

let verify (description: string) (initial_state: pred) (program: policy) (final_state: pred) (desired_outcome: bool) : bool = 
	check description initial_state program final_state (Some desired_outcome)

let verify_k k (description: string) (initial_state: pred) (program: policy) (final_state: pred) (desired_outcome: bool) : bool = 
	check_reachability_k k description initial_state program final_state [] (Some desired_outcome)

let verify_waypoint = check_waypoint

(* let verify_history (description: string) (initial_state: pred) (program: policy) expr (final_state: pred) (desired_outcome: bool) : bool = 
	check_with_history expr description initial_state program final_state (Some desired_outcome)

let verify_specific_k (description: string) (initial_state: pred) (program: policy) (final_state: pred) (desired_outcome: bool) k : bool = 
	check description initial_state program final_state (Some desired_outcome) *)

let make_transition (s1, p1) (s2, p2) = remove_links (Link (make_vint s1, make_vint p1, make_vint s2, make_vint p2))

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

let make_switch = make_packet_1

let make_packet_2 switch port  = 
  And (Test (Switch, make_vint switch), Test (Header SDN_Types.InPort, make_vint port))

let make_packet_3 switch port ethsrc  = 
  And ((make_packet_2 switch port), Test (Header SDN_Types.EthSrc, make_vint ethsrc))

let make_packet_4 switch port ethsrc ethdst  = 
  And ((make_packet_3 switch port ethsrc), Test (Header SDN_Types.EthDst, make_vint ethdst))

let dijkstra_test topo = longest_shortest (parse_graph (make_simple_topology topo))

let bool_to_z3 b = if b then ZTrue else ZFalse

let map_fun pred pkt = 
  Verify.forwards_pred pred pkt

let fold_pred_and pred: (zVar list -> zFormula) = 
  let ret l = 
	ZAnd (List.map (map_fun pred) l)
  in ret

let fold_pred_or pred: (zVar list -> zFormula) = 
  let ret l = 
	ZOr (List.map (map_fun pred) l)
  in ret

let matches_history preds : (zVar list -> zFormula) = 
  let ret l =  
	let rec recr pktl predl =
	  (match pktl with 
		| [] -> (match predl with 
			| [] -> bool_to_z3 true
			| _ -> bool_to_z3 false)
		| hd :: tl -> ZAnd [(Verify.forwards_pred (List.hd predl) hd); recr tl (List.tl predl) ])
	in
	recr l preds
  in
  ret

let fold_pred_or_with_counter (expr : int -> pred) : (zVar list -> zFormula) = 
  let ret l = 
	let rec recr pktl f count = 
	  match pktl with
		| hd :: [] -> Verify.forwards_pred (f count) hd
		| hd :: tl -> ZOr [Verify.forwards_pred (f count) hd; recr tl f (count + 1)]
		| [] -> ZTrue
	in
	recr l expr 0
  in
  ret

let junct_juncts_counter outer inner (exprs : (int -> pred) list ) : (zVar list -> zFormula) = 
  let ret (l : 'a list) = 
	let rec recr (count : int) (pktl : 'a list) (f : int -> pred) = 
	  match pktl with
		| hd :: [] -> Verify.forwards_pred (f count) hd
		| hd :: tl -> inner [Verify.forwards_pred (f count) hd; recr (count + 1) tl f ]
		| [] -> ZTrue
	in
	outer (List.map (recr 0 l) exprs)
  in
  ret

let conjunct_disjuncts_counter = junct_juncts_counter (fun n -> ZAnd n) (fun n -> ZOr n)

let fold_pred_and_with_counter (expr : int -> pred) : (zVar list -> zFormula) = 
  let ret l = 
	let rec recr pktl f count = 
	  match pktl with
		| hd :: [] -> Verify.forwards_pred (f count) hd
		| hd :: tl -> ZAnd [Verify.forwards_pred (f count) hd; recr tl f (count + 1)]
		| [] -> ZTrue
	in
	recr l expr 0
  in
  ret

let no_waypoint_expr waypoint_switchnum : (zVar list -> zFormula) = 
  let ret history = 
	ZNot ((fold_pred_or (Test (Switch, make_vint waypoint_switchnum))) history)
  in 
  ret

(* let equal_fields fieldname  : (zVar list -> zFormula) = 
  (Verify.equal_single_field fieldname) *)

let exists_waypoint_in_one_history waypoint_switchnum = 
  let ret history = 
	((fold_pred_or (Test (Switch, make_vint waypoint_switchnum))) history) in ret

let rec print_predicate p = 
  match p with 
    | False  -> "False"
    | True -> "True"
    | Test (hdr, v) -> Printf.sprintf "%s = %d" (serialize_header hdr) (Int64.to_int (VInt.get_int64 v))
    | Neg p -> "!(p)"
    | And (p1, p2) -> Printf.sprintf "(%s) && (%s)" (print_predicate p1) (print_predicate p2)
    | Or (p1, p2) -> Printf.sprintf "(%s) || (%s)" (print_predicate p1) (print_predicate p2)

let rec print_program p = 
  match p with
    | Filter pred -> Printf.sprintf "Test (%s)" (print_predicate pred)
    | Mod (hdr, value) -> Printf.sprintf "%s <- %d" (serialize_header hdr) (Int64.to_int (VInt.get_int64 value))
    | Par (p1, p2) -> Printf.sprintf "(%s) | (%s)" (print_program p1) (print_program p2)
    | Seq (p1, p2) -> Printf.sprintf "%s; %s" (print_program p1)  (print_program p2)
    | Star s -> Printf.sprintf "(%s)*" (print_program s)
    | Link _ -> failwith "no links please"
    | Choice _ -> failwith "no choices please"
      
