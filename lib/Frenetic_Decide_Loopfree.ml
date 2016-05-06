open Core.Std
open Frenetic_Decide_Ast
open Frenetic_Decide_Util
open Frenetic_Decide_Deriv.BDDDeriv

let rec one_dup_trm (trm : Term.t) = let open Term in
  match trm.Hashcons.node with
  | Dup -> one
  | Times trms -> times (List.map trms one_dup_trm)
  | Plus trms -> plus (TermSet.map trms one_dup_trm)
  | Not trm -> not trm
  | Star trm -> star (one_dup_trm trm)
  | _ -> trm

let loop_freedom edge_pol pol topo _ (*out_edge_pol *) =
  if set_univ [Term.values edge_pol; Term.values pol; Term.values topo]
  then
    begin
      let p_t = Term.times [pol;topo] in
      let trm = Term.times [edge_pol; Term.star p_t] in
      let pset = get_e (make_term (TermSet.singleton (one_dup_trm trm))) in
      let inner_term = Term.times [p_t; Term.star p_t] in
      let inner_e = get_e (make_term (TermSet.singleton (one_dup_trm inner_term))) in
      (* Printf.printf "So far elapsed: %f\n" (Sys.time ()); *)
      (* Printf.printf "We have this many bases to iterate over: %u\n" (Base.Set.cardinal pset); *)
      (* Printf.printf "We have this many bases to query for membership: %u\n" (Base.Set.cardinal inner_e); *)
      EMatrix.fold pset
	~f:(fun acc pt ->
	  let beta = snd pt in
	  let beta_beta =
	    (beta, beta) in
	  if (not (EMatrix.run inner_e beta_beta))
	  then acc
	  else (Printf.printf
		  "Bad! Circular path found: Entering at %s, we loop on %s\n"
		  (packet_to_string (fst pt))
		  (packet_to_string (snd pt));
		false)
	) ~init:true
    end
  else true

