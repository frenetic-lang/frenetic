open Packet
open Types
open Util
open Unix
open NetKAT_Sat
open NetKAT_Dehop_Graph

module NetCore_Gensym = struct


    
  type t = string ref
    
  let next_int = ref 0
    
  let gensym () =
    let n = !next_int in
    incr next_int;
    ref ("gensym" ^ string_of_int n)
      
  let gensym_printing str =
    ref str
      
  let to_string sym = !sym

end

open NetCore_Gensym



module Verify = struct
  open Sat
  open SDN_Types
  open Types
  module Z3PacketSet = Set.Make(String)

  let all_fields =
      [ Header InPort 
      ; Header EthSrc
      ; Header EthDst
      ; Header EthType
      ; Header Vlan
      ; Header VlanPcp
      ; Header IPProto
      ; Header IP4Src
      ; Header IP4Dst
      ; Header TCPSrcPort
      ; Header TCPDstPort
      ; Switch 
]


  let encode_header (header: header) (pkt:zVar) : zTerm =
    match header with
      | Header InPort -> 
        TApp (TVar (serialize_header header), [TVar pkt])
      | Header EthSrc -> 
        TApp (TVar (serialize_header header), [TVar pkt])
      | Header EthDst -> 
        TApp (TVar (serialize_header header), [TVar pkt])
      | Header EthType ->  
        TApp (TVar (serialize_header header), [TVar pkt])
      | Header Vlan ->  
        TApp (TVar (serialize_header header), [TVar pkt])
      | Header VlanPcp ->
        TApp (TVar (serialize_header header), [TVar pkt])
      | Header IPProto ->  
        TApp (TVar (serialize_header header), [TVar pkt])
      | Header IP4Src ->  
        TApp (TVar (serialize_header header), [TVar pkt])
      | Header IP4Dst ->  
        TApp (TVar (serialize_header header), [TVar pkt])
      | Header TCPSrcPort ->  
        TApp (TVar (serialize_header header), [TVar pkt])
      | Header TCPDstPort ->  
        TApp (TVar (serialize_header header), [TVar pkt])
      | Switch -> 
        TApp (TVar (serialize_header header), [TVar pkt])

  let encode_packet_equals, reset_state_encode_packet_equals = 
    let hash = Hashtbl.create 0 in 
    let reset_state () = Hashtbl.clear hash; in
    let encode_packet_equals = 
      (fun (pkt1: zTerm) (pkt2: zTerm) (except :header)  -> 
	ZTerm (TApp (
	  (if Hashtbl.mem hash except
	   then
	      Hashtbl.find hash except
	   else
	      let l = 
		List.fold_left 
		  (fun acc hd -> 
		    if  hd = except then 
		      acc 
		    else
		      ZEquals (ZTerm (encode_header hd "x"), ZTerm( encode_header hd "y"))::acc) 
		  [] all_fields in 
	      let new_except = (z3_macro_top ("packet_equals_except_" ^ (serialize_header except) )
				  [("x", SPacket);("y", SPacket)] SBool  
				  (ZIf (ZEquals (ZTerm (pkt1), Z3macro.nopacket ),
					ZEquals (ZTerm (pkt2), Z3macro.nopacket),
					  (ZAnd(l))))) in
	      Hashtbl.add hash except new_except;
	      new_except), 
	  [pkt1; pkt2]))) in
    encode_packet_equals, reset_state

  let encode_vint (v: VInt.t): zTerm = 
    TInt (VInt.get_int64 v)

  
  let range = ( fun i j ->
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc) in
    aux j ([]) )

  let pred_test,pred_test_not,reset_pred_test = 
    let hashmap = Hashtbl.create 0 in
    let false_hashmap = Hashtbl.create 0 in
    let pred_test f =  
      try (Hashtbl.find hashmap f)
      with Not_found -> 
	let macro = z3_macro ((serialize_header f) ^ "-equals") [("x", SPacket); ("v", SInt)] SBool 
	  (ZIf (ZEquals (ZTerm (TVar "x"), Z3macro.nopacket), 
		ZFalse,
		(ZEquals (ZTerm (encode_header f "x"), ZTerm (TVar "v")))
	   ))
	in
	Hashtbl.add hashmap f macro; 
	(Hashtbl.find hashmap f) in	
    let pred_test_not f = 
      try (Hashtbl.find false_hashmap f)
      with Not_found -> 
	let macro = z3_macro ((serialize_header f) ^ "-not-equals") [("x", SPacket); ("v", SInt)] SBool 
	  (ZIf (ZEquals (ZTerm (TVar "x"), Z3macro.nopacket),
		ZTrue,
		 (*ZNot (ZEquals (ZTerm (encode_header f "x"), ZTerm (TVar "v")))*)
		 ZOr [(ZLessThan (ZTerm (encode_header f "x"), ZTerm (TVar "v")));
		 (ZGreaterThan (ZTerm (encode_header f "x"), ZTerm (TVar "v")))]
		
	   )) 
	in
	Hashtbl.add false_hashmap f macro; 
	(Hashtbl.find false_hashmap f) in	

    let reset_pred_test () = Hashtbl.clear hashmap; Hashtbl.clear false_hashmap in
    pred_test, pred_test_not, reset_pred_test

  let reset_state () = 
    reset_state_encode_packet_equals (); 
    fresh_cell := []; 
    decl_list := [];
    reset_pred_test ()

  let rec forwards_pred (prd : pred) (pkt : zVar) : zFormula = 
    let forwards_pred pr : zFormula = forwards_pred pr pkt in
    let rec in_a_neg pred : zFormula = 
      match pred with
	| Neg p -> forwards_pred p
	| False -> ZTrue
	| True -> ZFalse
	| Test (hdr, v) -> ZTerm (TApp (pred_test_not hdr, [TVar pkt; encode_vint v])) 
	| And (pred1, pred2) -> ZOr [in_a_neg pred1; in_a_neg pred1]
	| Or (pred1, pred2) -> ZAnd [in_a_neg pred1; in_a_neg pred2] in
    match prd with
      | False -> 
	ZFalse
      | True -> 
	ZTrue
      | Test (hdr, v) -> ZTerm (TApp (pred_test hdr, [TVar pkt; encode_vint v]))
      | Neg p -> in_a_neg p
      | And (pred1, pred2) -> 
	(ZAnd [forwards_pred pred1; 
	       forwards_pred pred2])
      | Or (pred1, pred2) -> 
	(ZOr [forwards_pred pred1;
	      forwards_pred pred2]) 


  open Z3macro

  let mod_fun,reset_mod_fun = 
    let hashmap = Hashtbl.create 0 in
    let mod_fun f =  
      let packet_equals_fun = encode_packet_equals (TVar "x") (TVar "y") f in
      try ZTerm (Hashtbl.find hashmap packet_equals_fun)
      with Not_found -> 
	let macro = z3_macro ("mod_" ^ (serialize_header f)) [("x", SPacket); ("y", SPacket); ("v", SInt)] SBool 
	  (
	    ZAnd [
		  packet_equals_fun;
		  ZEquals(ZTerm (encode_header f "y"), ZTerm (TVar "v"))]) in
	Hashtbl.add hashmap packet_equals_fun macro; 
	ZTerm (Hashtbl.find hashmap packet_equals_fun) in	
    let reset_mod_fun () = Hashtbl.clear hashmap in
    mod_fun,reset_mod_fun

  let test_previous_packet curr prev = 
    TApp((z3_macro ("test-previous-packet-" ^ curr ^ "-" ^ prev) [("x", SPacket); ("y"), SPacket] SBool
	   (let x_t = ZTerm (TVar "x") in
	    let y_t = ZTerm (TVar "y") in
	    ZIf (ZEquals(x_t, Z3macro.nopacket), 
		 ZEquals (y_t, Z3macro.nopacket),
		 ZEquals( y_t, (ZApp (ZTerm (TVar "PreviousPacket"), [x_t])))))), 
    [TVar curr; TVar prev])

  let rec unzip_list_tuple (t : ('a * 'b) list) : ('a list * 'b list) = 
    match t with 
      | (hdl,hdr)::tl -> 
	let retl, retr = unzip_list_tuple tl in (hdl::retl), (hdr::retr)
      | [] -> ([],[])
	  

    (* output is formula, set formula produces *)
    (* why are we calling this explicitly on p and t as opposed to just on p;t? *)
  let rec forwards_pol (pol : policy) (inpkt : zVar) : zFormula * (Z3PacketSet.t) = 
    let inpkt_t = ZTerm (TVar inpkt) in
    (* let nullinput = ZEquals (inpkt_t, nopacket) in *)
    match pol with 
      | Filter pred -> 
	forwards_pred pred inpkt, (Z3PacketSet.singleton inpkt)
      | Mod(f,v) -> 
	let outpkt = fresh SPacket in
	let outpkt_t = ZTerm (TVar outpkt) in
	let modfn = mod_fun f in
	ZAnd [ ZTerm (test_previous_packet outpkt inpkt);
	  ZApp (modfn, [inpkt_t; outpkt_t; ZTerm (encode_vint v)])],  (Z3PacketSet.singleton outpkt)
      | Par (pol1, pol2) -> 
	let formu1, out1 = forwards_pol pol1 inpkt in
	let formu2, out2 = forwards_pol pol2 inpkt in
	let blacklisting_comment l = 
	  Printf.sprintf "blacklisting %s " (Z3PacketSet.fold (fun x a -> Printf.sprintf "%s %s" x a) l "") in
	ZAnd[
	  ZOr[formu1; ZComment 
	    ( blacklisting_comment out1,
	      (ZAnd (Z3PacketSet.fold (fun x l -> (ZEquals (ZTerm (TVar x), nopacket))::l) out1 [])))];
	  ZOr[formu2; ZComment 
	    ( blacklisting_comment out2, 
	      ZAnd (Z3PacketSet.fold (fun x l -> (ZEquals (ZTerm (TVar x), nopacket))::l) out2 []))]], (Z3PacketSet.union out1 out2)
      | Seq (pol1, pol2) -> 
	let formu', midpkts = forwards_pol pol1 inpkt in
	let outformu, outpkts = unzip_list_tuple 
	  (Z3PacketSet.fold
	     (fun mpkt l -> 
	       let packet_formula, output = forwards_pol pol2 mpkt in
	       (packet_formula, output)::l )
	     midpkts []) in
	ZAnd (formu'::outformu), (List.fold_left Z3PacketSet.union Z3PacketSet.empty outpkts)
      | Star _  -> failwith "NetKAT program not in form (p;t)*"
      | Choice _-> failwith "I'm not rightly sure what a \"choice\" is "
      | Link _ -> failwith "wait, link is a special form now?  What's going on?"

  let rec fold_in_and expr = 
    match expr with 
      | ZAnd l -> 
	let new_list = List.map fold_in_and l in
	ZAnd ((List.flatten (List.map (fun x -> 
	  match x with
	    | ZAnd l -> List.map fold_in_and l
	    | _ -> failwith "filter failed")
			       (List.filter 
				  (fun x -> match x with 
				    | ZAnd l -> true
				    | _ -> false) new_list))) @ 
		 (List.filter (fun x -> match x with | ZAnd l -> false | _ -> true) new_list))
      | _ -> expr

	
  let exists (list : Z3PacketSet.t) func : zFormula = 
    ZOr (Z3PacketSet.fold (fun pkt l -> (func pkt)::l) list [])

  let forall (list : Z3PacketSet.t) func : zFormula = 
    fold_in_and (ZAnd (Z3PacketSet.fold (fun pkt l -> (func pkt)::l )  list []))
      
  let packet_equals p1 p2 = ZEquals(ZTerm (TVar p1), ZTerm (TVar p2))
   	  
  let not_in_history waypoint pkt k = 
    let rec not_in_history waypoint pkt k = 
      match k with 
	| 0 -> ZNot (forwards_pred waypoint pkt)
	| _ -> ZAnd [ZNot (forwards_pred waypoint pkt); not_in_history waypoint (previous_packet pkt) (k-1)] in
    fold_in_and (not_in_history waypoint pkt (k * 2))


(* the multi-input logic is wrong here *)
  let rec forwards_k p_t_star (inpkts : Z3PacketSet.t) k : zFormula * (Z3PacketSet.t) = 
    match p_t_star with 
      | Star (Seq (p, t)) -> 
 	let pol_form, polout = unzip_list_tuple (Z3PacketSet.fold (fun mpkt l -> (forwards_pol p mpkt)::l) inpkts []) in
	let polout = List.fold_left Z3PacketSet.union Z3PacketSet.empty polout in
	let topo_form, topo_out = unzip_list_tuple (Z3PacketSet.fold (fun mpkt l -> (forwards_pol t mpkt)::l) polout []) in
	let topo_out = List.fold_left Z3PacketSet.union Z3PacketSet.empty topo_out in
	(* set inpkt to be history element of all in topo_out *)

	(match k with 
	  | 0 -> ZTrue, inpkts
	  | 1 -> ZAnd[(ZComment ("forwards_k: pol_form",ZAnd pol_form));
	       ZComment ("forwards_k: topo_form", ZAnd topo_form)
	       (*;history_constraint*)
		     ],  topo_out
	  | _ -> 
	    let rest_of_links, final_out = forwards_k p_t_star topo_out (k-1) in
	    ZAnd[(ZComment ("forwards_k: pol_form",ZAnd pol_form));
		 ZComment ("forwards_k: topo_form", ZAnd topo_form);
		 (*history_constraint;*)
		 ZComment ("forward_k_set: recur", rest_of_links)], final_out )
      | _ -> failwith "NetKAT program not in form (p;t)*"


(*
  let forwards_star p_t_star inpkt k : zFormula * (Z3PacketSet.t) = 
    let forwards_k = forwards_k p_t_star (Z3PacketSet.singleton inpkt) in
    let blacklist_packets set = 
      ZAnd (Z3PacketSet.fold (fun x l -> (ZEquals (ZTerm (TVar x), nopacket))::l) (Z3PacketSet.remove inpkt set) []) in
    let rec build_up_forwards j prev_results = 
      let form,set = forwards_k j in
      let forward_comment e blacklist_set = 
	ZComment (Printf.sprintf "attempting to forward in %d hops, output is %s, blacklisting %s" j 
		    (Z3PacketSet.fold (fun pkt l -> Printf.sprintf "%s %s" pkt l ) set "")
		    (Z3PacketSet.fold (fun pkt l -> Printf.sprintf "%s %s" pkt l ) blacklist_set ""), e) in
      if (j = k) then 
	ZOr[(forward_comment form prev_results); blacklist_packets prev_results], set
      else
	let rest_forwards,rest_set = build_up_forwards (j + 1) set in
	let blacklist_set = (Z3PacketSet.diff (Z3PacketSet.union prev_results rest_set) set) in
	let forward_comment e = forward_comment e blacklist_set in
	ZOr [forward_comment (ZAnd[form; blacklist_packets blacklist_set]);
	     rest_forwards], (Z3PacketSet.union set rest_set) in
    let resform, resset = build_up_forwards 1 (Z3PacketSet.singleton inpkt) in
    ZOr [resform; blacklist_packets (Z3PacketSet.remove inpkt resset)],Z3PacketSet.add inpkt resset 
*)

  let forwards_star p_t_star (inpkt : zVar) k : zFormula * (Z3PacketSet.t) = 
    let blacklisting_comment l = 
      Printf.sprintf "blacklisting %s " (Z3PacketSet.fold (fun x a -> Printf.sprintf "%s %s" x a) l "") in
    let forwards_k =  forwards_k p_t_star (Z3PacketSet.singleton inpkt)  in
    let combine_results x = 
      let form,set = forwards_k x in
      let form = ZOr[form; ZComment (blacklisting_comment set,
	ZAnd (Z3PacketSet.fold (fun x l -> (ZEquals (ZTerm (TVar x), nopacket))::l) set []))] in
      ZComment ( Printf.sprintf "Attempting to forward in %u hops" x, form ), set in
    let forms, finalset = unzip_list_tuple ((List.map combine_results (range 0 k))) in
    ZAnd forms, List.fold_left Z3PacketSet.union Z3PacketSet.empty finalset

end


  let run_solve oko prog str : bool =
    let run_result = (
      match oko, Sat.solve prog with
	| Some (ok : bool), (sat : bool) ->
          if ok = sat then
            true
          else
            (Printf.printf "[Verify.check %s: expected %b got %b]\n%!" str ok sat; 
	     (let file = (Filename.get_temp_dir_name ()) ^ Filename.dir_sep ^ "debug-" ^ 
		(to_string (gensym ())) ^ ".rkt" in
		(Printf.printf "Offending program is in %s\n" file;
		 let oc = open_out (file) in 
		 Printf.fprintf oc "%s\n" (Sat.serialize_program prog);
		 close_out oc));
	     false)
	| None, sat ->
          (Printf.printf "[Verify.check %s: %b]\n%!" str sat; false)) in
    Verify.reset_state (); Verify.reset_mod_fun (); run_result

(* str: name of your test (unique ID)
inp: initial packet
   pol: policy to test
outp: fully-transformed packet
oko: bool option. has to be Some. True if you think it should be satisfiable.
*)


  let check_reachability_k  k str inp pol outp extra_conditions oko =
  let x = Sat.fresh Sat.SPacket in
  let clean_history = Verify.test_previous_packet x Sat.Z3macro.nopacket_s in
  let forwards_star_formula, forwards_star_result = Verify.forwards_star (Sat.remove_links pol) x k in
  let prog =     Sat.ZProgram [ 
    Sat.ZDeclareAssert (Verify.forwards_pred inp x)
    ; Sat.ZDeclareAssert (Sat.ZTerm (clean_history))
    ; Sat.ZDeclareAssert forwards_star_formula
    ; Sat.ZToplevelComment (Printf.sprintf "We are choosing between %s " 
			      (Verify.Z3PacketSet.fold (fun x a -> Printf.sprintf "%s %s" x a) 
				 forwards_star_result ""))
    ; Sat.ZDeclareAssert (Verify.exists forwards_star_result 
			    (fun y -> 
			      Sat.ZAnd
				((Verify.forwards_pred outp y) :: 
				    (List.map (fun f -> f y) extra_conditions))))] in 
  run_solve oko prog str

  open NetKAT_Dehop_Graph
    
  let check_reachability str inp pol outp oko = 
    check_reachability_k (longest_shortest (parse_graph pol))
      str inp pol outp [] oko
    
  let check_waypoint str inp pol outp waypoint oko= 
    let k = (longest_shortest (parse_graph pol)) in
    check_reachability_k k
      str inp pol outp [(fun y -> Sat.ZComment ("waypoint check", Verify.not_in_history waypoint y k))] (Some (not oko))

  (*slow version*)
  let check_slice_isolation str inpts pol outpts not_in_slice oko : bool = 
    List.fold_left (fun a b -> a && b) true
    (List.map
       (fun inpt -> 
	 List.fold_left (fun a b -> a && b) true 
	   (List.map 
	      (fun outp -> 
		List.fold_left (fun a b -> a && b) true 
		  (List.map 
		     (fun not_in_slice -> 
		       let reachability_result : bool = check_reachability str inpt pol outp (Some true) in
		       ((reachability_result 
			 && (check_waypoint str inpt pol outp not_in_slice (not oko))) 
			or (not reachability_result))
		     ) not_in_slice))
	      outpts))
       inpts)
      
  let check = check_reachability
    

