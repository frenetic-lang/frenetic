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

  let pred_test,reset_pred_test = 
    let hashmap = Hashtbl.create 0 in
    let pred_test f =  
      try (Hashtbl.find hashmap f)
      with Not_found -> 
	let macro = z3_macro ((serialize_header f) ^ "-equals") [("x", SPacket); ("v", SInt)] SBool 
	  (ZAnd [ZNot (ZEquals (ZTerm (TVar "x"), Z3macro.nopacket));
		 (ZEquals (ZTerm (encode_header f "x"), ZTerm (TVar "v")))
		]) 
	in
	Hashtbl.add hashmap f macro; 
	(Hashtbl.find hashmap f) in	
    let reset_pred_test () = Hashtbl.clear hashmap in
    pred_test, reset_pred_test

  let reset_state () = 
    reset_state_encode_packet_equals (); 
    fresh_cell := []; 
    decl_list := [];
    reset_pred_test ()

  let rec forwards_pred (pred : pred) (pkt : zVar) : zFormula = 

    match pred with
      | False -> 
	ZFalse
      | True -> 
	ZTrue
      | Test (hdr, v) -> ZTerm (TApp (pred_test hdr, [TVar pkt; encode_vint v]))
      | Neg p ->
        (ZNot (forwards_pred p pkt))
      | And (pred1, pred2) -> 
	(ZAnd [forwards_pred pred1 pkt; 
	       forwards_pred pred2 pkt])
      | Or (pred1, pred2) -> 
	(ZOr [forwards_pred pred1 pkt;
              forwards_pred pred2 pkt])

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
	  

    
  let rec forwards_pol (pol : policy) (inpkt : zVar) : zFormula * (zVar list) = 
    let inpkt_t = ZTerm (TVar inpkt) in
    (* let nullinput = ZEquals (inpkt_t, nopacket) in *)
    match pol with 
      | Filter pred -> 
	forwards_pred pred inpkt, [inpkt]
      | Mod(f,v) -> 
	let outpkt = fresh SPacket in
	let outpkt_t = ZTerm (TVar outpkt) in
	let modfn = mod_fun f in
	ZAnd [ ZTerm (test_previous_packet outpkt inpkt);
	  ZApp (modfn, [inpkt_t; outpkt_t; ZTerm (encode_vint v)])],  [outpkt]
      | Par (pol1, pol2) -> 
	let formu1, out1 = forwards_pol pol1 inpkt in
	let formu2, out2 = forwards_pol pol2 inpkt in
	let blacklisting_comment l = 
	  Printf.sprintf "blacklisting %s " (List.fold_left (fun x a -> Printf.sprintf "%s %s" x a) "" l) in
	ZAnd[
	  ZOr[formu1; ZComment 
	    ( blacklisting_comment out1,
	      (ZAnd (List.map (fun x -> ZEquals (ZTerm (TVar x), nopacket)) out1)))];
	  ZOr[formu2; ZComment 
	    ( blacklisting_comment out2, 
	      ZAnd (List.map (fun x -> ZEquals (ZTerm (TVar x), nopacket)) out2))]], out1@out2
      | Seq (pol1, pol2) -> 
	let formu', midpkts = forwards_pol pol1 inpkt in
	let outformu, outpkts = unzip_list_tuple 
	  (List.map 
	     (fun mpkt -> 
	       let packet_formula, output = forwards_pol pol2 mpkt in
	       packet_formula, output )
	     midpkts) in
	ZAnd (formu'::outformu), List.flatten outpkts
      | Star _  -> failwith "NetKAT program not in form (p;t)*"
      | Choice _-> failwith "I'm not rightly sure what a \"choice\" is "
      | Link _ -> failwith "wait, link is a special form now?  What's going on?"
	
  let exists (list : zVar list) func : zFormula = 
    ZOr (List.map (fun pkt -> func pkt) list)

  let forall (list : zVar list) func : zFormula = 
    ZAnd (List.map (fun pkt -> func pkt) list)

  let packet_equals p1 p2 = ZEquals(ZTerm (TVar p1), ZTerm (TVar p2))

  let rec forwards_k p_t_star inpkt k : zFormula * (zVar list) = 
    match p_t_star with 
      | Star (Seq (p, t)) -> 
	let pol_form, polout = forwards_pol p inpkt in
	let topo_form, topo_out = unzip_list_tuple (List.map (fun mpkt -> forwards_pol t mpkt) polout) in
	let topo_out = List.flatten topo_out in
	(* set inpkt to be history element of all in topo_out *)
	let history_constraint = ZAnd (List.map (fun x -> 
	  ZTerm (test_previous_packet x inpkt)
	) topo_out) in

	(match k with 
	  | 0 -> ZTrue, [inpkt]
	  | 1 -> ZAnd[(ZComment ("forwards_k: pol_form",pol_form));
	       ZComment ("forwards_k: topo_form", ZAnd topo_form)
	       (*;history_constraint*)
		     ],  topo_out
	  | _ -> 
	    let rest_of_links, final_out = unzip_list_tuple 
	      (List.map (fun x -> forwards_k p_t_star x (k-1)) topo_out) in
	    ZAnd[(ZComment ("forwards_k: pol_form",pol_form));
		 ZComment ("forwards_k: topo_form", ZAnd topo_form);
		 (*history_constraint;*)
		 ZComment ("forward_k_set: recur", ZAnd rest_of_links)], List.flatten final_out )
      | _ -> failwith "NetKAT program not in form (p;t)*"

  let forwards_star p_t_star inpkt k : zFormula * (zVar list) = 
    let blacklisting_comment l = 
      Printf.sprintf "blacklisting %s " (List.fold_left (fun x a -> Printf.sprintf "%s %s" x a) "" l) in
    let forwards_k =  forwards_k p_t_star inpkt  in
    let combine_results x = 
      let form,set = forwards_k x in
      let form = ZOr[form; ZComment (blacklisting_comment set,
	ZAnd (List.map (fun x -> ZEquals (ZTerm (TVar x), nopacket)) set))] in
      ZComment ( Printf.sprintf "Attempting to forward in %u hops" x, form ), set in
    let forms, finalset = unzip_list_tuple ((List.map combine_results (range 0 k))) in
    ZAnd forms, List.flatten finalset

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

  let check_reachability_k  k str inp pol outp oko =
  let x = Sat.fresh Sat.SPacket in
  let clean_history = Verify.test_previous_packet x Sat.Z3macro.nopacket_s in
  let forwards_star_formula, forwards_star_result = Verify.forwards_star pol x k in
  let prog =     Sat.ZProgram [ 
    Sat.ZDeclareAssert (Verify.forwards_pred inp x)
    (* ; Sat.ZDeclareAssert (Sat.ZTerm (clean_history))*)
    ; Sat.ZDeclareAssert forwards_star_formula
    ; Sat.ZToplevelComment (Printf.sprintf "We are choosing between %s " 
			      (List.fold_left (fun x a -> Printf.sprintf "%s %s" x a) "" 
				 forwards_star_result))
    ; Sat.ZDeclareAssert (Verify.exists forwards_star_result (fun y -> Verify.forwards_pred outp y))] in 
  run_solve oko prog str

  open NetKAT_Dehop_Graph
    
  let check_reachability str inp pol outp oko = 
    check_reachability_k (longest_shortest (parse_graph pol))
      str inp pol outp oko
    


  let check = check_reachability
    

