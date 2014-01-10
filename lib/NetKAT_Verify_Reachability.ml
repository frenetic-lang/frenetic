open Packet
open Types
open Util
open Unix
open NetKAT_Sat

module Verify = struct
  open Sat
  open SDN_Types
  open Types

  let reset_state, register_state = 
    let state_tracked = ref [] in
    let reset_state () = 
      Sat.reset_state ();
      List.iter (fun x -> x ()) (!state_tracked) in
    let register_state st =
      state_tracked := st::(!state_tracked) in
    reset_state, register_state
      
  let encode_packet_equals = 
    let hash = Hashtbl.create 0 in 
    let reset_packet_equals () = Hashtbl.clear hash; in
    let encode_packet_equals = 
      (fun (pkt1: zTerm) (pkt2: zTerm) (except :header)  -> 
	(TApp (
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
		      ZEquals ( (encode_header hd "x"), ( encode_header hd "y"))::acc)
		  [] all_fields in 
	      let new_except = (z3_macro_top ("packet_equals_except_" ^ (serialize_header except) )
				  [("x", SPacket);("y", SPacket)] SBool  
				  (ZAnd(l))) in
	      Hashtbl.add hash except new_except;
	      new_except), 
	  [pkt1; pkt2]))) in
    register_state reset_packet_equals; 
    encode_packet_equals

  module Pervasives = struct
    let startpkt = "starting_packet"
    let endpkt = "ending_packet"
    let inpkt = "inpkt"
    let midpkt = "midpkt"
    let outpkt = "outpkt"
    let qrule = "q"
    let declarations = [ZDeclareVar(startpkt, SPacket);
			ZDeclareVar(endpkt, SPacket);
			ZDeclareVar(inpkt, SPacket);
			ZDeclareVar(midpkt, SPacket);
			ZDeclareVar(outpkt, SPacket);
			ZDeclareVar(qrule, SRelation([SPacket; SPacket]))]
    let reachability_query = (Printf.sprintf "(query (q %s %s) 
:default-relation smt_relation2
:engine PDR
:print-answer false)
" startpkt endpkt)
  end

  let pred_test,pred_test_not = 
    let true_hashmap = Hashtbl.create 0 in
    let false_hashmap = Hashtbl.create 0 in
    let pred_test want_true f =  
      let hashmap = (if want_true then true_hashmap else false_hashmap) in
      let name_suffix = (if want_true then "-equals" else "-not-equals") in
      try (Hashtbl.find hashmap f)
      with Not_found -> 
	let macro = 
	  z3_macro ((serialize_header f) ^ name_suffix)
	    [("x", SPacket); ("v", SInt)] SBool 
	    (if want_true 
	     then
		(ZEquals ( (encode_header f "x"),  (TVar "v")))
	     else
		(ZOr [(ZLessThan ( (encode_header f "x"),  (TVar "v")));
		      (ZGreaterThan ( (encode_header f "x"),  (TVar "v")))]))
	in
	Hashtbl.add hashmap f macro;
	(Hashtbl.find hashmap f) in
    let reset_pred_test () = Hashtbl.clear true_hashmap; Hashtbl.clear false_hashmap in
    register_state reset_pred_test; pred_test true, pred_test false

  let zterm x = ZEquals(x, TVar "true")

  let rec forwards_pred (prd : pred) (pkt : zVar) : zFormula = 
    let forwards_pred pr : zFormula = forwards_pred pr pkt in
    let rec in_a_neg pred : zFormula = 
      match pred with
	| Neg p -> forwards_pred p
	| False -> ZTrue
	| True -> ZFalse
	| Test (hdr, v) -> zterm (TApp (pred_test_not hdr, [TVar pkt; encode_vint v])) 
	| And (pred1, pred2) -> ZOr [in_a_neg pred1; in_a_neg pred1]
	| Or (pred1, pred2) -> ZAnd [in_a_neg pred1; in_a_neg pred2] in
    match prd with
      | False -> 
	ZFalse
      | True -> 
	ZTrue
      | Test (hdr, v) -> zterm (TApp (pred_test hdr, [TVar pkt; encode_vint v]))
      | Neg p -> in_a_neg p
      | And (pred1, pred2) -> 
	(ZAnd [forwards_pred pred1; 
	       forwards_pred pred2])
      | Or (pred1, pred2) -> 
	(ZOr [forwards_pred pred1;
	      forwards_pred pred2]) 

  let mod_fun = 
    let hashmap = Hashtbl.create 0 in
    let mod_fun f =  
      let packet_equals_fun = encode_packet_equals (TVar "x") (TVar "y") f in
      try (Hashtbl.find hashmap packet_equals_fun)
      with Not_found -> 
	let macro = z3_macro ("mod_" ^ (serialize_header f)) [("x", SPacket); ("y", SPacket); ("v", SInt)] SBool 
	  (
	    ZAnd [zterm packet_equals_fun;
		  ZEquals( (encode_header f "y"),  (TVar "v"))]) in
	Hashtbl.add hashmap packet_equals_fun macro; 
	(Hashtbl.find hashmap packet_equals_fun) in	
    let reset_mod_fun () = Hashtbl.clear hashmap in
    register_state reset_mod_fun; mod_fun

  let define_relation, get_rules = 
    let hashtbl = Hashtbl.create 0 in
    (*convenience names *)
    let inpkt = Pervasives.inpkt in
    let inpkt_t = TVar inpkt in
    let outpkt = Pervasives.outpkt in
    let outpkt_t = TVar outpkt in
    let midpkt = Pervasives.midpkt in
    let midpkt_t = TVar midpkt in
    let rec define_relation pol = 
      try 
	fst (Hashtbl.find hashtbl pol)
      with Not_found -> 
	let sym = fresh (SRelation [SPacket; SPacket]) in
	let rules = 
	  ZToplevelComment (Pretty.string_of_policy pol)::
	  (match pol with 
	    | Filter pred -> 
	      [ZToplevelComment("this is a filter");
	       ZDeclareRule (sym, [inpkt; outpkt], ZAnd [forwards_pred pred inpkt; ZEquals( inpkt_t,  outpkt_t )])]
	    | Mod(f,v) -> 
	      let modfn = mod_fun f in
	      [ZToplevelComment("this is a mod");
	       ZDeclareRule (sym, [inpkt; outpkt], zterm (TApp (modfn, [inpkt_t; outpkt_t; (encode_vint v)])))]
	    | Par (pol1, pol2) -> 
	      let pol1_sym = TVar (define_relation pol1) in
	      let pol2_sym = TVar (define_relation pol2) in
 	      [ZToplevelComment("this is a par");
	       ZDeclareRule (sym, [inpkt; outpkt], zterm (TApp (pol1_sym, [inpkt_t; outpkt_t]))); 
	       ZDeclareRule (sym, [inpkt; outpkt], zterm (TApp (pol2_sym, [inpkt_t; outpkt_t])))]
	    | Seq (pol1, pol2) -> 
	      let pol1_sym = TVar (define_relation pol1) in
	      let pol2_sym = TVar (define_relation pol2) in
 	      [ZToplevelComment("this is a seq");
	       ZDeclareRule (sym, [inpkt; outpkt], ZAnd[ zterm (TApp (pol1_sym, [inpkt_t; midpkt_t])); 
							 zterm (TApp (pol2_sym, [midpkt_t; outpkt_t]))])]
	    | Star pol1  -> 
	      let pol1_sym = TVar (define_relation pol1) in
	      [ZToplevelComment("this is a star");
	       ZDeclareRule (sym, [inpkt; outpkt], ZEquals (inpkt_t, outpkt_t)); 
	       ZDeclareRule (sym, [inpkt; outpkt], ZAnd[ zterm (TApp (pol1_sym, [inpkt_t; midpkt_t]) ); 
							 zterm (TApp (TVar sym, [midpkt_t; outpkt_t]))])]
	    | Choice _-> failwith "I'm not rightly sure what a \"choice\" is "
	    | Link _ -> failwith "wait, link is a special form now?  What's going on?") in
	Hashtbl.add hashtbl pol (sym,rules); sym in
    let get_rules () = Hashtbl.fold (fun _ rules a -> snd(rules)@a ) hashtbl [] in
    let reset_rules_table () = Hashtbl.clear hashtbl in
    register_state reset_rules_table; define_relation, get_rules

end

  let run_solve oko prog query str : bool =
    let file = Printf.sprintf "%s%sdebug-%s.rkt" (Filename.get_temp_dir_name ()) Filename.dir_sep str in
    let oc = open_out (file) in 
    Printf.fprintf oc "%s\n;This is the program corresponding to %s\n" (Sat.serialize_program prog query) str;
    close_out oc;
    let run_result = (
      match oko, Sat.solve prog query with
	| Some (ok : bool), (sat : bool) ->
          if ok = sat then
	    true
          else
            (Printf.printf "[Verify.check %s: expected %b got %b]\n%!" str ok sat; 
	     Printf.printf "Offending program is in %s\n" file;
	     false)
	| None, sat ->
          (Printf.printf "[Verify.check %s: %b]\n%!" str sat; false)) in
    Verify.reset_state (); run_result

(* str: name of your test (unique ID)
inp: initial packet
   pol: policy to test
outp: fully-transformed packet
oko: bool option. has to be Some. True if you think it should be satisfiable.
*)


  let check_reachability  str inp pol outp oko =
  let x = Verify.Pervasives.inpkt in
  let y = Verify.Pervasives.outpkt in
  let entry_sym = Verify.define_relation pol in
  let last_rule = Sat.ZDeclareRule (Verify.Pervasives.qrule, [x;y],
				    Sat.ZAnd[Verify.forwards_pred inp x;
					     Verify.forwards_pred outp y;
					     Verify.zterm (Sat.TApp (Sat.TVar entry_sym, [Sat.TVar x; Sat.TVar y]))] ) in
  let prog = Sat.ZProgram ( List.flatten
			      [Verify.Pervasives.declarations;
			       Sat.ZToplevelComment("rule that puts it all together\n")::last_rule
			       ::Sat.ZToplevelComment("syntactically-generated rules\n")::(Verify.get_rules())] ) in

  run_solve oko prog Verify.Pervasives.reachability_query str
    
  let check = check_reachability
