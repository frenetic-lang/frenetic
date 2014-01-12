open Packet
open Types
open Util
open Unix
open NetKAT_Sat

module type Sat_description = 
sig 
  val serialize_sort : Sat_Syntax.zSort -> string
  val z3_macro_top : string -> (Sat_Syntax.zVar * Sat_Syntax.zSort) list -> Sat_Syntax.zSort -> Sat_Syntax.zFormula -> Sat_Syntax.zTerm
  val z3_macro : string -> (Sat_Syntax.zVar * Sat_Syntax.zSort) list -> Sat_Syntax.zSort -> Sat_Syntax.zFormula -> Sat_Syntax.zTerm
  val fresh : Sat_Syntax.zSort -> Sat_Syntax.zVar
end
  
module Verify = struct
  module Stateless = struct
    open Sat_Syntax
    open Sat_Utils
      
      
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
      
      
    let sint () = SInt
      
    let zterm x = ZEquals(x, TVar "true") 
      
  end
  module Stateful = functor (Sat : Sat_description) -> struct
    open SDN_Types
    open Types
    open Sat_Syntax
    open Sat_Utils
    open Stateless

    module Z3Pervasives = struct
      open Sat
      let declare_datatypes : string = 
	"
(declare-datatypes 
 () 
 ((Packet
   (packet
    (Switch "^serialize_sort SInt ^")
    (EthDst "^serialize_sort SInt ^")
    (EthType "^serialize_sort SInt ^")
    (Vlan "^serialize_sort SInt ^")
    (VlanPcp "^serialize_sort SInt ^")
    (IPProto "^serialize_sort SInt ^")
    (IP4Src "^serialize_sort SInt ^")
    (IP4Dst "^serialize_sort SInt ^")
    (TCPSrcPort "^serialize_sort SInt ^")
    (TCPDstPort "^serialize_sort SInt ^")
    (EthSrc "^serialize_sort SInt ^")
    (InPort "^serialize_sort SInt ^")))))
(declare-datatypes
 ()
 ((Hist 
    (hist-singleton (packet Packet))
    (hist (packet Packet) (rest-hist Hist))
    )))" ^ "\n" 

    end
      

    
    let encode_packet_equals = 
      let open Sat in
      let hash = Hashtbl.create 0 in 
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
      encode_packet_equals 

    let pred_test,pred_test_not = 
      let open Sat in
      let true_hashmap = Hashtbl.create 0 in
      let false_hashmap = Hashtbl.create 0 in
      let pred_test want_true f =  
	let hashmap = (if want_true then true_hashmap else false_hashmap) in
	let name_suffix = (if want_true then "-equals" else "-not-equals") in
	try (Hashtbl.find hashmap f)
	with Not_found -> 
	  let macro = 
	    z3_macro ((serialize_header f) ^ name_suffix)
	      [("x", SPacket); ("v", sint ())] SBool 
	      (if want_true 
	       then
		  (ZEquals ( (encode_header f "x"),  (TVar "v")))
	       else
		  (ZNotEquals ((encode_header f "x"), (TVar "v"))))
	  in
	  Hashtbl.add hashmap f macro;
	  (Hashtbl.find hashmap f) in
      pred_test true, pred_test false 

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
      let open Sat in
      let hashmap = Hashtbl.create 0 in
      let mod_fun f =  
	let packet_equals_fun = encode_packet_equals (TVar "x") (TVar "y") f in
	try (Hashtbl.find hashmap packet_equals_fun)
	with Not_found -> 
	  let macro = z3_macro ("mod_" ^ (serialize_header f)) [("x", SPacket); ("y", SPacket); ("v", sint ())] SBool 
	    (
	      ZAnd [zterm packet_equals_fun;
		    ZEquals( (encode_header f "y"),  (TVar "v"))]) in
	  Hashtbl.add hashmap packet_equals_fun macro; 
	  (Hashtbl.find hashmap packet_equals_fun) in	
      mod_fun 

    let define_relation, get_rules = 
      let open Sat in
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
      define_relation, get_rules

  end

end  
    
(* str: name of your test (unique ID)
inp: initial packet
   pol: policy to test
outp: fully-transformed packet
oko: bool option. has to be Some. True if you think it should be satisfiable.
*)

let run_solve oko prog query ints str serialize_fun solve_fun : bool =
  let file = Printf.sprintf "%s%sdebug-%s.rkt" (Filename.get_temp_dir_name ()) Filename.dir_sep str in
  let oc = open_out (file) in 
  Printf.fprintf oc "%s\n;This is the program corresponding to %s\n" (serialize_fun prog query) str;
  close_out oc;
  let run_result = (
    match oko, solve_fun prog query with
      | Some (ok : bool), (sat : bool) ->
        if ok = sat then
	  true
        else
            (Printf.printf "[Verify.check %s: expected %b got %b]\n%!" str ok sat; 
	     Printf.printf "Offending program is in %s\n" file;
	     false)
      | None, sat ->
        (Printf.printf "[Verify.check %s: %b]\n%!" str sat; false)) in
  run_result 


let check_reachability  str inp pol outp oko =
  let ints = (Sat_Utils.collect_constants (Seq (Seq (Filter inp,pol),Filter outp))) in
  let module Sat = Sat(struct let ints = ints end) in
  let open Verify.Stateless in
  let module Verify = Verify.Stateful(Sat) in
  let x = Pervasives.inpkt in
  let y = Pervasives.outpkt in
  let open Sat_Syntax in
  let entry_sym = Verify.define_relation pol in
  let last_rule = ZDeclareRule (Pervasives.qrule, [x;y],
				    ZAnd[Verify.forwards_pred inp x;
					     Verify.forwards_pred outp y;
					     zterm (TApp (TVar entry_sym, [TVar x; TVar y]))] ) in
  let prog = ZProgram ( List.flatten
			      [Pervasives.declarations;
			       ZToplevelComment("rule that puts it all together\n")::last_rule
			       ::ZToplevelComment("syntactically-generated rules\n")::(Verify.get_rules())] ) in
  let query = Pervasives.reachability_query in
  let pervasives = Verify.Z3Pervasives.declare_datatypes in
  let serialize = (Sat.serialize_program pervasives) in
  let solve = (Sat.solve pervasives) in
  run_solve oko prog query ints str serialize solve

  let check = check_reachability
