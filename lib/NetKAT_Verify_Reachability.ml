open Packet
open Types
open Util
open Unix
open NetKAT_Sat

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
	  (ZEquals ( (encode_header f "x"),  (TVar "v")))
	in
	Hashtbl.add hashmap f macro; 
	(Hashtbl.find hashmap f) in	
    let pred_test_not f = 
      try (Hashtbl.find false_hashmap f)
      with Not_found -> 
	let macro = z3_macro ((serialize_header f) ^ "-not-equals") [("x", SPacket); ("v", SInt)] SBool 
	  (ZOr [(ZLessThan ( (encode_header f "x"),  (TVar "v")));
		(ZGreaterThan ( (encode_header f "x"),  (TVar "v")))])
	in
	Hashtbl.add false_hashmap f macro; 
	(Hashtbl.find false_hashmap f) in	

    let reset_pred_test () = Hashtbl.clear hashmap; Hashtbl.clear false_hashmap in
    pred_test, pred_test_not, reset_pred_test

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


  open Z3macro

  let mod_fun,reset_mod_fun = 
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
    mod_fun,reset_mod_fun


  let rec unzip_list_tuple (t : ('a * 'b) list) : ('a list * 'b list) = 
    match t with 
      | (hdl,hdr)::tl -> 
	let retl, retr = unzip_list_tuple tl in (hdl::retl), (hdr::retr)
      | [] -> ([],[])
	  

  let define_relation, get_rules, reset_rules_table = 
    let hashtbl = Hashtbl.create 0 in
    let inpkt = Z3macro.inpkt in
    let inpkt_t = TVar inpkt in
    let outpkt = Z3macro.outpkt in
    let outpkt_t = TVar outpkt in
    let midpkt = Z3macro.midpkt in
    let midpkt_t = TVar midpkt in
    let define_relation pol = 
      let counter = ref 0 in
      let rec define_relation pol = 
	try 
	  fst (Hashtbl.find hashtbl pol)
	with Not_found -> 
	  let sym = fresh (SRelation [SPacket; SPacket]) in
	  counter := ((!counter) +1);
	  let local_counter = !counter in
	  let rules = ZToplevelComment(Printf.sprintf "%d" local_counter)::
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
      define_relation pol in
    let get_rules () = Hashtbl.fold (fun _ rules a -> snd(rules)@a ) hashtbl [] in
    let reset_rules_table () = Hashtbl.clear hashtbl in
    define_relation, get_rules, reset_rules_table


  let reset_state () = 
    reset_state_encode_packet_equals (); 
    fresh_cell := []; 
    decl_list := [];
    reset_pred_test ();
    reset_rules_table ();
    reset_mod_fun ()
end

  let run_solve oko prog str : bool =
    let file = (Filename.get_temp_dir_name ()) ^ Filename.dir_sep ^ "debug-" ^ str ^ "-" ^
       ".rkt" in
    let oc = open_out (file) in 
    Printf.fprintf oc "%s\n;This is the program corresponding to %s" (Sat.serialize_program prog) str;
    close_out oc;
    let run_result = (
      match oko, Sat.solve prog with
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
  let x = Sat.Z3macro.inpkt in
  let y = Sat.Z3macro.outpkt in
  let entry_sym = Verify.define_relation pol in
  let last_rule = Sat.ZDeclareRule (Sat.Z3macro.q, [x;y], 
				    Sat.ZAnd[Verify.forwards_pred inp x; 			   
					     Verify.forwards_pred outp y; 
					     Verify.zterm (Sat.TApp (Sat.TVar entry_sym, [Sat.TVar x; Sat.TVar y]))] ) in
  let prog = Sat.ZProgram ( Sat.ZToplevelComment("rule that puts it all together\n")::last_rule
			    ::Sat.ZToplevelComment("syntactically-generated rules\n")::(Verify.get_rules()) ) in
    run_solve oko prog str

  open NetKAT_Dehop_Graph
    
    
      
  let check = check_reachability
    

