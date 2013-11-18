open Packet
open NetCore_Types
open NetCore_Util
open SDN_Headers
open Unix
open NetCore_Gensym

(* The [Sat] module provides a representation of formulas in
   first-order logic, a representation of packets, and a function for
   testing their satisfiability. *)
module Sat = struct

  type zVar = string

  type zSort = 
    | SPacket
    | SInt
    | SSet 
    | SBool
    | SFunction of (zSort list) * zSort
    | SMacro of ((zVar * zSort) list) * zSort

  type zTerm = 
    | TUnit 
    | TVar of zVar
    | TInt of Int64.t
    | TPkt of switchId * portId * packet
    | TApp of zTerm * (zTerm list)

  type zFormula =
    | ZNoop
    | ZTerm of zTerm
    | ZTrue
    | ZFalse 
    | ZNot of zFormula
    | ZAnd of zFormula list
    | ZOr of zFormula list
    | ZEquals of zFormula * zFormula
    | ZComment of string * zFormula
    | ZForall of ((zVar * zSort) list) * zFormula
(*    | ZExists of ((zVar * zSort) list) * zFormula *)
    | ZIf of zFormula * zFormula * zFormula
    | ZApp of zFormula * (zFormula list)

  type zDeclare = 
    | ZDeclareVar of zVar * zSort
    | ZDefineVar of zVar * zSort * zFormula
    | ZDeclareAssert of zFormula
    | ZToplevelComment of string

  type zProgram = 
    | ZProgram of zDeclare list

  (* fresh variables *)
  let fresh_cell = ref []
  let decl_list = ref []

  let fresh s = 
    let l = !fresh_cell in  
    let n = List.length l in 
    let x = match s with
      | SPacket -> 
        Printf.sprintf "_pkt%d" n
      | SInt -> 
        Printf.sprintf "_n%d" n
      | SSet -> 
        Printf.sprintf "_s%d" n 
      | SFunction _ -> 
        Printf.sprintf "_f%d" n 
      | _ -> failwith "not implemented in fresh" in 
    fresh_cell := ZDeclareVar(x,s)::l;
    x




  (* serialization *)
  let serialize_located_packet (sw,pt,pkt) = 
    Printf.sprintf "(Packet %s %s %s %s)" 
      (Int64.to_string sw) 
      (Int32.to_string pt)
      (Int64.to_string pkt.dlSrc)
      (Int64.to_string pkt.dlDst)


  let rec serialize_sort = function
    | SInt -> 
      "Int"
    | SPacket -> 
      "Packet"
    | SBool ->
      "Bool"
    | SSet -> 
      Printf.sprintf "Set"
    | SFunction(sortlist,sort2) -> 
      Printf.sprintf "(%s) %s" 
        (intercalate serialize_sort " " sortlist)
        (serialize_sort sort2)
    | SMacro(args,ret) -> 
      let serialize_arglist args = 
	(intercalate (fun (a, t) -> Printf.sprintf "(%s %s)" a (serialize_sort t)) " " args) in
      Printf.sprintf "(%s) %s"
	(serialize_arglist args)
	(serialize_sort ret)

  let serialize_arglist args = 
    (intercalate (fun (a, t) -> Printf.sprintf "(%s %s)" a (serialize_sort t)) " " args)

	 

  let rec serialize_term term : string = 
    match term with 
      | TUnit -> 
        "()"
      | TVar x -> 
	x
      | TPkt (sw,pt,pkt) -> 
	serialize_located_packet (sw,pt,pkt)
      | TInt n -> 
	Printf.sprintf "%s" 
          (Int64.to_string n)
      | TApp (term1, terms) -> 
	Printf.sprintf "(%s %s)" (serialize_term term1) (intercalate serialize_term " " terms)

  let serialize_comment c = 
    Printf.sprintf "%s" c

  let rec serialize_formula = function
    | ZNoop -> ""
    | ZTrue -> 
      Printf.sprintf "true"
    | ZFalse -> 
      Printf.sprintf "false"
    | ZNot f1 -> 
      Printf.sprintf "(not %s)" (serialize_formula f1)
    | ZEquals (t1, t2) -> 
      Printf.sprintf "(equals %s %s)" (serialize_formula t1) (serialize_formula t2)
    | ZAnd([]) -> 
      Printf.sprintf "true"
    | ZAnd([f]) -> 
      Printf.sprintf "%s" (serialize_formula f)
    | ZAnd(f::fs) -> 
      Printf.sprintf "(and %s %s)" (serialize_formula f) (serialize_formula (ZAnd(fs)))
    | ZOr([]) -> 
      Printf.sprintf "false"
    | ZOr([f]) -> 
      Printf.sprintf "%s" (serialize_formula f)
    | ZOr(f::fs) -> 
      Printf.sprintf "(or %s %s)" (serialize_formula f) (serialize_formula (ZOr(fs)))
    | ZComment(c, f) -> 
      Printf.sprintf "\n;%s\n%s\n; END %s\n" c (serialize_formula f) c
    | ZForall (args, form) ->
      Printf.sprintf "(forall (%s) %s)" (serialize_arglist args) (serialize_formula form)
(*    | ZExists (args, form) ->
      Printf.sprintf "(exists (%s) %s)" (serialize_arglist args) (serialize_formula form) *)
    | ZTerm t -> serialize_term t
    | ZIf (i, t, e) -> Printf.sprintf "(ite %s %s %s)" 
      (serialize_formula i) (serialize_formula t) (serialize_formula e)
    | ZApp (term1, terms) -> 
      Printf.sprintf "(%s %s)" (serialize_formula term1) (intercalate serialize_formula " " terms)

  let serialize_declare d = 
    match d with 
      | ZToplevelComment(c) -> 
	Printf.sprintf "\n;%s" c 
      | ZDefineVar (x, s, b) -> 
	Printf.sprintf "(define-fun %s %s %s)" x (serialize_sort s) (serialize_formula b)
      | ZDeclareVar (x, s) ->
        (match s with 
          | SFunction _ -> Printf.sprintf "(declare-fun %s %s)" x (serialize_sort s)
	  | SMacro _ -> failwith "macros should be in ZDefineVar"
	  | SPacket -> 
	    "(declare-var "^x^" "^(serialize_sort s)^")" 
          | _ -> Printf.sprintf "(declare-var %s %s)" x (serialize_sort s)
	)
      | ZDeclareAssert(f) -> 
        Printf.sprintf "(assert %s)" (serialize_formula f)

  let define_z3_macro (name : string) (arglist : (zVar * zSort) list)  (rettype : zSort) (body : zFormula)  = 
    [ZDefineVar (name, SMacro (arglist, rettype), body)]


  let zApp x = (fun l -> ZTerm (TApp (x, l)))

  let z3_macro, z3_macro_top = 
    let z3_macro_picklocation put_at_top (name : string) (arglist : (zVar * zSort) list) (rettype : zSort)(body : zFormula) : zTerm = 
      let l = !fresh_cell in
      let name = name (* ^ "_" ^ to_string (gensym ()) *) in
      let new_macro = (define_z3_macro name arglist rettype body) in
      (if put_at_top then
	fresh_cell := new_macro @ l
       else
	  decl_list := new_macro @ (!decl_list));
      TVar name in
      
    let z3_macro = z3_macro_picklocation false in
    let z3_macro_top = z3_macro_picklocation true in
    z3_macro, z3_macro_top


    
  module Z3macro = struct
    let nopacket = (ZTerm (TVar "nopacket")) 
  end
  open Z3macro
      
  let pervasives : string = 
    "
(declare-datatypes 
 () 
 ((Packet
   (nopacket)
   (packet 
    (PreviousPacket Packet)
    (Switch Int) 
    (EthDst Int) 
    (EthType Int) 
    (Vlan Int) 
    (VlanPcp Int) 
    (IPProto Int) 
    (IP4Src Int) 
    (IP4Dst Int) 
    (TCPSrcPort Int) 
    (TCPDstPort Int) 
    (EthSrc Int) 
    (InPort Int)))))" ^ "\n" 
      
      
      
  let serialize_program p : string = 
    let ZProgram(ds) = p in 
    let ds' = List.flatten [!fresh_cell; 
			    [ZToplevelComment("end initial declarations, commence dependent declarations\n")];
			    !decl_list;
			    [ZToplevelComment("End Definitions, Commence SAT expressions\n")]; 
			    ds] in 
    Printf.sprintf "%s%s\n(check-sat)\n"
      pervasives (intercalate serialize_declare "\n" ds') 

  let solve prog : bool = 
    let s = serialize_program prog in 
    let z3_out,z3_in = open_process "z3 -in -smt2 -nw" in 
    let _ = output_string z3_in s in
    let _ = flush z3_in in 
    let _ = close_out z3_in in 
    let b = Buffer.create 17 in 
    (try
       while true do
         Buffer.add_string b (input_line z3_out);
         Buffer.add_char b '\n';
       done
     with End_of_file -> ());
    Buffer.contents b = "sat\n"
end

module Verify_Graph = struct
  open Topology
  open NetKAT_Types
  module S = SDN_Types
	
  let longest_shortest graph = 
    let vertices = Topology.get_vertices graph in
    let f acc v = 
      List.fold_left 
        (fun acc v' -> 
	  try
	    let p = Topology.shortest_path graph v v'  in
            max (List.length p) acc
	  with _ -> acc) (* TODO(mmilano): should be acc, not 0 right? *)
        0 vertices in 
    List.fold_left f 0 vertices

  let unfold_graph graph =
    let edges = Topology.get_edges graph in
    let src_port_vals h =
      let swSrc =
	(match Link.src h with
	  | Node.Switch (str, id) -> VInt.Int64 id
	  | _ -> failwith "Switch not in proper form" ) in
      let swDst =
	(match Link.dst h with
	  | Node.Switch (str, id) -> VInt.Int64 id
	  | _ -> failwith"Switch not in proper form" ) in
      let prtSrc = 
	let val64 = Int64.of_int32 (Link.srcport h) in
	VInt.Int64 val64
      in
      let prtDst = 
	let val64 = Int64.of_int32 (Link.dstport h) in
	VInt.Int64 val64 in
      (swSrc, prtSrc, swDst, prtDst) in
    let rec create_pol edgeList =
      match edgeList with
	| h::[] ->
	  let (swSrc, prtSrc, swDst, prtDst) = src_port_vals h in
	  Seq ( Seq (Filter (Test (Switch, swSrc)),
		     Filter (Test( Header S.InPort, prtSrc))),
		Seq (Mod (Switch, swDst), Mod (Header S.InPort, prtDst)))
	| h::t -> 
	  let (swSrc, prtSrc, swDst, prtDst) = src_port_vals h in
	  Seq ( Seq ( Seq (Filter (Test (Switch, swSrc)), 
			   Filter (Test (Header SDN_Types.InPort, prtSrc))),
		      Seq (Mod (Switch, swDst), Mod (Header S.InPort, prtDst))),
		create_pol t)
	| _ -> failwith "non-empty lists only for now." in 
    create_pol edges

  let parse_graph ptstar = 
    let graph = Topology.empty in 
    let rec parse_links (graph: Topology.t) (pol: policy): Topology.t = 
      let assemble switch1 port1 switch2 port2 : Topology.t =
	match port1, port2 with 
	  | VInt.Int64 port1, VInt.Int64 port2 -> 
	    let (node1: Node.t) = Node.Switch ("fresh tag", VInt.get_int64 switch1) in
	    let (node2: Node.t) = Node.Switch ("fresh tag", VInt.get_int64 switch2) in
	    Topology.add_switch_edge graph node1 (Int64.to_int32 port1) node2 (Int64.to_int32 port2)
	  | _,_ -> failwith "need int64 people" in 
      match pol with
	| Seq (Filter(And (Test (Switch, switch1), Test (Header SDN_Types.InPort, port1))),
	       Seq (Mod (Switch, switch2), Mod (Header S.InPort, port2)))
	  -> (assemble switch1 port1 switch2 port2)
	  
	| Par
	    (Seq (Filter(And (Test (Switch, switch1), Test (Header SDN_Types.InPort, port1))),
		  Seq (Mod (Switch, switch2), Mod (Header S.InPort, port2))), t)
	  -> parse_links (assemble switch1 port1 switch2 port2) t
	| _ -> failwith (Printf.sprintf "unimplemented composition pattern") in 
    match ptstar with
      | Star (Seq (p, t)) -> parse_links graph t
      | _ -> failwith "graph parsing assumes input is of the form (p;t)*"
end
  
module Verify = struct
  open Sat
  open SDN_Types
  open NetKAT_Types

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

  let serialize_header (header: header) : string = 
    match header with
      | Header InPort -> 
	"InPort"
      | Header EthSrc -> 
	"EthSrc"
      | Header EthDst -> 
	"EthDst"
      | Header EthType ->  
	"EthType"
      | Header Vlan ->  
	"Vlan"
      | Header VlanPcp ->
	"VlanPcp"
      | Header IPProto ->  
	"IPProto"
      | Header IP4Src ->  
	"IP4Src"
      | Header IP4Dst ->  
	"IP4Dst"
      | Header TCPSrcPort ->  
	"TCPSrcPort"
      | Header TCPDstPort ->  
	"TCPDstPort"
      | Switch -> 
	"Switch"
    

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
      (fun (pkt1: zVar) (pkt2: zVar) (except :header)  -> 
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
				  (ZAnd(l))) in
	      Hashtbl.add hash except new_except;
	      new_except), 
	  [TVar pkt1; TVar pkt2]))) in
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
      let packet_equals_fun = encode_packet_equals "x" "y" f in
      try ZTerm (Hashtbl.find hashmap packet_equals_fun)
      with Not_found -> 
	let macro = z3_macro ("mod_" ^ (serialize_header f)) [("x", SPacket); ("y", SPacket); ("v", SInt)] SBool 
	  (
	    ZAnd [
	      ZIf ((ZEquals (ZTerm (TVar "x"), nopacket)), 
		   (ZEquals (ZTerm (TVar "y"), nopacket)),
		   (ZNot (ZEquals (ZTerm (TVar "y"), nopacket))));
		  packet_equals_fun;
		  ZEquals(ZTerm (encode_header f "y"), ZTerm (TVar "v"))]) in
	Hashtbl.add hashmap packet_equals_fun macro; 
	ZTerm (Hashtbl.find hashmap packet_equals_fun) in	
    let reset_mod_fun () = Hashtbl.clear hashmap in
    mod_fun,reset_mod_fun

  let rec unzip_list_tuple (t : ('a * 'b) list) : ('a list * 'b list) = 
    match t with 
      | (hdl,hdr)::tl -> 
	let retl, retr = unzip_list_tuple tl in (hdl::retl), (hdr::retr)
      | [] -> ([],[])
	  

    
  let rec forwards_pol (forall_on : bool) (pol : policy) (inpkt : zVar) : zFormula * (zVar list) = 
    let forwards_pol = forwards_pol forall_on in
    let inpkt_t = ZTerm (TVar inpkt) in
    (* let nullinput = ZEquals (inpkt_t, nopacket) in *)
    match pol with 
      | Filter pred -> 
	ZOr[forwards_pred pred inpkt(*; nullinput*)], [inpkt]
      | Mod(f,v) -> 
	let outpkt = fresh SPacket in
	let outpkt_t = ZTerm (TVar outpkt) in
	let modfn = mod_fun f in
	ZOr [ZApp (modfn, [inpkt_t; outpkt_t; ZTerm (encode_vint v)])(*; nullinput*)], [outpkt]
      | Par (pol1, pol2) -> 
	let formu1, out1 = forwards_pol pol1 inpkt in
	let formu2, out2 = forwards_pol pol2 inpkt in
	if forall_on then
	  ZOr[formu1;formu2], out1@out2
	else
	  ZAnd[
	    ZOr[formu1; ZAnd (List.map (fun x -> ZEquals (ZTerm (TVar x), nopacket)) out1)];
	    ZOr[formu2; ZAnd (List.map (fun x -> ZEquals (ZTerm (TVar x), nopacket)) out2)]], out1@out2
      | Seq (pol1, pol2) -> 
	let formu', midpkts = forwards_pol pol1 inpkt in
	let outformu, outpkts = unzip_list_tuple (List.map (fun mpkt -> forwards_pol pol2 mpkt) midpkts) in
	ZAnd (formu'::outformu), List.flatten outpkts
      | Star _  -> failwith "NetKAT program not in form (p;t)*"
      | Choice _-> failwith "I'm not rightly sure what a \"choice\" is "
	
  let exists (list : zVar list) func : zFormula = 
    ZOr (List.map (fun pkt -> func pkt) list)

  let forall (list : zVar list) func : zFormula = 
    ZAnd (List.map (fun pkt -> func pkt) list)

  let packet_equals p1 p2 = ZEquals(ZTerm (TVar p1), ZTerm (TVar p2))

  let rec forwards_k forall p_t_star inpkt k : zFormula * (zVar list) = 
    let forwards_k = forwards_k forall in
    match p_t_star with 
      | Star (Seq (p, t)) -> 
	let pol_form, polout = forwards_pol forall p inpkt in
	let topo_form, topo_out = unzip_list_tuple (List.map (fun mpkt -> forwards_pol forall t mpkt) polout) in
	(match k with 
	  | 0 -> (if forall then ZNoop else ZTrue), [inpkt]
	  | 1 -> ZAnd[(ZComment ("forwards_k: pol_form",pol_form));
	       ZComment ("forwards_k: topo_form", ZAnd topo_form)], List.flatten topo_out
	  | _ -> 
	    let rest_of_links, final_out = unzip_list_tuple 
	      (List.map (fun x -> forwards_k p_t_star x (k-1)) (List.flatten topo_out)) in
	    ZAnd[(ZComment ("forwards_k: pol_form",pol_form));
		 ZComment ("forwards_k: topo_form", ZAnd topo_form);
		 ZComment ("forward_k_set: recur", ZAnd rest_of_links)], List.flatten final_out )
      | _ -> failwith "NetKAT program not in form (p;t)*"

  let forwards_star p_t_star inpkt k : zFormula * (zVar list) = 
    let forwards_k =  forwards_k false p_t_star inpkt  in
    let combine_results x = 
      let form,set = forwards_k x in
      let form = ZOr[form; ZAnd (List.map (fun x -> ZEquals (ZTerm (TVar x), nopacket)) set)] in
      ZComment ( Printf.sprintf "Attempting to forward in %u hops" x, form ), set in
    let forms, finalset = unzip_list_tuple ((List.map combine_results (range 0 k))) in
    ZAnd forms, List.flatten finalset

  let forwards_star_forall p_t_star inpkt k : zFormula * (zVar list) = 
    let forwards_k = forwards_k true p_t_star inpkt  in
    let combine_results x = 
      let form,set = forwards_k x in
      ZComment ( Printf.sprintf "Attempting to forward in %u hops" x, form ), set in
    let forms, finalset = unzip_list_tuple ((List.map combine_results (range 0 k))) in
    ZOr forms, List.flatten finalset
  
      
end


  let run_solve oko prog str : bool =
    let run_result = (
      match oko, Sat.solve prog with
	| Some (ok : bool), (sat : bool) ->
          if ok = sat then
            true
          else
            (Printf.printf "[Verify.check %s: expected %b got %b]\n%!" str ok sat; 
	     (let file = "debug-" ^ (to_string (gensym ())) ^ ".rkt" in
		(Printf.printf "Offending program is in %s.rkt\n" file;
		 let oc = open_out file in 
		 Printf.fprintf oc "%s\n" (Sat.serialize_program prog);
		 close_out oc));
	     false)
	| None, sat ->
          (Printf.printf "[Verify.check %s: %b]\n%!" str sat; false)) in
    Verify.reset_state (); Verify.reset_mod_fun (); run_result

  let combine_programs progs =
  Sat.ZProgram (List.flatten (List.map (fun prog -> match prog with
    | Sat.ZProgram (asserts) -> asserts) progs))

  let make_vint v = VInt.Int64 (Int64.of_int v)

(* str: name of your test (unique ID)
inp: initial packet
   pol: policy to test
outp: fully-transformed packet
oko: bool option. has to be Some. True if you think it should be satisfiable.
*)

  let check_reachability_k  k str inp pol outp oko =
  let x = Sat.fresh Sat.SPacket in
  let forwards_star_formula, forwards_star_result = Verify.forwards_star pol x k in
  let prog =     Sat.ZProgram [ 
    Sat.ZDeclareAssert (Verify.forwards_pred inp x)
    ; Sat.ZDeclareAssert forwards_star_formula
    ; Sat.ZDeclareAssert (Verify.exists forwards_star_result (fun y -> Verify.forwards_pred outp y))] in 
  run_solve oko prog str
    
  let check_equivalence_k str k1 pol1 k2 pol2 oko = 
    let x = Sat.fresh Sat.SPacket in
    let forwards_star_pol1_formula, forwards_star_pol1_result = Verify.forwards_star_forall pol1 x k1 in
    let forwards_star_pol2_formula, forwards_star_pol2_result = Verify.forwards_star_forall pol2 x k2 in
    let prog = Sat.ZProgram [
      Sat.ZDeclareAssert (Sat.ZNot (Sat.ZEquals(Sat.ZTerm (Sat.TVar x), Sat.Z3macro.nopacket)))
      ; Sat.ZDeclareAssert forwards_star_pol1_formula
      ; Sat.ZDeclareAssert forwards_star_pol2_formula
      ; Sat.ZDeclareAssert 
	(Verify.forall forwards_star_pol1_result 
	   (fun x -> Verify.exists forwards_star_pol2_result
	     (fun y -> 
	       Verify.packet_equals x y )))] in
      run_solve oko prog str

  let check_reachability str inp pol outp oko = 
    check_reachability_k (Verify_Graph.longest_shortest (Verify_Graph.parse_graph pol))
      str inp pol outp oko

  let check_equivalence str pol1 pol2 oko = 
    check_equivalence_k str 
      (Verify_Graph.longest_shortest (Verify_Graph.parse_graph pol1)) pol1
      (Verify_Graph.longest_shortest (Verify_Graph.parse_graph pol2)) pol2
      oko
    


  let check = check_reachability
    

