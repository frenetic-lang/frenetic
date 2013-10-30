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



  let reset () = 
    fresh_cell := []

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
	  | SPacket -> ";; declaring packet "^x^"\n" ^
	    "(declare-var "^x^" "^(serialize_sort s)^")" ^
	    "" ^ "(assert (not (= "^x^" nopacket))) " ^
	    (*"(declare-var "^x^"-int1 Int)" ^ 
	    "(declare-var "^x^"-int2 Int)" ^
	    "(declare-var "^x^"-int3 Int)" ^
	    "(declare-var "^x^"-int4 Int)" ^
	    "(declare-var "^x^"-int5 Int)" ^
	    "(declare-var "^x^"-int6 Int)" ^
	    "(declare-var "^x^"-int7 Int)" ^
	    "(declare-var "^x^"-int8 Int)" ^
	    "(declare-var "^x^"-int9 Int)" ^
	    "(declare-var "^x^"-int10 Int)" ^
	    "(declare-var "^x^"-int11 Int)" ^
	    "(declare-var "^x^"-int12 Int)" ^
	    "(assert (= "^x^"" ^
		"(packet "^x^"-int11 "^x^"-int10 "^x^"-int9" ^
		  " "^x^"-int8 "^x^"-int7 "^x^"-int6" ^
		  " "^x^"-int5 "^x^"-int12 "^x^"-int4" ^
	    " "^x^"-int3 "^x^"-int2 "^x^"-int1)))\n" ^ *)
	    ";; end declaration"
          | _ -> Printf.sprintf "(declare-var %s %s)" x (serialize_sort s)
	)
      | ZDeclareAssert(f) -> 
        Printf.sprintf "(assert %s)" (serialize_formula f)


  let define_z3_fun (name : string) (arglist : (zVar * zSort) list)  (rettype : zSort) (body : zFormula)  = 
    let args = List.map (fun (a, t) -> TVar a) arglist in
    let argtypes = List.map (fun (a, t) -> t) arglist in
    [ZDeclareVar (name, SFunction (argtypes, rettype)); ZDeclareAssert (ZForall (arglist, ZEquals (ZTerm (TApp ((TVar name), args)), body))) ]

  let foo = 4

  let define_z3_macro (name : string) (arglist : (zVar * zSort) list)  (rettype : zSort) (body : zFormula)  = 
    [ZDefineVar (name, SMacro (arglist, rettype), body)]


  let zApp x = (fun l -> ZTerm (TApp (x, l)))

  let z3_fun (name : string) (arglist : (zVar * zSort) list) (rettype : zSort) (body : zFormula) : zTerm = 
    let l = !fresh_cell in
    let name = name ^ "_" ^ to_string (gensym ()) in
    (match (define_z3_fun name arglist rettype body) with
      | [decl; def] -> fresh_cell := decl :: (l @ [ZToplevelComment "defined with z3_fun"; def])
      | _ -> assert false);
    TVar name

  let z3_macro (name : string) (arglist : (zVar * zSort) list) (rettype : zSort)(body : zFormula) : zTerm = 
    let l = !fresh_cell in
    let name = name ^ "_" ^ to_string (gensym ()) in
    fresh_cell := (define_z3_macro name arglist rettype body) @ l;
    TVar name

  let z3_macro_app (s : string) (vars : zVar list) (expr : zFormula) (args : zFormula list) = 
    let params = (List.map (fun v -> (v, SPacket)) vars) in
    let fun2map = (z3_fun ("forwards_pol_" ^ s) params SPacket expr) in
    (ZApp (ZTerm fun2map, args))
    
  let z3_map_expr (s : string) (vars : zVar list) (expr : zFormula) (lists : zTerm list) = 
    let params = (List.map (fun v -> (v, SPacket)) vars) in
    let fun2map = (z3_fun ("forwards_pol_" ^ s) params SPacket expr) in
    ZTerm (TApp ( (TApp (TVar "_", [TVar "map"; fun2map]  )), lists))


      
  module Z3macro = struct
    let x = (ZTerm (TVar "x")) 
    let y = (ZTerm (TVar "y")) 
    let s = (ZTerm (TVar "s")) 
    let s1 = (ZTerm (TVar "s1")) 
    let s2 = (ZTerm (TVar "s2")) 
    let set_empty = (ZTerm (TVar "set_empty"))
    let z3app2 f =
      (fun sp xp -> match sp, xp with 
	| (ZTerm (TVar s)), (ZTerm (TVar x)) -> ZTerm (TApp ((TVar f ), [(TVar s);(TVar x)]))
	| _ -> failwith "need to apply functions to variables as of right now.") 
    let z3app3 f = (fun sp xp yp -> match sp, xp, yp with 
      | (ZTerm (TVar s)), (ZTerm (TVar x)), (ZTerm (TVar y)) -> ZTerm (TApp ((TVar f ), [(TVar s);(TVar x); (TVar y)]))
      | _ -> failwith "need to apply functions to variables as of right now.") 
    let select = z3app2 "select" 
    let store = z3app3 "store" 
    let set_add = z3app2 "set_add"
    let rec set_add_chain set pkt_list = match pkt_list with
      | [] -> failwith "please use set_add_chain with non-empty list"
      | pkt::[] -> set_add set pkt
      | pkt::rest -> set_add (set_add_chain set rest) pkt      

    let set_diff = z3app2 "set_diff" 
    let map = (fun fp l1p l2p -> match fp, l1p, l2p with 
      | f, (ZTerm (TVar l1)), ZTerm (TVar l2) -> ZTerm (TApp ( (TApp (TVar "_", [TVar "map"; TVar f]  )), [(TVar l1); (TVar l2)]))
      | _ -> failwith "need to apply functions to variables as of right now.") 
    let nopacket = (ZTerm (TVar "nopacket"))  
    let set_empty = (ZTerm (TVar "set_empty")) 
  end
  open Z3macro

  let z3_static =

    (*some convenience functions to make z3 function definitions more readable*)

    (* actual z3 function definitions *)
    
    (define_z3_fun "packet_and"  [("x", SPacket); ("y", SPacket)] SPacket
       (ZIf (ZAnd 
	       [ZNot (ZEquals (x, nopacket)); 
		ZEquals (x, y)], 
	     x, nopacket))) @  
      
      define_z3_fun "packet_or" [("x", SPacket); ("y", SPacket)] SPacket
      (ZIf (ZEquals (x, nopacket), y, x)) @
      
      define_z3_fun "packet_diff" [("x", SPacket); ("y", SPacket)] SPacket
      (ZIf (ZEquals (x,y), nopacket, x)) @
      
      define_z3_macro "set_mem" [("x", SPacket); ("s", SSet)] SBool
      (ZNot (ZEquals ((select s x), nopacket))) @
      
      define_z3_macro "set_add" [("s", SSet); ("x", SPacket)] SSet 
      (store s x x) @
      
      define_z3_macro "set_inter" [("s1", SSet); ("s2", SSet)] SSet
      (map "packet_and" s1 s2) @
      
      define_z3_macro "set_union" [("s1", SSet); ("s2", SSet)] SSet
      (map "packet_or" s1 s2) @ 
      
      define_z3_macro "set_diff" [("s1", SSet); ("s2", SSet)] SSet 
      (map "packet_diff" s1 s2) @
      
      define_z3_macro "set_subseteq" [("s1", SSet); ("s2", SSet)] SBool 
      (ZEquals (set_empty, (set_diff s1 s2)))
      
  let pervasives : string = 
    "
(declare-datatypes 
 () 
 ((Packet 
   (nopacket )
   (packet 
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
    (InPort Int)))))" ^ "\n" ^ 
      "(define-sort Set () (Array Packet Packet))" ^ "\n (check-sat) \n"^ 
      "(define-fun set_empty () Set ((as const Set) nopacket))" ^ "\n" ^
      (intercalate serialize_declare "\n" z3_static)^ "\n;;end libraries\n\n\n;;begin code\n" 
      
      
  let serialize_program p : string = 
    let ZProgram(ds) = p in 
    let ds' = List.flatten [!fresh_cell; [ZToplevelComment("End Definitions, Commence SAT expressions\n")]; ds] in 
    Printf.sprintf "%s%s\n(check-sat)\n"
      pervasives (intercalate serialize_declare "\n" ds') 

  let solve prog : bool = 
    let s = serialize_program prog in 
    (* Printf.eprintf "%s" s; *)
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
    Printf.eprintf "%s\n%s" s (Buffer.contents b); 
    Buffer.contents b = "sat\nsat\n"
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
	| _ -> failwith (Printf.sprintf "unimplemented") in 
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

  let encode_header (header: header) (pkt:zVar) : zTerm =
    match header with
      | Header InPort -> 
        TApp (TVar "InPort", [TVar pkt])
      | Header EthSrc -> 
        TApp (TVar "EthSrc", [TVar pkt])
      | Header EthDst -> 
        TApp (TVar "EthDst", [TVar pkt])
      | Header EthType ->  
        TApp (TVar "EthType", [TVar pkt])
      | Header Vlan ->  
        TApp (TVar "Vlan", [TVar pkt])
      | Header VlanPcp ->
        TApp (TVar "VlanPcp", [TVar pkt])
      | Header IPProto ->  
        TApp (TVar "IPProto", [TVar pkt])
      | Header IP4Src ->  
        TApp (TVar "IP4Src", [TVar pkt])
      | Header IP4Dst ->  
        TApp (TVar "IP4Dst", [TVar pkt])
      | Header TCPSrcPort ->  
        TApp (TVar "TCPSrcPort", [TVar pkt])
      | Header TCPDstPort ->  
        TApp (TVar "TCPDstPort", [TVar pkt])
      | Switch -> 
        TApp (TVar "Switch", [TVar pkt])

  let encode_packet_equals, reset_state = 
    let encode_packet_equals_2 = 
      let hash = Hashtbl.create 0 in 
      (fun (reset : bool) (pkt1: zVar) (pkt2: zVar) (except :header)  -> 
	if reset then (Hashtbl.clear hash; (ZTerm (TVar "nopacket"))) else 
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
		let new_except = (z3_macro ("packet_equals_except_" ^ (*todo: how to serialize headers? serialize_ except*) "" ) 
				    [("x", SPacket);("y", SPacket)] SBool  
				    (ZAnd(l))) in
		Hashtbl.add hash except new_except;
		new_except), 
	    [TVar pkt1; TVar pkt2]))) in
    let encode_packet_equals = encode_packet_equals_2 false in
    let reset_state () = let _ = encode_packet_equals_2 true "" "" Switch in (); fresh_cell := [] in
    encode_packet_equals, reset_state

  let encode_vint (v: VInt.t): zTerm = 
    TInt (VInt.get_int64 v)

  
  let range = ( fun i j ->
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc) in
    aux j ([]) )

  let rec forwards_pred (pred : pred) (pkt : zVar) : zFormula = 
    match pred with
      | False -> 
	ZFalse
      | True -> 
	ZTrue
      | Test (hdr, v) -> 
	(ZEquals (ZTerm (encode_header hdr pkt), ZTerm (encode_vint v)))
      | Neg p ->
        (ZNot (forwards_pred p pkt))
      | And (pred1, pred2) -> 
	(ZAnd [forwards_pred pred1 pkt; 
	       forwards_pred pred2 pkt])
      | Or (pred1, pred2) -> 
	(ZOr [forwards_pred pred1 pkt;
              forwards_pred pred2 pkt])

  open Z3macro
  let rec forwards_pol (pol:policy) (inset:zVar) (inset_pkts : zVar list) (outset:zVar) : zFormula * (zVar list)  =
    let x = (ZTerm (TVar "x")) in
    let y = (ZTerm (TVar "y")) in
    (* let inset_f = (ZTerm (TVar outset)) in  *)
    let outset_f = (ZTerm (TVar outset)) in 
    let inset_f = (ZTerm (TVar inset)) in 

    let (formu : zFormula), outset_pkts = match pol with
      | Filter pred ->
	let expr_to_map = (ZIf (forwards_pred pred "x", x, nopacket)) in
	(* ZEquals(
	  (z3_map_expr "Filter"  ["x"] expr_to_map [(TVar inset)]), 
	  outset_f) *)
	let macro_to_map = z3_macro "Filter" [("x", SPacket)] SPacket expr_to_map in 
	let outset_pkts = List.map (fun _ -> fresh SPacket) inset_pkts in
	ZAnd (List.map2 (fun arg out ->  ZEquals (out, ZApp (ZTerm macro_to_map, [arg]))) 
		(List.map (fun t -> ZTerm (TVar t)) inset_pkts)
		(List.map (fun t -> ZTerm (TVar t)) outset_pkts)
	), outset_pkts
	  
      | Mod(f,v) -> 
	let expr_to_map = 
	  (ZIf 
	     ((ZAnd [
	       encode_packet_equals "x" "y" f;
	       ZEquals(ZTerm (encode_header f "y"), ZTerm (encode_vint v))]), 
	       (* then *) y, 
	       (* else *) nopacket)) in

(*	let map_result = 
	  (z3_map_expr "mod" ["x";"y"]  expr_to_map [TVar inset; TVar outset]) in
	ZEquals(map_result, outset_f) *)

	let macro_to_map = z3_macro "mod" [("x", SPacket); ("y", SPacket)] SPacket expr_to_map in
	let outset_pkts = List.map (fun _ -> fresh SPacket) inset_pkts in
	ZAnd (List.map2 (fun arg out ->  ZEquals (out, ZApp (ZTerm macro_to_map, [arg]))) 
		(List.map (fun t -> ZTerm (TVar t)) inset_pkts)
		(List.map (fun t -> ZTerm (TVar t)) outset_pkts)
	), outset_pkts

	  
      | Par(pol1,pol2) -> 
        let set1 = fresh SSet in 
        let set2 = fresh SSet in 
	let pol1form,pol1outs = forwards_pol pol1 inset inset_pkts set1 in
	let pol2form,pol2outs = forwards_pol pol2 inset inset_pkts set2 in
        (ZAnd[pol1form;
              pol2form;
              ZEquals(ZTerm (TApp(TVar "set_union", [TVar set1;
						     TVar set2])),
                      outset_f)]), pol1outs@pol2outs
      | Seq(pol1,pol2) -> 
	let set' = fresh SSet in
	let form,pkts' = forwards_pol pol1 inset inset_pkts set' in
	let fform, fpkts = forwards_pol pol2 set' pkts' outset in
	(ZAnd[form; fform]), fpkts

      | Star _  -> failwith "NetKAT program not in form (p;t)*"
      | Choice _-> failwith "I'm not rightly sure what a \"choice\" is "
    in
    ZAnd [ZEquals(set_add_chain set_empty (List.map (fun v -> ZTerm (TVar v)) inset_pkts), inset_f); 
	  
	  formu], outset_pkts

  let rec forwards_k p_t_star set1 pkts1 set2 k : zFormula * (zVar list) = 
    match p_t_star with
      | Star( Seq (p, t)) -> 
	if k = 0 then
	  ZEquals (ZTerm (TVar set1), ZTerm (TVar set2)), pkts1
	else
	  let set' = fresh SSet in 
	  let form, pkts' = forwards_k p_t_star set1 pkts1 set' (k-1) in
	  let set'' = fresh SSet in 
	  let form', pkts'' = forwards_pol p set' pkts' set'' in
	  let form'', pkts2 = forwards_pol t set'' pkts'' set2 in
	  ZAnd [form;form';form''], pkts2
      | _ -> failwith "NetKAT program not in form (p;t)*"

  let forwards_star p_t_star set1 pkts1 set2 k : zFormula = 
    let forwards_k = forwards_k p_t_star set1 pkts1 set2 in
    let combine_results x = 
      let form,pkts2 = forwards_k x in
      ZAnd[form;
	   ZEquals (set_add_chain set_empty (List.map (fun v -> ZTerm (TVar v)) pkts2), 
	   (ZTerm (TVar set2)))] in
    ZOr (List.map combine_results (range 0 k))

  open Sat.Z3macro
  let non_empty_set () =  
    let ret = fresh SSet in
    let s = ZTerm (TVar (ret)) in
    let assertvar = ZTerm (TVar (fresh SPacket)) in
    fresh_cell := !fresh_cell @ [(ZToplevelComment "asserting non-empty set");
				 (ZDeclareAssert (ZNot (ZEquals (select s assertvar, nopacket))))];
    ret

  let one_element_set elem = 
    let ret = fresh SSet in
    let s = ZTerm (TVar (ret)) in
    let assertvar = ZTerm (TVar (elem)) in 
    fresh_cell := !fresh_cell @ [(ZToplevelComment "creating one-element set");
				 (ZDeclareAssert (ZEquals (s, set_add set_empty assertvar)))]; ret
      
      

		
end


  let run_solve oko prog str : bool =
    let run_result = (
      match oko, Sat.solve prog with
	| Some (ok : bool), (sat : bool) ->
          if ok = sat then
            true
          else
            (Printf.printf "[Verify.check %s: expected %b got %b]\n%!" str ok sat; false)
	| None, sat ->
          (Printf.printf "[Verify.check %s: %b]\n%!" str sat; false)) in
    Verify.reset_state (); run_result

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
  let check_reachability  str inp pol outp oko =
  let k = Verify_Graph.longest_shortest (Verify_Graph.parse_graph pol) in
  let x = Sat.fresh Sat.SPacket in
  let xset = Verify.one_element_set x in
  let y = Sat.fresh Sat.SPacket in
  let yset = Verify.non_empty_set () in
  let prog =     Sat.ZProgram [ 
    Sat.ZDeclareAssert (Sat.Z3macro.select (Sat.ZTerm (Sat.TVar xset)) (Sat.ZTerm (Sat.TVar x))) 
    ; Sat.ZDeclareAssert (Verify.forwards_pred inp x)
    ; Sat.ZDeclareAssert (Verify.forwards_star pol xset [x] yset k )
    ; Sat.ZDeclareAssert (Verify.forwards_pred outp y)
    ; Sat.ZDeclareAssert (Sat.ZEquals ((Sat.ZTerm (Sat.TVar y)), Sat.Z3macro.select (Sat.ZTerm (Sat.TVar yset)) (Sat.ZTerm (Sat.TVar y))))] in
  run_solve oko prog str

  let check = check_reachability
