open NetCore
open Topology
open Sat

let rec encode_predicate (pred:predicate) (pkt:zVar) : zAtom * zRule list =
  match pred with
  | All ->
    (ZTrue,[])
  | NoPackets ->
    (ZFalse,[])
  | Not pred1 ->
    let atom, rules = encode_predicate pred1 pkt in 
    (ZNot (atom), rules)  
  | And (pred1, pred2) ->
    let atom1, rules1 = encode_predicate pred1 pkt in 
    let atom2, rules2 = encode_predicate pred1 pkt in 
    let rel = fresh (SRelation [SPacket]) in 
    let atom = ZRelation(rel,[TVar pkt]) in 
    let rules = ZRule(rel,[pkt],[atom1; atom2])::rules1@rules2 in 
    (atom, rules)
  | Or(pred1, pred2) -> 
    let atom1, rules1 = encode_predicate pred1 pkt in 
    let atom2, rules2 = encode_predicate pred1 pkt in 
    let rel = fresh (SRelation [SPacket]) in 
    let atom = ZRelation(rel, [TVar pkt]) in
    let rule1 = ZRule(rel,[pkt],[atom1]) in 
    let rule2 = ZRule(rel,[pkt],[atom2]) in 
    let rules = rule1::rule2::rules1@rules2 in 
    (atom, rules)
  | DlSrc mac -> 
    (ZEquals (TFunction("DlSrc", [TVar pkt]), TInt mac),[])
  | DlDst mac -> 
    (ZEquals (TFunction("DlDst", [TVar pkt]), TInt mac),[])
  | SrcIP ip -> 
    (ZEquals (TFunction("DstIP", [TVar pkt]), TInt (Int64.of_int32 ip)),[])
  | DstIP ip -> 
    (ZEquals (TFunction("SrcIP", [TVar pkt]), TInt (Int64.of_int32 ip)),[])
  | TcpSrcPort port -> 
    (ZEquals (TFunction("TcpSrcPort", [TVar pkt]), TInt (Int64.of_int port)),[])
  | TcpDstPort port -> 
    (ZEquals (TFunction("TcpDstPort", [TVar pkt]), TInt (Int64.of_int port)),[])
  | InPort portId -> 
    (ZEquals (TFunction("InPort", [TVar pkt]), TInt (Int64.of_int portId)), [])
  | Switch switchId -> 
    (ZEquals (TFunction("Switch", [TVar pkt]), TInt switchId), [])
      
let equals (fields:string list) (pkt1:zVar) (pkt2:zVar) : zAtom list = 
  List.map (fun field -> ZEquals (TFunction(field, [TVar pkt1]), 
				  TFunction(field, [TVar pkt2]))) fields

let action_forwards (act:action) (pkt1:zVar) (pkt2:zVar) : zAtom list = 
  match act with 
  | To pId ->
      equals [ "Switch"; "DlSrc"; "DlDst" ] pkt1 pkt2 @ 
      [ZEquals (TFunction ("InPort", [TVar pkt2]), TInt (Int64.of_int pId))]
  | ToAll ->
    equals [ "Switch"; "DlSrc"; "DlDst" ] pkt1 pkt2 @
      [ ZNot (ZEquals (TFunction ("InPort", [TVar pkt1]), 
		       TFunction ("InPort", [TVar pkt2])))]
  | GetPacket gph ->
    [ZFalse]

let topology_forwards (Topology topo:topology) (rel:zVar) (pkt1:zVar) (pkt2:zVar) : zRule list = 
  let eq = equals ["DlSrc"; "DlDst"] pkt1 pkt2 in 
  List.map 
    (fun (Link(s1, p1), Link(s2, p2)) ->   
      let body = 
	eq @ 
	  [ ZEquals (TFunction("Switch",[TVar pkt1]), TInt s1)
	  ; ZEquals (TFunction("InPort",[TVar pkt1]), TInt (Int64.of_int p1))
	  ; ZEquals (TFunction("Switch",[TVar pkt2]), TInt s2)
	  ; ZEquals (TFunction("InPort",[TVar pkt2]), TInt (Int64.of_int p2)) ] in 
      ZRule(rel,[pkt1;pkt2], body))
    topo  

let rec policy_forwards (pol:policy) (rel:zVar) (pkt1:zVar) (pkt2:zVar) : zRule list = 
  match pol with
  | Pol (pred, actions) ->
    let pred_atom, pred_rules = encode_predicate pred pkt1 in 
    List.fold_left  
      (fun acc action -> 
	let atoms = pred_atom :: action_forwards action pkt1 pkt2 in 
	let rule = ZRule(rel,[pkt1;pkt2], atoms) in 
	let acc' = rule::acc in 
	acc')
      pred_rules actions 
  | Par (pol1, pol2) ->
    policy_forwards pol1 rel pkt1 pkt2 @ policy_forwards pol2 rel pkt1 pkt2 
  | _ -> 
    assert false

let forwards (pol:policy) (topo:topology) : zVar * zRule list = 
  let p = fresh (SRelation [SPacket; SPacket]) in 
  let t = fresh (SRelation [SPacket; SPacket]) in 
  let f = fresh (SRelation [SPacket; SPacket]) in 
  let pkt1 = fresh SPacket in 
  let pkt2 = fresh SPacket in 
  let pkt3 = fresh SPacket in
  let pkt4 = fresh SPacket in
  let policy_rules = policy_forwards pol p pkt1 pkt2 in 
  let topology_rules = topology_forwards topo t pkt1 pkt2 in 
  let forwards_rules = 
    [ ZRule(f,[pkt1;pkt2],[ZRelation(p,[TVar pkt1; TVar pkt2])])
    ; ZRule(f,[pkt1;pkt2],[ ZRelation(p,[TVar pkt1; TVar pkt3])
			  ; ZRelation(t,[TVar pkt3; TVar pkt4])
			  ; ZRelation(f,[TVar pkt4; TVar pkt2])]) ] in 
  (f, policy_rules @ topology_rules @ forwards_rules)

(* temporary front-end for verification stuff *)
let () = 
  let s1 = Int64.of_int 1 in
  let s2 = Int64.of_int 2 in
  let s3 = Int64.of_int 3 in
  let p1 = Int64.of_int 1 in 
  let p2 = Int64.of_int 2 in 
  let topo = 
    bidirectionalize 
      (Topology 
	 [ (Link (s1, 4), Link (s3, 1)); (Link (s1,3), Link (s2, 1)); 
	   (Link (s3, 3), Link (s2, 2)) ]) in 
  let pol = Pol (All, [ToAll]) in
  let pkt1 = fresh SPacket in
  let pkt2 = fresh SPacket in
  let query = fresh (SRelation []) in 
  let fwds, rules = forwards pol topo in 
  let program = 
    ZProgram
      (ZRule (query, [],
	      [ ZEquals (TFunction ("Switch", [TVar pkt1]), TInt s1)
	      ; ZEquals (TFunction ("InPort", [TVar pkt1]), TInt p1)
	      ; ZEquals (TFunction ("Switch", [TVar pkt2]), TInt s3)
	      ; ZEquals (TFunction ("InPort", [TVar pkt2]), TInt p2)
	      ; ZRelation (fwds, [TVar pkt1; TVar pkt2])]) :: rules, 
       query) in 
  Printf.printf "%s\n" (solve program)
