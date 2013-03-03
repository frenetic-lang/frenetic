open NetCore
open Topology
open Sat


(* JNF: note the to_int conversions may fail!
   Perhaps TInt should take an Int64? *)
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
      let atom = ZRelation(rel,[TPacket pkt]) in 
      let rules = ZRule(rel,[pkt],[atom1; atom2])::rules1@rules2 in 
      (atom, rules)
    | Or(pred1, pred2) -> 
      let atom1, rules1 = encode_predicate pred1 pkt in 
      let atom2, rules2 = encode_predicate pred1 pkt in 
      let rel = fresh (SRelation [SPacket]) in 
      let atom = ZRelation(rel, [TPacket pkt]) in
      let rule1 = ZRule(rel,[pkt],[atom1]) in 
      let rule2 = ZRule(rel,[pkt],[atom2]) in 
      let rules = rule1::rule2::rules1@rules2 in 
      (atom, rules)
    | DlSrc mac -> 
      (ZEquals (TFunction("DlSrc", [TPacket pkt]), TInt mac),[])
    | DlDst mac -> 
      (ZEquals (TFunction("DlDst", [TPacket pkt]), TInt mac),[])
    | SrcIP ip -> 
      (ZEquals (TFunction("DstIP", [TPacket pkt]), TInt (Int64.of_int32 ip)),[])
    | DstIP ip -> 
      (ZEquals (TFunction("SrcIP", [TPacket pkt]), TInt (Int64.of_int32 ip)),[])
    | TcpSrcPort port -> 
      (ZEquals (TFunction("TcpSrcPort", [TPacket pkt]), TInt (Int64.of_int port)),[])
    | TcpDstPort port -> 
      (ZEquals (TFunction("TcpDstPort", [TPacket pkt]), TInt (Int64.of_int port)),[])
    | InPort portId -> 
      (ZEquals (TFunction("InPort", [TPacket pkt]), TInt (Int64.of_int portId)), [])
    | Switch switchId -> 
      (ZEquals (TFunction("Switch", [TPacket pkt]), TInt switchId), [])

let equals fList pkt1 pkt2 = 
  assert false
  (* let zList =  *)
  (*   List.fold_left  *)
  (*     (fun acc f -> ZEquals (PktHeader (f, pkt1), (PktHeader (f, pkt2))) :: acc) *)
  (*     [] fList in  *)
  (* ZAnd zList *)

let forwards act pkt1 pkt2 =
  assert false
  (* match act with *)
  (*   | To pId -> *)
  (*     ZAnd [ equals [ "Switch"; "DlSrc"; "DlDst" ] pkt1 pkt2 *)
  (* 	   ; ZEquals (PktHeader ("InPort", pkt2), Primitive (Int64.of_int pId)) ] *)
  (*   | ToAll ->  *)
  (*     ZAnd [ equals [ "Switch"; "DlSrc"; "DlDst" ] pkt1 pkt2 *)
  (* 	   ; ZNot (ZEquals (PktHeader ("InPort", pkt1), PktHeader ("InPort", pkt2))) ] *)
  (*   | GetPacket gph ->  *)
  (*     ZFalse *)

let topo_forwards topo pkt1 pkt2 =
  assert false
  (* ZAnd [ equals ["DlSrc"; "DlDst"] pkt1 pkt2 *)
  (*      ; ZOr (List.fold_left (fun acc (Link(s1, p1), Link(s2, p2)) ->  *)
  (* 	 ZAnd [ ZEquals (PktHeader ("Switch", pkt1), Primitive s1) *)
  (* 	      ; ZEquals (PktHeader ("InPort", pkt1), Primitive (Int64.of_int p1)) *)
  (*             ; ZEquals (PktHeader ("Switch", pkt2), Primitive s2) *)
  (* 	      ; ZEquals (PktHeader ("InPort", pkt2), Primitive (Int64.of_int p2)) ]::acc) [] topo) ] *)

let rec encode_policy pol pkt1 pkt2 =
  assert false
  (* match pol with *)
  (*   | Pol (pred, actList) ->  *)
  (*     ZAnd [encode_predicate pred pkt1 *)
  (* 	   ; ZOr (List.fold_left (fun z act -> (forwards act pkt1 pkt2)::z) [] actList)] *)
  (*   | Par (pol1, pol2) ->  *)
  (*     ZOr [encode_policy pol1 pkt1 pkt2 *)
  (* 	  ; encode_policy pol2 pkt1 pkt2] *)

let rec n_forwards n topo pol pkt1 pkt2 =
  assert false
  (* match n with *)
  (*   | 0 -> equals ["Switch"; "InPort"; "DlDst"; "DlSrc"] pkt1 pkt2 *)
  (*   | 1 -> ZOr [encode_policy pol pkt1 pkt2 *)
  (* 	       ; equals ["Switch"; "InPort"; "DlDst"; "DlSrc"] pkt1 pkt2 ] *)
  (*   | _ -> let pkt1' = fresh () in *)
  (* 	   let pkt1'' = fresh () in *)
  (* 	   ZAnd [ encode_policy pol pkt1 pkt1' *)
  (* 		;  topo_forwards topo pkt1' pkt1'' *)
  (* 		; n_forwards (n-1) topo pol pkt1'' pkt2] *)
      
(* temporary front-end for verification stuff *)
let () = 
  let s1 = Int64.of_int 1 in
  let s2 = Int64.of_int 2 in
  let s3 = Int64.of_int 3 in
  let p1 = Int64.of_int 1 in 
  let p2 = Int64.of_int 2 in 
  let topo = 
    [ (Link (s1, 4), Link (s3, 1)); (Link (s1,3), Link (s2, 1)); 
      (Link (s3, 3), Link (s2, 2)) ] in
  let bidirectTopo = bidirectionalize topo in
  let pol = Pol (All, [ToAll]) in
  let pkt1 = fresh SPacket in
  let term1 = TPacket pkt1 in 
  let pkt2 = fresh SPacket in
  let term2 = TPacket pkt2 in
  let query = fresh (SRelation []) in 
  let rule = 
    ZRule (query, [],
	   [ ZEquals (TFunction ("Switch", [term1]), TInt s1)
	   ; ZEquals (TFunction ("InPort", [term1]), TInt p1)
	   ; ZEquals (TFunction ("Switch", [term2]), TInt s3)
	   ; ZEquals (TFunction ("InPort", [term2]), TInt p2)
	   ; ZRelation ("Forwards", [term1; term2]) ]) in 
  let program = ZProgram([rule], query) in 
  Printf.printf "%s\n" (solve program)
