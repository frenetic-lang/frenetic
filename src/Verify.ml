open NetCore.Syntax
open Sat


(* Topology *)
type link = Link of OpenFlow0x01.Types.switchId * Packet.Types.portId

type topology = Topology of (link * link) list

let reverse_edge (sp1, sp2) = (sp2, sp1)

let bidirectionalize (Topology topo) = 
  Topology 
    (List.fold_left 
       (fun acc edge -> edge::(reverse_edge edge)::acc) 
       [] topo)

let packet_field (field:string) (pkt:zVar) (num:zTerm) : zFormula = 
  ZEquals(TFunction(field, [TVar pkt]), num)

(* Predicates *)
let rec encode_predicate (pred:predicate) (pkt:zVar) : zFormula =
  match pred with
  | All -> 
    ZTrue
  | NoPackets ->
    ZFalse
  | Not pred1 ->
    ZNot(encode_predicate pred1 pkt)
  | And (pred1, pred2) ->
    ZAnd([encode_predicate pred1 pkt;
          encode_predicate pred2 pkt])
  | Or(pred1, pred2) -> 
    ZOr([encode_predicate pred1 pkt;
          encode_predicate pred2 pkt])
  | DlSrc mac -> 
    packet_field "DlSrc" pkt (TInt mac)
  | DlDst mac -> 
    packet_field "DlSrc" pkt (TInt mac)
  | DlVlan vlan -> 
    packet_field "DlVlan" pkt (TInt (Int64.of_int vlan))
  | SrcIP ip -> 
    packet_field "SrcIP" pkt (TInt (Int64.of_int32 ip))
  | DstIP ip -> 
    packet_field "DstIP" pkt (TInt (Int64.of_int32 ip))
  | TcpSrcPort port -> 
    packet_field "TcpSrcPort" pkt (TInt (Int64.of_int port))
  | TcpDstPort port -> 
    packet_field "TcpDstPort" pkt (TInt (Int64.of_int port))
  | InPort portId -> 
    packet_field "InPort" pkt (TInt (Int64.of_int portId))
  | Switch switchId -> 
    packet_field "Switch" pkt (TInt switchId)
          
let equal_field (field:string) (pkt1:zVar) (pkt2:zVar) : zFormula list = 
  let num = TVar (fresh SInt) in 
  [packet_field field pkt1 num; packet_field field pkt2 num]

let equals (fields:string list) (pkt1:zVar) (pkt2:zVar) : zFormula list = 
  List.fold_left (fun acc field -> equal_field field pkt1 pkt2 @ acc) [] fields

let action_forwards (act:action) (pkt1:zVar) (pkt2:zVar) : zFormula = 
  match act with 
  | To pId ->
    ZAnd(equals [ "Switch"; "DlSrc"; "DlDst" ] pkt1 pkt2 @ 
         [packet_field "InPort" pkt2 (TInt (Int64.of_int pId))])
  | ToAll ->
    let num1 = TVar (fresh SInt) in
    let num2 = TVar (fresh SInt) in
    ZAnd(equals [ "Switch"; "DlSrc"; "DlDst" ] pkt1 pkt2 @
           [packet_field "InPort" pkt1 num1;
            packet_field "InPort" pkt2 num2;
            ZNot (ZEquals (num1, num2))])
  | GetPacket gph ->
    ZFalse
  | _ -> 
    failwith "action_forwards: not yet implemented"

let topology_forwards (Topology topo:topology) (pkt1:zVar) (pkt2:zVar) : zFormula = 
  let eq = equals ["DlSrc"; "DlDst"] pkt1 pkt2 in 
  ZAnd (List.fold_left 
          (fun acc (Link(s1, p1), Link(s2, p2)) ->   
	    eq 
	    @ [ packet_field "Switch" pkt1 (TInt s1)
	      ; packet_field "InPort" pkt1 (TInt (Int64.of_int p1))
	      ; packet_field "Switch" pkt2 (TInt s2)
	      ; packet_field "InPort" pkt2 (TInt (Int64.of_int p2))]
            @ acc)
          [] topo)

let rec policy_forwards (pol:policy) (pkt1:zVar) (pkt2:zVar) : zFormula = 
  match pol with
  | Act (action) -> 
    action_forwards action pkt1 pkt2
  | Par (pol1, pol2) ->
    ZOr([ policy_forwards pol1 pkt1 pkt2
        ; policy_forwards pol2 pkt1 pkt2 ])
  | Seq (pol1, pol2) ->
    let pkt' = fresh SPacket in 
    ZAnd([policy_forwards pol1 pkt1 pkt';
          policy_forwards pol2 pkt' pkt2])
  | Filter pred ->
    let eq = equals ["Switch"; "InPort"; "DlSrc"; "DlDst"] pkt1 pkt2 in 
    ZAnd(encode_predicate pred pkt1::eq)
  | Empty -> 
    ZFalse
  | _ -> 
    failwith "policy_forwards: not yet implemented"

let rec forwards (pol:policy) (topo:topology) (k:int) (pkt1:zVar) (pkt2:zVar) : zFormula = 
  if k = 0 then 
    ZAnd(equals ["Switch"; "InPort"; "DlSrc"; "DlDst"] pkt1 pkt2)
  else
    let pkt' = fresh SPacket in 
    let pkt'' = fresh SPacket in 
    ZAnd([ policy_forwards pol pkt1 pkt'
         ; topology_forwards topo pkt' pkt''
         ; forwards pol topo (k-1) pkt'' pkt2])

(* temporary front-end for verification stuff *)
let () = 
  let s1 = Int64.of_int 1 in
  let s2 = Int64.of_int 2 in
  let s3 = Int64.of_int 3 in
  let topo = 
    bidirectionalize 
      (Topology
         [ (Link (s1, 2), Link (s3, 1)) 
         ; (Link (s1,3), Link (s2, 1))
	 ; (Link (s3, 3), Link (s2, 2)) ]) in 
  let pol = Seq (Filter All, Act ToAll) in
  let pkt1 = fresh SPacket in
  let pkt2 = fresh SPacket in
  let fwds = forwards pol topo 1 pkt1 pkt2 in 
  let program = ZProgram [ZAssertDeclare(fwds)] in 
  Printf.printf "%s\n" (solve program)
