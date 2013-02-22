open NetCore
open Sat

let rec verify_predicate pred pkt =
  match pred with
    | All -> ZTrue
    | NoPackets -> ZFalse
    | Not p1 -> ZNot (verify_predicate p1 pkt)
    | And (p1, p2) -> ZAnd ((verify_predicate p1 pkt), (verify_predicate p2 pkt))
    | Or (p1, p2) -> ZOr ((verify_predicate p1 pkt), (verify_predicate p2 pkt))
    | Switch sId -> Equals (PktHeader ("Switch", pkt), Primitive sId)
    | InPort portId -> Equals (PktHeader ("InPort", pkt), Primitive (Int64.of_int portId))
    | DlSrc n -> Equals (PktHeader ("DlSrc", pkt), Primitive n)
    | DlDst n -> Equals (PktHeader ("DlDst", pkt), Primitive n)

let check_action act pkt1 pkt2 =
  match act with 
    | To pId -> Equals (PktHeader ("InPort", pkt2), Primitive (Int64.of_int pId))
    | ToAll -> failwith "not implemented"
    | GetPacket gph -> failwith "not implemented"

let check_all_actions actList pkt1 pkt2 =
  match actList with
    | [] -> failwith "empty action list"
    | h::[] -> check_action h pkt1 pkt2
    | h1::h2::t ->
      let check1 = check_action h1 pkt1 pkt2 in
      let check2 = check_action h2 pkt1 pkt2 in
      List.fold_left (fun x y -> ZOr (x, check_action y pkt1 pkt2)) (ZOr (check1, check2)) t

let compare_packets pkt1 pkt2 =
  let switchEq = Equals ((PktHeader ("Switch", pkt1)), (PktHeader ("Switch", pkt2))) in
  let dlSrcEq = Equals ((PktHeader ("DlSrc", pkt1)), (PktHeader ("DlSrc", pkt2))) in
  let dlDstEq = Equals ((PktHeader ("DlDst", pkt1)), (PktHeader ("DlDst", pkt2))) in
  ZAnd (switchEq, ZAnd (dlSrcEq, dlDstEq))

let rec verify_policy pol pkt1 pkt2 =
  match pol with
    | Pol (pred, actList) -> let predVerify = verify_predicate pred pkt1 in
			     let packetsEqual = compare_packets pkt1 pkt2 in
			     let actionVerify = check_all_actions actList pkt1 pkt2 in
			     ZAnd (predVerify, ZAnd (packetsEqual, actionVerify))
    | Par (pol1, pol2) -> ZOr ((verify_policy pol1 pkt1 pkt2), (verify_policy pol2 pkt1 pkt2))
      
(* temporary front-end for verification stuff *)
let () = 
  let pol1 = (Pol (InPort 6, [To 9])) in
  let pol2 = (Pol (All, [To 9])) in
  let z3Formula = verify_policy (Par (pol1, pol2)) (Z3Packet "pkt1") (Z3Packet "pkt2") in
  Printf.printf "%s\n"
    (solve (ZAnd (z3Formula, (verify_predicate (Not (InPort 9)) (Z3Packet "pkt2")))))
