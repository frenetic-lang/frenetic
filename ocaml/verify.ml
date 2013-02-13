open NetCore
open Sat

let rec verify_predicate pred pkt =
  match pred with
    | All -> ZTrue
    | NoPackets -> ZFalse
    | Not p1 -> ZNot (verify_peredicate p1 pkt)
    | And (p1, p2) -> ZAnd ((verify_predicate p1 pkt), (verify_predicate p2 pkt))
    | Or (p1, p2) -> ZOr ((verify_predicate p1 pkt), (verify_predicate p2 pkt))
    | Switch sId -> Equals ((PktHeader ("Switch", pkt)), Primitive sId)
    | InPort portId -> Equals ((PktHeader ("InPort", pkt)), Primitive portId)
    | DlSrc n -> Equals (PktHeader ("DlSrc", pkt), Primitive n) (*find out what fromIntegral does*)
    | DlDst n -> Equals ((PktHeader ("DlDst", pkt), Primitive n)
