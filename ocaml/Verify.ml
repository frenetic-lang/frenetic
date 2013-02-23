open NetCore
open Sat

type link = Link of MessagesDef.switchId * Packet.portId

type topo = Topo of (link * link) list

let fresh_cell = ref 0

let fresh () = 
  let n = !fresh_cell in
  let () = incr fresh_cell in
  Z3Packet (Printf.sprintf "p%d" n)

let rec encode_predicate pred pkt =
  match pred with
    | All ->
      ZTrue
    | NoPackets -> 
      ZFalse
    | Not p1 -> 
      ZNot (encode_predicate p1 pkt)
    | And (p1, p2) ->
      ZAnd [encode_predicate p1 pkt; encode_predicate p2 pkt]
    | Or (p1, p2) -> 
      ZOr [encode_predicate p1 pkt; encode_predicate p2 pkt]
    | Switch sId -> 
      ZEquals (PktHeader ("Switch", pkt), Primitive sId)
    | InPort portId -> 
      ZEquals (PktHeader ("InPort", pkt), Primitive (Int64.of_int portId))
    | DlSrc n -> 
      ZEquals (PktHeader ("DlSrc", pkt), Primitive n)
    | DlDst n -> 
      ZEquals (PktHeader ("DlDst", pkt), Primitive n)

let equals fList pkt1 pkt2 = 
  let zList = 
    List.fold_left 
      (fun acc f -> ZEquals (PktHeader (f, pkt1), (PktHeader (f, pkt2))) :: acc)
      [] fList in 
  ZAnd zList

let forwards act pkt1 pkt2 =
  match act with
    | To pId ->
      ZAnd [ equals [ "Switch"; "DlSrc"; "DlDst" ] pkt1 pkt2
	   ; ZEquals (PktHeader ("InPort", pkt2), Primitive (Int64.of_int pId)) ]
    | ToAll -> 
      ZAnd [ equals [ "Switch"; "DlSrc"; "DlDst" ] pkt1 pkt2
	   ; ZNot (ZEquals (PktHeader ("InPort", pkt1), PktHeader ("InPort", pkt2))) ]
    | GetPacket gph -> 
      ZFalse

(*let topo_forwards topo pkt1 pkt2 =
  ZAnd [ equals ["DlSrc"; "DlDst"] pkt1 pkt2
       ; ZOr (List.fold_left (fun acc (Link(s1, p1), Link(s2, p2)) -> 
	 ZOr [ ZAnd [ ZEquals (PktHeader ("Switch", pkt1), Primitive s1)
		    ; ZEquals (PktHeader ("InPort", pkt1), Primitive (Int64.of_int p1))
		    ; ZEquals (PktHeader ("Switch", pkt2), Primitive s2)
		    ; ZEquals (PktHeader ("InPort", pkt2), Primitive (Int64.of_int p2)) ]
	     ; ZAnd [ ZEquals (PktHeader ("Switch", pkt1), Primitive s2)
		    ; ZEquals (PktHeader ("InPort", pkt1), Primitive (Int64.of_int p2))
		    ; ZEquals (PktHeader ("Switch", pkt2), Primitive s1)
		    ; ZEquals (PktHeader ("InPort", pkt2), Primitive (Int64.of_int p1)) ]]::acc) [] topo) ]*)

let topo_forwards topo pkt1 pkt2 =
  ZAnd [ equals ["DlSrc"; "DlDst"] pkt1 pkt2
       ; ZOr (List.fold_left (fun acc (Link(s1, p1), Link(s2, p2)) -> 
	 ZAnd [ ZEquals (PktHeader ("Switch", pkt1), Primitive s1)
	      ; ZEquals (PktHeader ("InPort", pkt1), Primitive (Int64.of_int p1))
              ; ZEquals (PktHeader ("Switch", pkt2), Primitive s2)
	      ; ZEquals (PktHeader ("InPort", pkt2), Primitive (Int64.of_int p2)) ]::acc) [] topo) ]

let rec encode_policy pol pkt1 pkt2 =
  match pol with
    | Pol (pred, actList) -> 
      ZAnd [encode_predicate pred pkt1
	   ; ZOr (List.fold_left (fun z act -> (forwards act pkt1 pkt2)::z) [] actList)]
    | Par (pol1, pol2) -> 
      ZOr [encode_policy pol1 pkt1 pkt2
	  ; encode_policy pol2 pkt1 pkt2]

let rec n_forwards n topo pol pkt1 pkt2 =
  match n with
    | 0 -> equals ["Switch"; "InPort"; "DlDst"; "DlSrc"] pkt1 pkt2
    | 1 -> ZOr [encode_policy pol pkt1 pkt2
	       ; equals ["Switch"; "InPort"; "DlDst"; "DlSrc"] pkt1 pkt2 ]
    | _ -> let pkt1' = fresh () in
	   let pkt1'' = fresh () in
	   ZAnd [ encode_policy pol pkt1 pkt1'
		;  topo_forwards topo pkt1' pkt1''
		; n_forwards (n-1) topo pol pkt1'' pkt2]
      
(* temporary front-end for verification stuff *)
let () = 
  let s1 = Int64.of_int 1 in
  let s2 = Int64.of_int 2 in
  let s3 = Int64.of_int 3 in
  let topo = [ (Link (s1, 4), Link (s3, 1)); (Link (s1,3), Link (s2, 1)); (Link (s3, 3), Link (s2, 2))
	     ; (Link (s3, 1), Link (s1, 4)); (Link (s2,1), Link (s1, 3)); (Link (s2, 2), Link (s3, 3))] in
  let pol = Pol (All, [ToAll]) in
  let pkt1 = fresh () in
  let pkt2 = fresh () in
  let phi = ZAnd [ ZEquals (PktHeader ("Switch", pkt1), Primitive s1)
		 ; ZEquals (PktHeader ("InPort", pkt1), Primitive (Int64.of_int 1))
		 ; ZEquals (PktHeader ("Switch", pkt2), Primitive s3)
		 ; ZEquals (PktHeader ("InPort", pkt2), Primitive (Int64.of_int 2)) 
		 ; n_forwards 3 topo pol pkt1 pkt2] in
  Printf.printf "%s\n" (solve phi)
