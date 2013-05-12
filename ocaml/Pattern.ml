open Monad
open NetworkPacket
open OpenFlow0x01Types
open WordInterface

module type PORT = sig
  type t 
  
  val opt_portId : t -> portId option
 end

module type PATTERN = sig
  type port 
  
  type t 
  
  val inter : t -> t -> t
  
  val all : t
  
  val empty : t
  
  val exact_pattern : packet -> port -> t
  
  val is_empty : t -> bool
  
  val match_packet : port -> packet -> t -> bool
  
  val is_exact : t -> bool
  
  val to_match : t -> of_match option
  
  val beq : t -> t -> bool
  
  val dlSrc : dlAddr -> t
  
  val dlDst : dlAddr -> t
  
  val dlTyp : dlTyp -> t
  
  val dlVlan : dlVlan -> t
  
  val dlVlanPcp : dlVlanPcp -> t
  
  val ipSrc : nwAddr -> t
  
  val ipDst : nwAddr -> t
  
  val ipProto : nwProto -> t
  
  val inPort : port -> t
  
  val tcpSrcPort : tpPort -> t
  
  val tcpDstPort : tpPort -> t
  
  val udpSrcPort : tpPort -> t
  
  val udpDstPort : tpPort -> t
  
  val setDlSrc : dlAddr -> t -> t
  
  val setDlDst : dlAddr -> t -> t

  val to_string : t -> string
  
 end

module type MAKE = functor (Port:PORT) -> sig
  include PATTERN
end
  with type port = Port.t

module Make = 
 functor (Port:PORT) ->
 struct 
  type port = Port.t
  
  type pattern = { ptrnDlSrc : dlAddr Wildcard.coq_Wildcard;
                   ptrnDlDst : dlAddr Wildcard.coq_Wildcard;
                   ptrnDlType : dlTyp Wildcard.coq_Wildcard;
                   ptrnDlVlan : dlVlan Wildcard.coq_Wildcard;
                   ptrnDlVlanPcp : dlVlanPcp Wildcard.coq_Wildcard;
                   ptrnNwSrc : nwAddr Wildcard.coq_Wildcard;
                   ptrnNwDst : nwAddr Wildcard.coq_Wildcard;
                   ptrnNwProto : nwProto Wildcard.coq_Wildcard;
                   ptrnNwTos : nwTos Wildcard.coq_Wildcard;
                   ptrnTpSrc : tpPort Wildcard.coq_Wildcard;
                   ptrnTpDst : tpPort Wildcard.coq_Wildcard;
                   ptrnInPort : port Wildcard.coq_Wildcard }
  
 
  let ptrnDlSrc p = p.ptrnDlSrc
  
  let ptrnDlDst p = p.ptrnDlDst
  
  let ptrnDlType p = p.ptrnDlType
  
  let ptrnDlVlan p = p.ptrnDlVlan
  
  let ptrnDlVlanPcp p = p.ptrnDlVlanPcp
  
  let ptrnNwSrc p = p.ptrnNwSrc
  
  let ptrnNwDst p = p.ptrnNwDst
  
  let ptrnNwProto p = p.ptrnNwProto
  
  let ptrnNwTos p = p.ptrnNwTos
  
  let ptrnTpSrc p = p.ptrnTpSrc
  
  let ptrnTpDst p = p.ptrnTpDst
  
  let ptrnInPort p = p.ptrnInPort
  
  let eq_dec x y =
    let { ptrnDlSrc = x0; ptrnDlDst = x1; ptrnDlType = x2; ptrnDlVlan = x3;
      ptrnDlVlanPcp = x4; ptrnNwSrc = x5; ptrnNwDst = x6; ptrnNwProto = x7;
      ptrnNwTos = x8; ptrnTpSrc = x9; ptrnTpDst = x10; ptrnInPort = x11 } = x
    in
    let { ptrnDlSrc = ptrnDlSrc1; ptrnDlDst = ptrnDlDst1; ptrnDlType =
      ptrnDlType1; ptrnDlVlan = ptrnDlVlan1; ptrnDlVlanPcp = ptrnDlVlanPcp1;
      ptrnNwSrc = ptrnNwSrc1; ptrnNwDst = ptrnNwDst1; ptrnNwProto =
      ptrnNwProto1; ptrnNwTos = ptrnNwTos1; ptrnTpSrc = ptrnTpSrc1;
      ptrnTpDst = ptrnTpDst1; ptrnInPort = ptrnInPort1 } = y
    in
    if Wildcard.Wildcard.eq_dec Word48.eq_dec x0 ptrnDlSrc1
    then if Wildcard.Wildcard.eq_dec Word48.eq_dec x1 ptrnDlDst1
         then if Wildcard.Wildcard.eq_dec Word16.eq_dec x2 ptrnDlType1
              then if Wildcard.Wildcard.eq_dec Word16.eq_dec x3 ptrnDlVlan1
                   then if Wildcard.Wildcard.eq_dec Word8.eq_dec x4
                             ptrnDlVlanPcp1
                        then if Wildcard.Wildcard.eq_dec Word32.eq_dec x5
                                  ptrnNwSrc1
                             then if Wildcard.Wildcard.eq_dec Word32.eq_dec
                                       x6 ptrnNwDst1
                                  then if Wildcard.Wildcard.eq_dec
                                            Word8.eq_dec x7 ptrnNwProto1
                                       then if Wildcard.Wildcard.eq_dec
                                                 Word8.eq_dec x8 ptrnNwTos1
                                            then if Wildcard.Wildcard.eq_dec
                                                      Word16.eq_dec x9
                                                      ptrnTpSrc1
                                                 then if Wildcard.Wildcard.eq_dec
                                                           Word16.eq_dec x10
                                                           ptrnTpDst1
                                                      then Wildcard.Wildcard.eq_dec
                                                             (=) x11
                                                             ptrnInPort1
                                                      else false
                                                 else false
                                            else false
                                       else false
                                  else false
                             else false
                        else false
                   else false
              else false
         else false
    else false
  
  let all =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan = Wildcard.WildcardAll;
      ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc = Wildcard.WildcardAll;
      ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto = Wildcard.WildcardAll;
      ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc = Wildcard.WildcardAll;
      ptrnTpDst = Wildcard.WildcardAll; ptrnInPort = Wildcard.WildcardAll }
  
  let empty =
    { ptrnDlSrc = Wildcard.WildcardNone; ptrnDlDst = Wildcard.WildcardNone;
      ptrnDlType = Wildcard.WildcardNone; ptrnDlVlan = Wildcard.WildcardNone;
      ptrnDlVlanPcp = Wildcard.WildcardNone; ptrnNwSrc =
      Wildcard.WildcardNone; ptrnNwDst = Wildcard.WildcardNone; ptrnNwProto =
      Wildcard.WildcardNone; ptrnNwTos = Wildcard.WildcardNone; ptrnTpSrc =
      Wildcard.WildcardNone; ptrnTpDst = Wildcard.WildcardNone; ptrnInPort =
      Wildcard.WildcardNone }
  
  let is_empty pat =
    let { ptrnDlSrc = dlSrc0; ptrnDlDst = dlDst0; ptrnDlType = typ;
      ptrnDlVlan = vlan; ptrnDlVlanPcp = pcp; ptrnNwSrc = nwSrc; ptrnNwDst =
      nwDst; ptrnNwProto = nwProto0; ptrnNwTos = nwTos0; ptrnTpSrc = tpSrc;
      ptrnTpDst = tpDst; ptrnInPort = inPort0 } = pat
    in
    (||)
      ((||)
        ((||)
          ((||)
            ((||)
              ((||)
                ((||)
                  ((||)
                    ((||)
                      ((||)
                        ((||) (Wildcard.Wildcard.is_empty inPort0)
                          (Wildcard.Wildcard.is_empty dlSrc0))
                        (Wildcard.Wildcard.is_empty dlDst0))
                      (Wildcard.Wildcard.is_empty vlan))
                    (Wildcard.Wildcard.is_empty pcp))
                  (Wildcard.Wildcard.is_empty typ))
                (Wildcard.Wildcard.is_empty nwSrc))
              (Wildcard.Wildcard.is_empty nwDst))
            (Wildcard.Wildcard.is_empty nwTos0))
          (Wildcard.Wildcard.is_empty nwProto0))
        (Wildcard.Wildcard.is_empty tpSrc))
      (Wildcard.Wildcard.is_empty tpDst)
  
  let wild_to_opt = function
  | Wildcard.WildcardExact x -> Some (Some x)
  | Wildcard.WildcardAll -> Some None
  | Wildcard.WildcardNone -> None
  
  let to_match pat =
    let { ptrnDlSrc = dlSrc0; ptrnDlDst = dlDst0; ptrnDlType = typ;
      ptrnDlVlan = vlan; ptrnDlVlanPcp = pcp; ptrnNwSrc = nwSrc; ptrnNwDst =
      nwDst; ptrnNwProto = nwProto0; ptrnNwTos = nwTos0; ptrnTpSrc = tpSrc;
      ptrnTpDst = tpDst; ptrnInPort = pt } = pat
    in
    Maybe.bind (wild_to_opt dlSrc0) (fun dlSrc1 ->
      Maybe.bind (wild_to_opt dlDst0) (fun dlDst1 ->
        Maybe.bind (wild_to_opt typ) (fun typ0 ->
          Maybe.bind (wild_to_opt vlan) (fun vlan0 ->
            Maybe.bind (wild_to_opt pcp) (fun pcp0 ->
              Maybe.bind (wild_to_opt nwSrc) (fun nwSrc0 ->
                Maybe.bind (wild_to_opt nwDst) (fun nwDst0 ->
                  Maybe.bind (wild_to_opt nwProto0) (fun nwProto1 ->
                    Maybe.bind (wild_to_opt nwTos0) (fun nwTos1 ->
                      Maybe.bind (wild_to_opt tpSrc) (fun tpSrc0 ->
                        Maybe.bind (wild_to_opt tpDst) (fun tpDst0 ->
                          Maybe.bind (wild_to_opt pt) (fun pt0 ->
                            Maybe.bind
                              (match pt0 with
                               | Some pt1 ->
                                 (match Port.opt_portId pt1 with
                                  | Some phys -> Some (Some phys)
                                  | None -> None)
                               | None -> Some None) (fun pt1 ->
                              Maybe.ret { matchDlSrc = dlSrc1; matchDlDst =
                                dlDst1; matchDlTyp = typ0; matchDlVlan =
                                vlan0; matchDlVlanPcp = pcp0; matchNwSrc =
                                nwSrc0; matchNwDst = nwDst0; matchNwProto =
                                nwProto1; matchNwTos = nwTos1; matchTpSrc =
                                tpSrc0; matchTpDst = tpDst0; matchInPort =
                                pt1 })))))))))))))

    let inter p p' =
    let dlSrc0 =
      Wildcard.Wildcard.inter Word48.eq_dec (ptrnDlSrc p) (ptrnDlSrc p')
    in
    let dlDst0 =
      Wildcard.Wildcard.inter Word48.eq_dec (ptrnDlDst p) (ptrnDlDst p')
    in
    let dlType =
      Wildcard.Wildcard.inter Word16.eq_dec (ptrnDlType p) (ptrnDlType p')
    in
    let dlVlan0 =
      Wildcard.Wildcard.inter Word16.eq_dec (ptrnDlVlan p) (ptrnDlVlan p')
    in
    let dlVlanPcp0 =
      Wildcard.Wildcard.inter Word8.eq_dec (ptrnDlVlanPcp p)
        (ptrnDlVlanPcp p')
    in
    let nwSrc =
      Wildcard.Wildcard.inter Word32.eq_dec (ptrnNwSrc p) (ptrnNwSrc p')
    in
    let nwDst =
      Wildcard.Wildcard.inter Word32.eq_dec (ptrnNwDst p) (ptrnNwDst p')
    in
    let nwProto0 =
      Wildcard.Wildcard.inter Word8.eq_dec (ptrnNwProto p) (ptrnNwProto p')
    in
    let nwTos0 =
      Wildcard.Wildcard.inter Word8.eq_dec (ptrnNwTos p) (ptrnNwTos p')
    in
    let tpSrc =
      Wildcard.Wildcard.inter Word16.eq_dec (ptrnTpSrc p) (ptrnTpSrc p')
    in
    let tpDst =
      Wildcard.Wildcard.inter Word16.eq_dec (ptrnTpDst p) (ptrnTpDst p')
    in
    let inPort0 =
      Wildcard.Wildcard.inter (=) (ptrnInPort p) (ptrnInPort p')
    in
    { ptrnDlSrc = dlSrc0; ptrnDlDst = dlDst0; ptrnDlType = dlType;
    ptrnDlVlan = dlVlan0; ptrnDlVlanPcp = dlVlanPcp0; ptrnNwSrc = nwSrc;
    ptrnNwDst = nwDst; ptrnNwProto = nwProto0; ptrnNwTos = nwTos0;
    ptrnTpSrc = tpSrc; ptrnTpDst = tpDst; ptrnInPort = inPort0 }
  
  let exact_pattern pk pt =
    { ptrnDlSrc = (Wildcard.WildcardExact pk.pktDlSrc); ptrnDlDst =
      (Wildcard.WildcardExact pk.pktDlDst); ptrnDlType =
      (Wildcard.WildcardExact pk.pktDlTyp); ptrnDlVlan =
      (Wildcard.WildcardExact pk.pktDlVlan); ptrnDlVlanPcp =
      (Wildcard.WildcardExact pk.pktDlVlanPcp); ptrnNwSrc =
      (Wildcard.WildcardExact (pktNwSrc pk)); ptrnNwDst =
      (Wildcard.WildcardExact (pktNwDst pk)); ptrnNwProto =
      (Wildcard.WildcardExact (pktNwProto pk)); ptrnNwTos =
      (Wildcard.WildcardExact (pktNwTos pk)); ptrnTpSrc =
      (Wildcard.WildcardExact (pktTpSrc pk)); ptrnTpDst =
      (Wildcard.WildcardExact (pktTpDst pk)); ptrnInPort =
      (Wildcard.WildcardExact pt) }
  
  let match_packet pt pk pat =
    not (is_empty (inter (exact_pattern pk pt) pat))
  
  let is_exact pat =
    let { ptrnDlSrc = dlSrc0; ptrnDlDst = dlDst0; ptrnDlType = typ;
      ptrnDlVlan = vlan; ptrnDlVlanPcp = pcp; ptrnNwSrc = nwSrc; ptrnNwDst =
      nwDst; ptrnNwProto = nwProto0; ptrnNwTos = nwTos0; ptrnTpSrc = tpSrc;
      ptrnTpDst = tpDst; ptrnInPort = inPort0 } = pat
    in
    (&&)
      ((&&)
        ((&&)
          ((&&)
            ((&&)
              ((&&)
                ((&&)
                  ((&&)
                    ((&&)
                      ((&&)
                        ((&&) (Wildcard.Wildcard.is_exact inPort0)
                          (Wildcard.Wildcard.is_exact dlSrc0))
                        (Wildcard.Wildcard.is_exact dlDst0))
                      (Wildcard.Wildcard.is_exact typ))
                    (Wildcard.Wildcard.is_exact vlan))
                  (Wildcard.Wildcard.is_exact pcp))
                (Wildcard.Wildcard.is_exact nwSrc))
              (Wildcard.Wildcard.is_exact nwDst))
            (Wildcard.Wildcard.is_exact nwProto0))
          (Wildcard.Wildcard.is_exact nwTos0))
        (Wildcard.Wildcard.is_exact tpSrc))
      (Wildcard.Wildcard.is_exact tpDst)
  
  let coq_SupportedNwProto =
    coq_Const_0x6 :: (coq_Const_0x7 :: [])
  
  let coq_SupportedDlTyp =
    coq_Const_0x800 :: (coq_Const_0x806 :: [])
  
  let to_valid pat =
    let { ptrnDlSrc = dlSrc0; ptrnDlDst = dlDst0; ptrnDlType = dlTyp0;
      ptrnDlVlan = dlVlan0; ptrnDlVlanPcp = dlVlanPcp0; ptrnNwSrc = nwSrc;
      ptrnNwDst = nwDst; ptrnNwProto = nwProto0; ptrnNwTos = nwTos0;
      ptrnTpSrc = tpSrc; ptrnTpDst = tpDst; ptrnInPort = inPort0 } = pat
    in
    let validDlTyp =
      match dlTyp0 with
      | Wildcard.WildcardExact n ->
        if Word16.eq_dec n coq_Const_0x800
        then true
        else if Word16.eq_dec n coq_Const_0x806 then true else false
      | _ -> false
    in
    let validNwProto =
      match nwProto0 with
      | Wildcard.WildcardExact n ->
        if Word8.eq_dec n coq_Const_0x6
        then true
        else if Word8.eq_dec n coq_Const_0x7 then true else false
      | _ -> false
    in
    { ptrnDlSrc = dlSrc0; ptrnDlDst = dlDst0; ptrnDlType = dlTyp0;
    ptrnDlVlan = dlVlan0; ptrnDlVlanPcp = dlVlanPcp0; ptrnNwSrc =
    (if validDlTyp then nwSrc else Wildcard.WildcardAll); ptrnNwDst =
    (if validDlTyp then nwDst else Wildcard.WildcardAll); ptrnNwProto =
    (if validDlTyp then nwProto0 else Wildcard.WildcardAll); ptrnNwTos =
    (if validDlTyp then nwTos0 else Wildcard.WildcardAll); ptrnTpSrc =
    (if validNwProto then tpSrc else Wildcard.WildcardAll); ptrnTpDst =
    (if validNwProto then tpDst else Wildcard.WildcardAll); ptrnInPort =
    inPort0 }
  
  let to_all w = function
  | true -> Wildcard.WildcardAll
  | false -> w
  
  let setDlSrc dlSrc0 pat =
    let { ptrnDlSrc = ptrnDlSrc0; ptrnDlDst = dlDst0; ptrnDlType = dlTyp0;
      ptrnDlVlan = dlVlan0; ptrnDlVlanPcp = dlVlanPcp0; ptrnNwSrc = nwSrc;
      ptrnNwDst = nwDst; ptrnNwProto = nwProto0; ptrnNwTos = nwTos0;
      ptrnTpSrc = tpSrc; ptrnTpDst = tpDst; ptrnInPort = inPort0 } = pat
    in
    to_valid { ptrnDlSrc = (Wildcard.WildcardExact dlSrc0); ptrnDlDst =
      dlDst0; ptrnDlType = dlTyp0; ptrnDlVlan = dlVlan0; ptrnDlVlanPcp =
      dlVlanPcp0; ptrnNwSrc = nwSrc; ptrnNwDst = nwDst; ptrnNwProto =
      nwProto0; ptrnNwTos = nwTos0; ptrnTpSrc = tpSrc; ptrnTpDst = tpDst;
      ptrnInPort = inPort0 }
  
  let setDlDst dlDst0 pat =
    let { ptrnDlSrc = dlSrc0; ptrnDlDst = ptrnDlDst0; ptrnDlType = dlTyp0;
      ptrnDlVlan = dlVlan0; ptrnDlVlanPcp = dlVlanPcp0; ptrnNwSrc = nwSrc;
      ptrnNwDst = nwDst; ptrnNwProto = nwProto0; ptrnNwTos = nwTos0;
      ptrnTpSrc = tpSrc; ptrnTpDst = tpDst; ptrnInPort = inPort0 } = pat
    in
    to_valid { ptrnDlSrc = dlSrc0; ptrnDlDst = (Wildcard.WildcardExact
      dlDst0); ptrnDlType = dlTyp0; ptrnDlVlan = dlVlan0; ptrnDlVlanPcp =
      dlVlanPcp0; ptrnNwSrc = nwSrc; ptrnNwDst = nwDst; ptrnNwProto =
      nwProto0; ptrnNwTos = nwTos0; ptrnTpSrc = tpSrc; ptrnTpDst = tpDst;
      ptrnInPort = inPort0 }
  
  type t = pattern
  
  let beq p1 p2 =
    if eq_dec p1 p2 then true else false
  
  let inPort pt =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan = Wildcard.WildcardAll;
      ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc = Wildcard.WildcardAll;
      ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto = Wildcard.WildcardAll;
      ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc = Wildcard.WildcardAll;
      ptrnTpDst = Wildcard.WildcardAll; ptrnInPort = (Wildcard.WildcardExact
      pt) }
  
  let dlSrc dlAddr0 =
    { ptrnDlSrc = (Wildcard.WildcardExact dlAddr0); ptrnDlDst =
      Wildcard.WildcardAll; ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc =
      Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll; ptrnInPort =
      Wildcard.WildcardAll }
  
  let dlDst dlAddr0 =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = (Wildcard.WildcardExact
      dlAddr0); ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc =
      Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll; ptrnInPort =
      Wildcard.WildcardAll }
  
  let dlTyp typ =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact typ); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc =
      Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll; ptrnInPort =
      Wildcard.WildcardAll }
  
  let dlVlan vlan =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan = (Wildcard.WildcardExact
      vlan); ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc =
      Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll; ptrnInPort =
      Wildcard.WildcardAll }
  
  let dlVlanPcp pcp =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan = Wildcard.WildcardAll;
      ptrnDlVlanPcp = (Wildcard.WildcardExact pcp); ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc =
      Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll; ptrnInPort =
      Wildcard.WildcardAll }
  
  let ipSrc addr =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact coq_Const_0x800); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      (Wildcard.WildcardExact addr); ptrnNwDst = Wildcard.WildcardAll;
      ptrnNwProto = Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll;
      ptrnTpSrc = Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll;
      ptrnInPort = Wildcard.WildcardAll }
  
  let ipDst addr =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact coq_Const_0x800); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = (Wildcard.WildcardExact addr);
      ptrnNwProto = Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll;
      ptrnTpSrc = Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll;
      ptrnInPort = Wildcard.WildcardAll }
  
  let ipProto proto =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact coq_Const_0x800); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      (Wildcard.WildcardExact proto); ptrnNwTos = Wildcard.WildcardAll;
      ptrnTpSrc = Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll;
      ptrnInPort = Wildcard.WildcardAll }
  
  let tpSrcPort proto tpPort0 =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact coq_Const_0x800); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      (Wildcard.WildcardExact proto); ptrnNwTos = Wildcard.WildcardAll;
      ptrnTpSrc = (Wildcard.WildcardExact tpPort0); ptrnTpDst =
      Wildcard.WildcardAll; ptrnInPort = Wildcard.WildcardAll }
  
  let tpDstPort proto tpPort0 =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact coq_Const_0x800); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      (Wildcard.WildcardExact proto); ptrnNwTos = Wildcard.WildcardAll;
      ptrnTpSrc = Wildcard.WildcardAll; ptrnTpDst = (Wildcard.WildcardExact
      tpPort0); ptrnInPort = Wildcard.WildcardAll }
  
  let tcpSrcPort = tpSrcPort coq_Const_0x6
  
  let tcpDstPort = tpDstPort coq_Const_0x6
  
  let udpSrcPort = tpSrcPort coq_Const_0x7
  
  let udpDstPort = tpDstPort coq_Const_0x7

  let to_string p = failwith "NYI: Pattern.to_string."

 end

