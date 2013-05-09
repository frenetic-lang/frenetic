open Datatypes
open NetworkPacket
open OpenFlow0x01Types
open WordInterface

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
                 ptrnInPort : portId Wildcard.coq_Wildcard }

(** val pattern_rect :
    (dlAddr Wildcard.coq_Wildcard -> dlAddr Wildcard.coq_Wildcard -> dlTyp
    Wildcard.coq_Wildcard -> dlVlan Wildcard.coq_Wildcard -> dlVlanPcp
    Wildcard.coq_Wildcard -> nwAddr Wildcard.coq_Wildcard -> nwAddr
    Wildcard.coq_Wildcard -> nwProto Wildcard.coq_Wildcard -> nwTos
    Wildcard.coq_Wildcard -> tpPort Wildcard.coq_Wildcard -> tpPort
    Wildcard.coq_Wildcard -> portId Wildcard.coq_Wildcard -> 'a1) -> pattern
    -> 'a1 **)

let pattern_rect f p =
  let { ptrnDlSrc = x; ptrnDlDst = x0; ptrnDlType = x1; ptrnDlVlan = x2;
    ptrnDlVlanPcp = x3; ptrnNwSrc = x4; ptrnNwDst = x5; ptrnNwProto = x6;
    ptrnNwTos = x7; ptrnTpSrc = x8; ptrnTpDst = x9; ptrnInPort = x10 } = p
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

(** val pattern_rec :
    (dlAddr Wildcard.coq_Wildcard -> dlAddr Wildcard.coq_Wildcard -> dlTyp
    Wildcard.coq_Wildcard -> dlVlan Wildcard.coq_Wildcard -> dlVlanPcp
    Wildcard.coq_Wildcard -> nwAddr Wildcard.coq_Wildcard -> nwAddr
    Wildcard.coq_Wildcard -> nwProto Wildcard.coq_Wildcard -> nwTos
    Wildcard.coq_Wildcard -> tpPort Wildcard.coq_Wildcard -> tpPort
    Wildcard.coq_Wildcard -> portId Wildcard.coq_Wildcard -> 'a1) -> pattern
    -> 'a1 **)

let pattern_rec f p =
  let { ptrnDlSrc = x; ptrnDlDst = x0; ptrnDlType = x1; ptrnDlVlan = x2;
    ptrnDlVlanPcp = x3; ptrnNwSrc = x4; ptrnNwDst = x5; ptrnNwProto = x6;
    ptrnNwTos = x7; ptrnTpSrc = x8; ptrnTpDst = x9; ptrnInPort = x10 } = p
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

(** val ptrnDlSrc : pattern -> dlAddr Wildcard.coq_Wildcard **)

let ptrnDlSrc x = x.ptrnDlSrc

(** val ptrnDlDst : pattern -> dlAddr Wildcard.coq_Wildcard **)

let ptrnDlDst x = x.ptrnDlDst

(** val ptrnDlType : pattern -> dlTyp Wildcard.coq_Wildcard **)

let ptrnDlType x = x.ptrnDlType

(** val ptrnDlVlan : pattern -> dlVlan Wildcard.coq_Wildcard **)

let ptrnDlVlan x = x.ptrnDlVlan

(** val ptrnDlVlanPcp : pattern -> dlVlanPcp Wildcard.coq_Wildcard **)

let ptrnDlVlanPcp x = x.ptrnDlVlanPcp

(** val ptrnNwSrc : pattern -> nwAddr Wildcard.coq_Wildcard **)

let ptrnNwSrc x = x.ptrnNwSrc

(** val ptrnNwDst : pattern -> nwAddr Wildcard.coq_Wildcard **)

let ptrnNwDst x = x.ptrnNwDst

(** val ptrnNwProto : pattern -> nwProto Wildcard.coq_Wildcard **)

let ptrnNwProto x = x.ptrnNwProto

(** val ptrnNwTos : pattern -> nwTos Wildcard.coq_Wildcard **)

let ptrnNwTos x = x.ptrnNwTos

(** val ptrnTpSrc : pattern -> tpPort Wildcard.coq_Wildcard **)

let ptrnTpSrc x = x.ptrnTpSrc

(** val ptrnTpDst : pattern -> tpPort Wildcard.coq_Wildcard **)

let ptrnTpDst x = x.ptrnTpDst

(** val ptrnInPort : pattern -> portId Wildcard.coq_Wildcard **)

let ptrnInPort x = x.ptrnInPort

(** val eq_dec : pattern -> pattern -> bool **)

let eq_dec x y =
  let { ptrnDlSrc = x0; ptrnDlDst = x1; ptrnDlType = x2; ptrnDlVlan = x3;
    ptrnDlVlanPcp = x4; ptrnNwSrc = x5; ptrnNwDst = x6; ptrnNwProto = x7;
    ptrnNwTos = x8; ptrnTpSrc = x9; ptrnTpDst = x10; ptrnInPort = x11 } = x
  in
  let { ptrnDlSrc = ptrnDlSrc1; ptrnDlDst = ptrnDlDst1; ptrnDlType =
    ptrnDlType1; ptrnDlVlan = ptrnDlVlan1; ptrnDlVlanPcp = ptrnDlVlanPcp1;
    ptrnNwSrc = ptrnNwSrc1; ptrnNwDst = ptrnNwDst1; ptrnNwProto =
    ptrnNwProto1; ptrnNwTos = ptrnNwTos1; ptrnTpSrc = ptrnTpSrc1; ptrnTpDst =
    ptrnTpDst1; ptrnInPort = ptrnInPort1 } = y
  in
  if Wildcard.Wildcard.eq_dec Word48.eq_dec x0 ptrnDlSrc1
  then if Wildcard.Wildcard.eq_dec Word48.eq_dec x1 ptrnDlDst1
       then if Wildcard.Wildcard.eq_dec Word16.eq_dec x2 ptrnDlType1
            then if Wildcard.Wildcard.eq_dec Word16.eq_dec x3 ptrnDlVlan1
                 then if Wildcard.Wildcard.eq_dec Word8.eq_dec x4
                           ptrnDlVlanPcp1
                      then if Wildcard.Wildcard.eq_dec Word32.eq_dec x5
                                ptrnNwSrc1
                           then if Wildcard.Wildcard.eq_dec Word32.eq_dec x6
                                     ptrnNwDst1
                                then if Wildcard.Wildcard.eq_dec Word8.eq_dec
                                          x7 ptrnNwProto1
                                     then if Wildcard.Wildcard.eq_dec
                                               Word8.eq_dec x8 ptrnNwTos1
                                          then if Wildcard.Wildcard.eq_dec
                                                    Word16.eq_dec x9
                                                    ptrnTpSrc1
                                               then if Wildcard.Wildcard.eq_dec
                                                         Word16.eq_dec x10
                                                         ptrnTpDst1
                                                    then Wildcard.Wildcard.eq_dec
                                                           Word16.eq_dec x11
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

(** val coq_Wildcard_of_option :
    'a1 -> 'a1 option -> 'a1 Wildcard.coq_Wildcard **)

let coq_Wildcard_of_option def v =
  Wildcard.WildcardExact
    (match v with
     | Some v0 -> v0
     | None -> def)

(** val all : pattern **)

let all =
  { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
    ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan = Wildcard.WildcardAll;
    ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc = Wildcard.WildcardAll;
    ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto = Wildcard.WildcardAll;
    ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc = Wildcard.WildcardAll;
    ptrnTpDst = Wildcard.WildcardAll; ptrnInPort = Wildcard.WildcardAll }

(** val empty : pattern **)

let empty =
  { ptrnDlSrc = Wildcard.WildcardNone; ptrnDlDst = Wildcard.WildcardNone;
    ptrnDlType = Wildcard.WildcardNone; ptrnDlVlan = Wildcard.WildcardNone;
    ptrnDlVlanPcp = Wildcard.WildcardNone; ptrnNwSrc = Wildcard.WildcardNone;
    ptrnNwDst = Wildcard.WildcardNone; ptrnNwProto = Wildcard.WildcardNone;
    ptrnNwTos = Wildcard.WildcardNone; ptrnTpSrc = Wildcard.WildcardNone;
    ptrnTpDst = Wildcard.WildcardNone; ptrnInPort = Wildcard.WildcardNone }

(** val is_empty : pattern -> bool **)

let is_empty pat =
  let { ptrnDlSrc = dlSrc; ptrnDlDst = dlDst; ptrnDlType = typ; ptrnDlVlan =
    vlan; ptrnDlVlanPcp = pcp; ptrnNwSrc = nwSrc; ptrnNwDst = nwDst;
    ptrnNwProto = nwProto0; ptrnNwTos = nwTos0; ptrnTpSrc = tpSrc;
    ptrnTpDst = tpDst; ptrnInPort = inPort } = pat
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
                      ((||) (Wildcard.Wildcard.is_empty inPort)
                        (Wildcard.Wildcard.is_empty dlSrc))
                      (Wildcard.Wildcard.is_empty dlDst))
                    (Wildcard.Wildcard.is_empty vlan))
                  (Wildcard.Wildcard.is_empty pcp))
                (Wildcard.Wildcard.is_empty typ))
              (Wildcard.Wildcard.is_empty nwSrc))
            (Wildcard.Wildcard.is_empty nwDst))
          (Wildcard.Wildcard.is_empty nwTos0))
        (Wildcard.Wildcard.is_empty nwProto0))
      (Wildcard.Wildcard.is_empty tpSrc)) (Wildcard.Wildcard.is_empty tpDst)

(** val to_match : pattern -> of_match **)

let to_match pat =
  let { ptrnDlSrc = ptrnDlSrc0; ptrnDlDst = ptrnDlDst0; ptrnDlType =
    ptrnDlType0; ptrnDlVlan = ptrnDlVlan0; ptrnDlVlanPcp = ptrnDlVlanPcp0;
    ptrnNwSrc = ptrnNwSrc0; ptrnNwDst = ptrnNwDst0; ptrnNwProto =
    ptrnNwProto0; ptrnNwTos = ptrnNwTos0; ptrnTpSrc = ptrnTpSrc0; ptrnTpDst =
    ptrnTpDst0; ptrnInPort = ptrnInPort0 } = pat
  in
  { matchDlSrc = (Wildcard.Wildcard.to_option ptrnDlSrc0); matchDlDst =
  (Wildcard.Wildcard.to_option ptrnDlDst0); matchDlTyp =
  (Wildcard.Wildcard.to_option ptrnDlType0); matchDlVlan =
  (Wildcard.Wildcard.to_option ptrnDlVlan0); matchDlVlanPcp =
  (Wildcard.Wildcard.to_option ptrnDlVlanPcp0); matchNwSrc =
  (Wildcard.Wildcard.to_option ptrnNwSrc0); matchNwDst =
  (Wildcard.Wildcard.to_option ptrnNwDst0); matchNwProto =
  (Wildcard.Wildcard.to_option ptrnNwProto0); matchNwTos =
  (Wildcard.Wildcard.to_option ptrnNwTos0); matchTpSrc =
  (Wildcard.Wildcard.to_option ptrnTpSrc0); matchTpDst =
  (Wildcard.Wildcard.to_option ptrnTpDst0); matchInPort =
  (Wildcard.Wildcard.to_option ptrnInPort0) }

(** val inter : pattern -> pattern -> pattern **)

let inter p p' =
  let dlSrc = Wildcard.Wildcard.inter Word48.eq_dec p.ptrnDlSrc p'.ptrnDlSrc
  in
  let dlDst = Wildcard.Wildcard.inter Word48.eq_dec p.ptrnDlDst p'.ptrnDlDst
  in
  let dlType =
    Wildcard.Wildcard.inter Word16.eq_dec p.ptrnDlType p'.ptrnDlType
  in
  let dlVlan0 =
    Wildcard.Wildcard.inter Word16.eq_dec p.ptrnDlVlan p'.ptrnDlVlan
  in
  let dlVlanPcp0 =
    Wildcard.Wildcard.inter Word8.eq_dec p.ptrnDlVlanPcp p'.ptrnDlVlanPcp
  in
  let nwSrc = Wildcard.Wildcard.inter Word32.eq_dec p.ptrnNwSrc p'.ptrnNwSrc
  in
  let nwDst = Wildcard.Wildcard.inter Word32.eq_dec p.ptrnNwDst p'.ptrnNwDst
  in
  let nwProto0 =
    Wildcard.Wildcard.inter Word8.eq_dec p.ptrnNwProto p'.ptrnNwProto
  in
  let nwTos0 = Wildcard.Wildcard.inter Word8.eq_dec p.ptrnNwTos p'.ptrnNwTos
  in
  let tpSrc = Wildcard.Wildcard.inter Word16.eq_dec p.ptrnTpSrc p'.ptrnTpSrc
  in
  let tpDst = Wildcard.Wildcard.inter Word16.eq_dec p.ptrnTpDst p'.ptrnTpDst
  in
  let inPort =
    Wildcard.Wildcard.inter Word16.eq_dec p.ptrnInPort p'.ptrnInPort
  in
  { ptrnDlSrc = dlSrc; ptrnDlDst = dlDst; ptrnDlType = dlType; ptrnDlVlan =
  dlVlan0; ptrnDlVlanPcp = dlVlanPcp0; ptrnNwSrc = nwSrc; ptrnNwDst = nwDst;
  ptrnNwProto = nwProto0; ptrnNwTos = nwTos0; ptrnTpSrc = tpSrc; ptrnTpDst =
  tpDst; ptrnInPort = inPort }

(** val exact_pattern : packet -> Word16.t -> pattern **)

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

(** val match_packet : Word16.t -> packet -> pattern -> bool **)

let match_packet pt pk pat =
  negb (is_empty (inter (exact_pattern pk pt) pat))

(** val is_exact : pattern -> bool **)

let is_exact pat =
  let { ptrnDlSrc = dlSrc; ptrnDlDst = dlDst; ptrnDlType = typ; ptrnDlVlan =
    vlan; ptrnDlVlanPcp = pcp; ptrnNwSrc = nwSrc; ptrnNwDst = nwDst;
    ptrnNwProto = nwProto0; ptrnNwTos = nwTos0; ptrnTpSrc = tpSrc;
    ptrnTpDst = tpDst; ptrnInPort = inPort } = pat
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
                      ((&&) (Wildcard.Wildcard.is_exact inPort)
                        (Wildcard.Wildcard.is_exact dlSrc))
                      (Wildcard.Wildcard.is_exact dlDst))
                    (Wildcard.Wildcard.is_exact typ))
                  (Wildcard.Wildcard.is_exact vlan))
                (Wildcard.Wildcard.is_exact pcp))
              (Wildcard.Wildcard.is_exact nwSrc))
            (Wildcard.Wildcard.is_exact nwDst))
          (Wildcard.Wildcard.is_exact nwProto0))
        (Wildcard.Wildcard.is_exact nwTos0))
      (Wildcard.Wildcard.is_exact tpSrc)) (Wildcard.Wildcard.is_exact tpDst)

(** val coq_SupportedNwProto : int list **)

let coq_SupportedNwProto =
  coq_Const_0x6 :: (coq_Const_0x7 :: [])

(** val coq_SupportedDlTyp : int list **)

let coq_SupportedDlTyp =
  coq_Const_0x800 :: (coq_Const_0x806 :: [])

(** val to_valid : pattern -> pattern **)

let to_valid pat =
  let { ptrnDlSrc = dlSrc; ptrnDlDst = dlDst; ptrnDlType = dlTyp0;
    ptrnDlVlan = dlVlan0; ptrnDlVlanPcp = dlVlanPcp0; ptrnNwSrc = nwSrc;
    ptrnNwDst = nwDst; ptrnNwProto = nwProto0; ptrnNwTos = nwTos0;
    ptrnTpSrc = tpSrc; ptrnTpDst = tpDst; ptrnInPort = inPort } = pat
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
  { ptrnDlSrc = dlSrc; ptrnDlDst = dlDst; ptrnDlType = dlTyp0; ptrnDlVlan =
  dlVlan0; ptrnDlVlanPcp = dlVlanPcp0; ptrnNwSrc =
  (if validDlTyp then nwSrc else Wildcard.WildcardAll); ptrnNwDst =
  (if validDlTyp then nwDst else Wildcard.WildcardAll); ptrnNwProto =
  (if validDlTyp then nwProto0 else Wildcard.WildcardAll); ptrnNwTos =
  (if validDlTyp then nwTos0 else Wildcard.WildcardAll); ptrnTpSrc =
  (if validNwProto then tpSrc else Wildcard.WildcardAll); ptrnTpDst =
  (if validNwProto then tpDst else Wildcard.WildcardAll); ptrnInPort =
  inPort }

(** val to_all :
    'a1 Wildcard.coq_Wildcard -> bool -> 'a1 Wildcard.coq_Wildcard **)

let to_all w = function
| true -> Wildcard.WildcardAll
| false -> w

(** val mask : pattern -> pattern -> pattern **)

let mask pat1 pat2 =
  let { ptrnDlSrc = dlSrc; ptrnDlDst = dlDst; ptrnDlType = dlTyp0;
    ptrnDlVlan = dlVlan0; ptrnDlVlanPcp = dlVlanPcp0; ptrnNwSrc = nwSrc;
    ptrnNwDst = nwDst; ptrnNwProto = nwProto0; ptrnNwTos = nwTos0;
    ptrnTpSrc = tpSrc; ptrnTpDst = tpDst; ptrnInPort = inPort } = pat1
  in
  let { ptrnDlSrc = dlSrc'; ptrnDlDst = dlDst'; ptrnDlType = dlTyp';
    ptrnDlVlan = dlVlan'; ptrnDlVlanPcp = dlVlanPcp'; ptrnNwSrc = nwSrc';
    ptrnNwDst = nwDst'; ptrnNwProto = nwProto'; ptrnNwTos = nwTos';
    ptrnTpSrc = tpSrc'; ptrnTpDst = tpDst'; ptrnInPort = inPort' } = pat2
  in
  to_valid { ptrnDlSrc = (to_all dlSrc (Wildcard.Wildcard.is_exact dlSrc'));
    ptrnDlDst = (to_all dlDst (Wildcard.Wildcard.is_exact dlDst'));
    ptrnDlType = (to_all dlTyp0 (Wildcard.Wildcard.is_exact dlTyp'));
    ptrnDlVlan = (to_all dlVlan0 (Wildcard.Wildcard.is_exact dlVlan'));
    ptrnDlVlanPcp =
    (to_all dlVlanPcp0 (Wildcard.Wildcard.is_exact dlVlanPcp')); ptrnNwSrc =
    (to_all nwSrc (Wildcard.Wildcard.is_exact nwSrc')); ptrnNwDst =
    (to_all nwDst (Wildcard.Wildcard.is_exact nwDst')); ptrnNwProto =
    (to_all nwProto0 (Wildcard.Wildcard.is_exact nwProto')); ptrnNwTos =
    (to_all nwTos0 (Wildcard.Wildcard.is_exact nwTos')); ptrnTpSrc =
    (to_all tpSrc (Wildcard.Wildcard.is_exact tpSrc')); ptrnTpDst =
    (to_all tpDst (Wildcard.Wildcard.is_exact tpDst')); ptrnInPort =
    (to_all inPort (Wildcard.Wildcard.is_exact inPort')) }

