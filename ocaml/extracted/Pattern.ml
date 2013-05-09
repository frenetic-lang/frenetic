open NetworkPacket
open OpenFlow0x01Types
open PatternImplDef
open WordInterface

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module Pattern = 
 struct 
  type pat =
    pattern
    (* singleton inductive, whose constructor was Pat *)
  
  (** val pat_rect : (pattern -> __ -> 'a1) -> pat -> 'a1 **)
  
  let pat_rect f p =
    f p __
  
  (** val pat_rec : (pattern -> __ -> 'a1) -> pat -> 'a1 **)
  
  let pat_rec f p =
    f p __
  
  (** val raw : pat -> pattern **)
  
  let raw p =
    p
  
  type t = pat
  
  (** val beq : t -> t -> bool **)
  
  let beq p1 p2 =
    if eq_dec (raw p1) (raw p2) then true else false
  
  (** val inter : t -> t -> pat **)
  
  let inter p1 p2 =
    inter (raw p1) (raw p2)
  
  (** val mask : t -> t -> t **)
  
  let mask p1 p2 =
    mask (raw p1) (raw p2)
  
  (** val all : t **)
  
  let all =
    all
  
  (** val empty : t **)
  
  let empty =
    empty
  
  (** val exact_pattern : packet -> Word16.t -> t **)
  
  let exact_pattern pk pt =
    exact_pattern pk pt
  
  (** val is_empty : pat -> bool **)
  
  let is_empty pat0 =
    is_empty (raw pat0)
  
  (** val match_packet : Word16.t -> packet -> pat -> bool **)
  
  let match_packet pt pk pat0 =
    match_packet pt pk (raw pat0)
  
  (** val is_exact : pat -> bool **)
  
  let is_exact pat0 =
    is_exact (raw pat0)
  
  (** val to_match : pat -> of_match **)
  
  let to_match pat0 =
    to_match (raw pat0)
  
  (** val inPort : portId -> t **)
  
  let inPort pt =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan = Wildcard.WildcardAll;
      ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc = Wildcard.WildcardAll;
      ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto = Wildcard.WildcardAll;
      ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc = Wildcard.WildcardAll;
      ptrnTpDst = Wildcard.WildcardAll; ptrnInPort = (Wildcard.WildcardExact
      pt) }
  
  (** val dlSrc : dlAddr -> t **)
  
  let dlSrc dlAddr0 =
    { ptrnDlSrc = (Wildcard.WildcardExact dlAddr0); ptrnDlDst =
      Wildcard.WildcardAll; ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc =
      Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll; ptrnInPort =
      Wildcard.WildcardAll }
  
  (** val dlDst : dlAddr -> t **)
  
  let dlDst dlAddr0 =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = (Wildcard.WildcardExact
      dlAddr0); ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc =
      Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll; ptrnInPort =
      Wildcard.WildcardAll }
  
  (** val dlTyp : dlTyp -> t **)
  
  let dlTyp typ =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact typ); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc =
      Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll; ptrnInPort =
      Wildcard.WildcardAll }
  
  (** val dlVlan : dlVlan -> t **)
  
  let dlVlan vlan =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan = (Wildcard.WildcardExact
      vlan); ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc =
      Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll; ptrnInPort =
      Wildcard.WildcardAll }
  
  (** val dlVlanPcp : dlVlanPcp -> t **)
  
  let dlVlanPcp pcp =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = Wildcard.WildcardAll; ptrnDlVlan = Wildcard.WildcardAll;
      ptrnDlVlanPcp = (Wildcard.WildcardExact pcp); ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll; ptrnTpSrc =
      Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll; ptrnInPort =
      Wildcard.WildcardAll }
  
  (** val ipSrc : nwAddr -> t **)
  
  let ipSrc addr =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact coq_Const_0x800); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      (Wildcard.WildcardExact addr); ptrnNwDst = Wildcard.WildcardAll;
      ptrnNwProto = Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll;
      ptrnTpSrc = Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll;
      ptrnInPort = Wildcard.WildcardAll }
  
  (** val ipDst : nwAddr -> t **)
  
  let ipDst addr =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact coq_Const_0x800); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = (Wildcard.WildcardExact addr);
      ptrnNwProto = Wildcard.WildcardAll; ptrnNwTos = Wildcard.WildcardAll;
      ptrnTpSrc = Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll;
      ptrnInPort = Wildcard.WildcardAll }
  
  (** val ipProto : nwProto -> t **)
  
  let ipProto proto =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact coq_Const_0x800); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      (Wildcard.WildcardExact proto); ptrnNwTos = Wildcard.WildcardAll;
      ptrnTpSrc = Wildcard.WildcardAll; ptrnTpDst = Wildcard.WildcardAll;
      ptrnInPort = Wildcard.WildcardAll }
  
  (** val tpSrcPort : int -> tpPort -> t **)
  
  let tpSrcPort proto tpPort0 =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact coq_Const_0x800); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      (Wildcard.WildcardExact proto); ptrnNwTos = Wildcard.WildcardAll;
      ptrnTpSrc = (Wildcard.WildcardExact tpPort0); ptrnTpDst =
      Wildcard.WildcardAll; ptrnInPort = Wildcard.WildcardAll }
  
  (** val tpDstPort : int -> tpPort -> t **)
  
  let tpDstPort proto tpPort0 =
    { ptrnDlSrc = Wildcard.WildcardAll; ptrnDlDst = Wildcard.WildcardAll;
      ptrnDlType = (Wildcard.WildcardExact coq_Const_0x800); ptrnDlVlan =
      Wildcard.WildcardAll; ptrnDlVlanPcp = Wildcard.WildcardAll; ptrnNwSrc =
      Wildcard.WildcardAll; ptrnNwDst = Wildcard.WildcardAll; ptrnNwProto =
      (Wildcard.WildcardExact proto); ptrnNwTos = Wildcard.WildcardAll;
      ptrnTpSrc = Wildcard.WildcardAll; ptrnTpDst = (Wildcard.WildcardExact
      tpPort0); ptrnInPort = Wildcard.WildcardAll }
  
  (** val tcpSrcPort : tpPort -> t **)
  
  let tcpSrcPort =
    tpSrcPort coq_Const_0x6
  
  (** val tcpDstPort : tpPort -> t **)
  
  let tcpDstPort =
    tpDstPort coq_Const_0x6
  
  (** val udpSrcPort : tpPort -> t **)
  
  let udpSrcPort =
    tpSrcPort coq_Const_0x7
  
  (** val udpDstPort : tpPort -> t **)
  
  let udpDstPort =
    tpDstPort coq_Const_0x7
 end

type pattern = Pattern.t

