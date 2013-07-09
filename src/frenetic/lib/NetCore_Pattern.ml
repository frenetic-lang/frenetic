open Packet
open OpenFlow0x01
open NetCore_Types
open NetCore_Wildcard

module OF = OpenFlow0x01_Core

type t = ptrn


module PortOrderedType = struct
  type t = port
  let compare = Pervasives.compare
  let to_string =  function
    | Physical pid -> "Physical " ^ (NetCore_Types.string_of_portId pid)
    | All -> "All"
    | Here -> "Here"

end

module DlVlanOrderedType = struct
  type t = int option

  let compare x y = match (x, y) with
    | None, None -> 0
    | None, _ -> -1
    | _, None -> 1
    | Some a, Some b -> Pervasives.compare a b

  let to_string x = match x with
    | Some n -> "Some " ^ string_of_int n
    | None -> "None"
end

module Int64Wildcard = NetCore_Wildcard.Make (Int64)
module Int32Wildcard = NetCore_Wildcard.Make (Int32)
module IntWildcard =  NetCore_Wildcard.Make (struct
  type t = int
  let compare = Pervasives.compare
  let to_string n = string_of_int n
end)

module DlAddrWildcard = Int64Wildcard
module DlTypWildcard = IntWildcard
module DlVlanWildcard = NetCore_Wildcard.Make (DlVlanOrderedType)
module DlVlanPcpWildcard = IntWildcard
module NwAddrWildcard = Int32Wildcard
module NwProtoWildcard = IntWildcard
module NwTosWildcard = IntWildcard
module TpPortWildcard = IntWildcard
module PortWildcard = NetCore_Wildcard.Make (PortOrderedType)

let is_empty pat =
  DlAddrWildcard.is_empty pat.ptrnDlSrc
  || DlAddrWildcard.is_empty pat.ptrnDlDst
  || DlTypWildcard.is_empty pat.ptrnDlTyp
  || DlVlanWildcard.is_empty pat.ptrnDlVlan
  || DlVlanPcpWildcard.is_empty pat.ptrnDlVlanPcp
  || NwAddrWildcard.is_empty pat.ptrnNwSrc
  || NwAddrWildcard.is_empty pat.ptrnNwDst
  || NwProtoWildcard.is_empty pat.ptrnNwProto
  || NwTosWildcard.is_empty pat.ptrnNwTos
  || TpPortWildcard.is_empty pat.ptrnTpSrc
  || TpPortWildcard.is_empty pat.ptrnTpDst
  || PortWildcard.is_empty pat.ptrnInPort

let is_all pat = pat = all

let is_exact pat =
  DlAddrWildcard.is_exact pat.ptrnDlSrc
  && DlAddrWildcard.is_exact pat.ptrnDlDst
  && DlTypWildcard.is_exact pat.ptrnDlTyp
  && DlVlanWildcard.is_exact pat.ptrnDlVlan
  && DlVlanPcpWildcard.is_exact pat.ptrnDlVlanPcp
  && NwAddrWildcard.is_exact pat.ptrnNwSrc
  && NwAddrWildcard.is_exact pat.ptrnNwDst
  && NwProtoWildcard.is_exact pat.ptrnNwProto
  && NwTosWildcard.is_exact pat.ptrnNwTos
  && TpPortWildcard.is_exact pat.ptrnTpSrc
  && TpPortWildcard.is_exact pat.ptrnTpDst
  && PortWildcard.is_exact pat.ptrnInPort

let to_match0x01 pat =
  match (DlAddrWildcard.to_option pat.ptrnDlSrc,
         DlAddrWildcard.to_option pat.ptrnDlDst,
         DlTypWildcard.to_option pat.ptrnDlTyp,
         DlVlanWildcard.to_option pat.ptrnDlVlan,
         DlVlanPcpWildcard.to_option pat.ptrnDlVlanPcp,
         NwAddrWildcard.to_option pat.ptrnNwSrc,
         NwAddrWildcard.to_option pat.ptrnNwDst,
         NwProtoWildcard.to_option pat.ptrnNwProto,
         NwTosWildcard.to_option pat.ptrnNwTos,
         TpPortWildcard.to_option pat.ptrnTpSrc,
         TpPortWildcard.to_option pat.ptrnTpDst,
         PortWildcard.to_option pat.ptrnInPort) with
    | (Some dlSrc, Some dlDst, Some dlTyp, Some dlVlan, Some dlVlanPcp,
       Some nwSrc, Some nwDst, Some nwProto, Some nwTos,
       Some tpSrc, Some tpDst, Some inPort) ->
      Some {
        OF.dlSrc = dlSrc;
        OF.dlDst = dlDst;
        OF.dlTyp = dlTyp;
        OF.dlVlan = dlVlan;
        OF.dlVlanPcp = dlVlanPcp;
        OF.nwSrc = nwSrc;
        OF.nwDst = nwDst;
        OF.nwProto = nwProto;
        OF.nwTos = nwTos;
        OF.tpSrc = tpSrc;
        OF.tpDst = tpDst;
        OF.inPort = match inPort with
          | Some (Physical pt) -> Some (Int32.to_int pt)
          | _ -> None
      }
    | _ -> None

module OF0x04 = OpenFlow0x04_Core

let to_match0x04 pat =
  match (DlAddrWildcard.to_option pat.ptrnDlSrc,
         DlAddrWildcard.to_option pat.ptrnDlDst,
         DlTypWildcard.to_option pat.ptrnDlTyp,
         DlVlanWildcard.to_option pat.ptrnDlVlan,
         DlVlanPcpWildcard.to_option pat.ptrnDlVlanPcp,
         NwAddrWildcard.to_option pat.ptrnNwSrc,
         NwAddrWildcard.to_option pat.ptrnNwDst,
         NwProtoWildcard.to_option pat.ptrnNwProto,
         NwTosWildcard.to_option pat.ptrnNwTos,
         TpPortWildcard.to_option pat.ptrnTpSrc,
         TpPortWildcard.to_option pat.ptrnTpDst,
         PortWildcard.to_option pat.ptrnInPort) with
    | (Some dlSrc, Some dlDst, Some dlTyp, Some dlVlan, Some dlVlanPcp,
       Some nwSrc, Some nwDst, Some nwProto, Some nwTos,
       Some tpSrc, Some tpDst, Some inPort) ->
      ((match dlSrc with       None -> [] | Some v -> [ OF0x04.OxmEthSrc (OF0x04.val_to_mask v) ]) 
       @ (match dlDst with     None -> [] | Some v -> [ OF0x04.OxmEthDst (OF0x04.val_to_mask v) ]) 
       @ (match dlTyp with     None -> [] | Some v -> [ OF0x04.OxmEthType v ]) 
       @ (match dlVlan with    None -> [] | Some (None) -> [] | Some (Some v) -> [ OF0x04.OxmVlanVId (OF0x04.val_to_mask v) ]) 
       @ (match dlVlanPcp with None -> [] | Some v -> [ OF0x04.OxmVlanPcp v ])
       @ (match nwSrc with     None -> [] | Some v -> [ OF0x04.OxmIP4Src (OF0x04.val_to_mask v) ])
       @ (match nwDst with     None -> [] | Some v -> [ OF0x04.OxmIP4Dst (OF0x04.val_to_mask v) ])
       @ (match nwProto with   None -> [] | Some v -> [ OF0x04.OxmIPProto v ])
       @ (match tpSrc with     None -> [] | Some v -> [ OF0x04.OxmTCPSrc (OF0x04.val_to_mask v) ])
       @ (match tpDst with     None -> [] | Some v -> [ OF0x04.OxmTCPDst (OF0x04.val_to_mask v) ])
       @ (match inPort with    None -> [] | Some (Physical pt) -> [ OF0x04.OxmInPort pt ] | _ -> []),
       (match inPort with Some (Physical pt) -> Some pt | _ -> None))
    | _ -> ([], None)

let inter pat pat' = {
  ptrnDlSrc = DlAddrWildcard.inter pat.ptrnDlSrc pat'.ptrnDlSrc;
  ptrnDlDst = DlAddrWildcard.inter pat.ptrnDlDst pat'.ptrnDlDst;
  ptrnDlTyp = DlTypWildcard.inter pat.ptrnDlTyp pat'.ptrnDlTyp;
  ptrnDlVlan = DlVlanWildcard.inter pat.ptrnDlVlan pat'.ptrnDlVlan;
  ptrnDlVlanPcp = DlVlanPcpWildcard.inter pat.ptrnDlVlanPcp pat'.ptrnDlVlanPcp;
  ptrnNwSrc = NwAddrWildcard.inter pat.ptrnNwSrc pat'.ptrnNwSrc;
  ptrnNwDst = NwAddrWildcard.inter pat.ptrnNwDst pat'.ptrnNwDst;
  ptrnNwProto = NwProtoWildcard.inter pat.ptrnNwProto pat'.ptrnNwProto;
  ptrnNwTos = NwTosWildcard.inter pat.ptrnNwTos pat'.ptrnNwTos;
  ptrnTpSrc = TpPortWildcard.inter pat.ptrnTpSrc pat'.ptrnTpSrc;
  ptrnTpDst = TpPortWildcard.inter pat.ptrnTpDst pat'.ptrnTpDst;
  ptrnInPort = PortWildcard.inter pat.ptrnInPort pat'.ptrnInPort
}

let contains pat1 pat2 =
  DlAddrWildcard.contains pat1.ptrnDlSrc pat2.ptrnDlSrc &&
  DlAddrWildcard.contains pat1.ptrnDlDst pat2.ptrnDlDst &&
  DlTypWildcard.contains pat1.ptrnDlTyp pat2.ptrnDlTyp &&
  DlVlanWildcard.contains pat1.ptrnDlVlan pat2.ptrnDlVlan &&
  DlVlanPcpWildcard.contains pat1.ptrnDlVlanPcp pat2.ptrnDlVlanPcp &&
  NwAddrWildcard.contains pat1.ptrnNwSrc pat2.ptrnNwSrc &&
  NwAddrWildcard.contains pat1.ptrnNwDst pat2.ptrnNwDst &&
  NwProtoWildcard.contains pat1.ptrnNwProto pat2.ptrnNwProto &&
  NwTosWildcard.contains pat1.ptrnNwTos pat2.ptrnNwTos &&
  TpPortWildcard.contains pat1.ptrnTpSrc pat2.ptrnTpSrc &&
  TpPortWildcard.contains pat1.ptrnTpDst pat2.ptrnTpDst &&
  PortWildcard.contains pat1.ptrnInPort pat2.ptrnInPort

let zero_default32 f x = 
  try f x 
  with Invalid_argument _ -> 0l

let zero_default f x = 
  try f x
  with Invalid_argument _ -> 0

let exact_pattern pk pt = 
  let dlTyp = Packet.dlTyp pk in
  { ptrnDlSrc = WildcardExact pk.dlSrc
  ; ptrnDlDst = WildcardExact pk.dlDst
  ; ptrnDlTyp = WildcardExact dlTyp
  ; ptrnDlVlan = WildcardExact pk.dlVlan
  ; ptrnDlVlanPcp = WildcardExact pk.dlVlanPcp
  ; ptrnNwSrc = WildcardExact (zero_default32 nwSrc pk)
  ; ptrnNwDst = WildcardExact (zero_default32 nwDst pk)
  ; ptrnNwProto = WildcardExact (zero_default nwProto pk)
  ; ptrnNwTos = WildcardExact (zero_default nwTos pk)
  ; ptrnTpSrc = WildcardExact (zero_default tpSrc pk)
  ; ptrnTpDst = WildcardExact (zero_default tpDst pk)
  ; ptrnInPort = WildcardExact pt }

let match_packet pt pk pat =
  not (is_empty (inter (exact_pattern pk pt) pat))

(* TODO(arjun): these set* used at all? *)
let setDlSrc dlSrc pat =
  { pat with ptrnDlSrc = WildcardExact dlSrc }

let setPort port pat =
  { pat with ptrnInPort = WildcardExact port }

let setDlDst dlDst pat =
  { pat with ptrnDlDst = WildcardExact dlDst }

let wildcardDlSrc pat =
  { pat with ptrnDlSrc = WildcardAll }


let wildcardDlDst pat =
  { pat with ptrnDlDst = WildcardAll }

let setDlVlan dlVlan pat =
  { pat with ptrnDlVlan = WildcardExact dlVlan }

let wildcardDlVlan pat =
  { pat with ptrnDlVlan = WildcardAll }


let wildcardPort pat =
  { pat with ptrnInPort = WildcardAll }

let wildcardDlVlanPcp pat =
  { pat with ptrnDlVlanPcp = WildcardAll }

let wildcardNwSrc pat =
  { pat with ptrnNwSrc = WildcardAll }

let wildcardNwDst pat =
  { pat with ptrnNwDst = WildcardAll }

let wildcardNwTos pat =
  { pat with ptrnNwTos = WildcardAll }

let wildcardTpSrc pat =
  { pat with ptrnTpSrc = WildcardAll }

let wildcardTpDst pat =
  { pat with ptrnTpDst = WildcardAll }
