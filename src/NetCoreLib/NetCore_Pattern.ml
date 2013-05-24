open Packet
open OpenFlow0x01

type port =
| Physical of portId
| All
| Bucket of int
| Here

module PortOrderedType = struct

  type t = port

  let compare = Pervasives.compare

  let to_string = function
    | Physical pid -> "Physical " ^ (portId_to_string pid)
    | All -> "All"
    | Bucket n -> "Bucket " ^ (string_of_int n)
    | Here -> "Here"

end

let string_of_port = PortOrderedType.to_string

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

type t = {
  ptrnDlSrc : DlAddrWildcard.t;
  ptrnDlDst : DlAddrWildcard.t;
  ptrnDlType : DlTypWildcard.t;
  ptrnDlVlan : DlVlanWildcard.t;
  ptrnDlVlanPcp : DlVlanPcpWildcard.t;
  ptrnNwSrc : NwAddrWildcard.t;
  ptrnNwDst : NwAddrWildcard.t;
  ptrnNwProto : NwProtoWildcard.t;
  ptrnNwTos : NwTosWildcard.t;
  ptrnTpSrc : TpPortWildcard.t;
  ptrnTpDst : TpPortWildcard.t;
  ptrnInPort : PortWildcard.t
}

let all = {
  ptrnDlSrc = DlAddrWildcard.WildcardAll;
  ptrnDlDst = DlAddrWildcard.WildcardAll;
  ptrnDlType = DlTypWildcard.WildcardAll;
  ptrnDlVlan = DlVlanWildcard.WildcardAll;
  ptrnDlVlanPcp = DlVlanPcpWildcard.WildcardAll;
  ptrnNwSrc = NwAddrWildcard.WildcardAll;
  ptrnNwDst = NwAddrWildcard.WildcardAll;
  ptrnNwProto = NwProtoWildcard.WildcardAll;
  ptrnNwTos = NwTosWildcard.WildcardAll;
  ptrnTpSrc = TpPortWildcard.WildcardAll;
  ptrnTpDst = TpPortWildcard.WildcardAll;
  ptrnInPort = PortWildcard.WildcardAll 
}

let empty = {
  ptrnDlSrc = DlAddrWildcard.WildcardNone;
  ptrnDlDst = DlAddrWildcard.WildcardNone;
  ptrnDlType = DlTypWildcard.WildcardNone;
  ptrnDlVlan = DlVlanWildcard.WildcardNone;
  ptrnDlVlanPcp = DlVlanPcpWildcard.WildcardNone;
  ptrnNwSrc = NwAddrWildcard.WildcardNone;
  ptrnNwDst = NwAddrWildcard.WildcardNone;
  ptrnNwProto = NwProtoWildcard.WildcardNone;
  ptrnNwTos = NwTosWildcard.WildcardNone;
  ptrnTpSrc = TpPortWildcard.WildcardNone;
  ptrnTpDst = TpPortWildcard.WildcardNone;
  ptrnInPort = PortWildcard.WildcardNone 
}

let is_empty pat =
  DlAddrWildcard.is_empty pat.ptrnDlSrc
  || DlAddrWildcard.is_empty pat.ptrnDlDst
  || DlTypWildcard.is_empty pat.ptrnDlType
  || DlVlanWildcard.is_empty pat.ptrnDlVlan
  || DlVlanPcpWildcard.is_empty pat.ptrnDlVlanPcp
  || NwAddrWildcard.is_empty pat.ptrnNwSrc
  || NwAddrWildcard.is_empty pat.ptrnNwDst
  || NwProtoWildcard.is_empty pat.ptrnNwProto
  || NwTosWildcard.is_empty pat.ptrnNwTos
  || TpPortWildcard.is_empty pat.ptrnTpSrc
  || TpPortWildcard.is_empty pat.ptrnTpDst
  || PortWildcard.is_empty pat.ptrnInPort

let is_exact pat =
  DlAddrWildcard.is_exact pat.ptrnDlSrc
  && DlAddrWildcard.is_exact pat.ptrnDlDst
  && DlTypWildcard.is_exact pat.ptrnDlType
  && DlVlanWildcard.is_exact pat.ptrnDlVlan
  && DlVlanPcpWildcard.is_exact pat.ptrnDlVlanPcp
  && NwAddrWildcard.is_exact pat.ptrnNwSrc
  && NwAddrWildcard.is_exact pat.ptrnNwDst
  && NwProtoWildcard.is_exact pat.ptrnNwProto
  && NwTosWildcard.is_exact pat.ptrnNwTos
  && TpPortWildcard.is_exact pat.ptrnTpSrc
  && TpPortWildcard.is_exact pat.ptrnTpDst
  && PortWildcard.is_exact pat.ptrnInPort

let to_match pat = 
  match (DlAddrWildcard.to_option pat.ptrnDlSrc,
         DlAddrWildcard.to_option pat.ptrnDlDst,
         DlTypWildcard.to_option pat.ptrnDlType,
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
        Match.dlSrc = dlSrc;
        Match.dlDst = dlDst;
        Match.dlTyp = dlTyp;
        Match.dlVlan = dlVlan;
        Match.dlVlanPcp = dlVlanPcp;
        Match.nwSrc = nwSrc;
        Match.nwDst = nwDst;
        Match.nwProto = nwProto;
        Match.nwTos = nwTos;
        Match.tpSrc = tpSrc;
        Match.tpDst = tpDst;
        Match.inPort = match inPort with
          | Some (Physical pt) -> Some pt
          | _ -> None
      }
    | _ -> None

let inter pat pat' = {
  ptrnDlSrc = DlAddrWildcard.inter pat.ptrnDlSrc pat'.ptrnDlSrc;
  ptrnDlDst = DlAddrWildcard.inter pat.ptrnDlDst pat'.ptrnDlDst;
  ptrnDlType = DlTypWildcard.inter pat.ptrnDlType pat'.ptrnDlType;
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
  DlTypWildcard.contains pat1.ptrnDlType pat2.ptrnDlType &&
  DlVlanWildcard.contains pat1.ptrnDlVlan pat2.ptrnDlVlan &&
  DlVlanPcpWildcard.contains pat1.ptrnDlVlanPcp pat2.ptrnDlVlanPcp &&
  NwAddrWildcard.contains pat1.ptrnNwSrc pat2.ptrnNwSrc &&
  NwAddrWildcard.contains pat1.ptrnNwDst pat2.ptrnNwDst &&
  NwProtoWildcard.contains pat1.ptrnNwProto pat2.ptrnNwProto &&
  NwTosWildcard.contains pat1.ptrnNwTos pat2.ptrnNwTos &&
  TpPortWildcard.contains pat1.ptrnTpSrc pat2.ptrnTpSrc &&
  TpPortWildcard.contains pat1.ptrnTpDst pat2.ptrnTpDst &&
  PortWildcard.contains pat1.ptrnInPort pat2.ptrnInPort
  
let exact_pattern pk pt = {
  ptrnDlSrc = DlAddrWildcard.WildcardExact pk.pktDlSrc;
  ptrnDlDst = DlAddrWildcard.WildcardExact pk.pktDlDst;
  ptrnDlType = DlTypWildcard.WildcardExact pk.pktDlTyp;
  ptrnDlVlan = DlVlanWildcard.WildcardExact pk.pktDlVlan;
  ptrnDlVlanPcp = DlVlanPcpWildcard.WildcardExact pk.pktDlVlanPcp;
  ptrnNwSrc = NwAddrWildcard.WildcardExact (pktNwSrc pk);
  ptrnNwDst = NwAddrWildcard.WildcardExact (pktNwDst pk);
  ptrnNwProto = NwProtoWildcard.WildcardExact (pktNwProto pk);
  ptrnNwTos = NwTosWildcard.WildcardExact (pktNwTos pk);
  ptrnTpSrc = TpPortWildcard.WildcardExact (pktTpSrc pk);
  ptrnTpDst = TpPortWildcard.WildcardExact (pktTpDst pk);
  ptrnInPort = PortWildcard.WildcardExact pt
}

let match_packet pt pk pat =
  not (is_empty (inter (exact_pattern pk pt) pat))

let setDlSrc dlSrc pat =
  { pat with ptrnDlSrc = DlAddrWildcard.WildcardExact dlSrc }

let wildcardDlSrc pat = 
  { pat with ptrnDlSrc = DlAddrWildcard.WildcardAll }

let setDlDst dlDst pat =
  { pat with ptrnDlDst = DlAddrWildcard.WildcardExact dlDst }

let wildcardDlDst pat = 
  { pat with ptrnDlDst = DlAddrWildcard.WildcardAll }

let setDlVlan dlVlan pat =
  { pat with ptrnDlVlan = DlVlanWildcard.WildcardExact dlVlan }

let wildcardDlVlan pat = 
  { pat with ptrnDlVlan = DlVlanWildcard.WildcardAll }

let setPort port pat =
  { pat with ptrnInPort = PortWildcard.WildcardExact port }

let wildcardPort pat = 
  { pat with ptrnInPort = PortWildcard.WildcardAll }

let inPort pt =
  { all with ptrnInPort = PortWildcard.WildcardExact pt }

let dlSrc mac =
  { all with ptrnDlSrc = DlAddrWildcard.WildcardExact mac }

let dlDst mac =
  { all with ptrnDlDst = DlAddrWildcard.WildcardExact mac }

let dlType typ =
  { all with ptrnDlType = DlTypWildcard.WildcardExact typ }

let dlVlan vlan =
  { all with ptrnDlVlan = DlVlanWildcard.WildcardExact vlan }

let dlVlanPcp pcp =
  { all with ptrnDlVlanPcp = DlVlanPcpWildcard.WildcardExact pcp }

let ipSrc ip =
  { all with
    ptrnDlType = DlTypWildcard.WildcardExact 0x800;
    ptrnNwSrc = NwAddrWildcard.WildcardExact ip }

let ipDst ip =
  { all with
    ptrnDlType = DlTypWildcard.WildcardExact 0x800;
    ptrnNwDst = NwAddrWildcard.WildcardExact ip }

let ipProto proto =
  { all with
    ptrnDlType = DlTypWildcard.WildcardExact 0x800;
    ptrnNwProto = NwProtoWildcard.WildcardExact proto }

let tpSrcPort proto tpPort =
  { all with
    ptrnDlType = DlTypWildcard.WildcardExact 0x800;
    ptrnNwProto = NwProtoWildcard.WildcardExact proto;
    ptrnTpSrc = TpPortWildcard.WildcardExact tpPort }

let tpDstPort proto tpPort =
  { all with
    ptrnDlType = DlTypWildcard.WildcardExact 0x800;
    ptrnNwProto = NwProtoWildcard.WildcardExact proto;
    ptrnTpDst = TpPortWildcard.WildcardExact tpPort }

let tcpSrcPort = tpSrcPort 6
let tcpDstPort = tpSrcPort 6
let udpSrcPort = tpSrcPort 17
let udpDstPort = tpDstPort 17


let to_format fmt pat = 
  let open Format in
  fprintf 
    fmt
    "@[{@;<1 2>@[dlSrc = %s;@ dlDst = %s;@ dlType = %s;@ \
     dlVlan = %s;@ dlVlanPcp = %s;@ nwSrc = %s;@ nwDst = %s;@ \
     nwProto = %s;@ nwTos = %s;@ tpSrc = %s;@ tpDst = %s;@ \
     inPort = %s@]@ }@]"
    (DlAddrWildcard.to_string pat.ptrnDlSrc)
    (DlAddrWildcard.to_string pat.ptrnDlDst)
    (DlTypWildcard.to_string pat.ptrnDlType)
    (DlVlanWildcard.to_string pat.ptrnDlVlan)
    (DlVlanPcpWildcard.to_string pat.ptrnDlVlanPcp)
    (NwAddrWildcard.to_string pat.ptrnNwSrc)
    (NwAddrWildcard.to_string pat.ptrnNwDst)
    (NwProtoWildcard.to_string pat.ptrnNwProto)
    (NwTosWildcard.to_string pat.ptrnNwTos)
    (TpPortWildcard.to_string pat.ptrnTpSrc)
    (TpPortWildcard.to_string pat.ptrnTpDst)
    (PortWildcard.to_string pat.ptrnInPort)

let to_string x = 
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in
  Format.pp_set_margin fmt 80;
  to_format fmt x;
  Format.fprintf fmt "@?";
  Buffer.contents buf
