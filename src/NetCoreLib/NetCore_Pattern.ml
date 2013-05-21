open Packet
open OpenFlow0x01

(* orphaned; plonking down here for now *)
module Maybe = 
 struct 
  type 'a m = 'a option

  let bind m f = match m with
    | Some a -> f a
    | None -> None
  
  let ret x = Some x
 end

type port =
| Physical of portId
| All
| Bucket of int

let opt_portId = function
  | Physical pt -> Some pt
  | _ -> None

let string_of_port = function
  | Physical pid -> "P" ^ (portId_to_string pid)
  | All -> "All"
  | Bucket n -> "B" ^ (string_of_int n)

type t =
  { ptrnDlSrc : dlAddr NetCore_Wildcard.coq_Wildcard;
    ptrnDlDst : dlAddr NetCore_Wildcard.coq_Wildcard;
    ptrnDlType : dlTyp NetCore_Wildcard.coq_Wildcard;
    ptrnDlVlan : dlVlan NetCore_Wildcard.coq_Wildcard;
    ptrnDlVlanPcp : dlVlanPcp NetCore_Wildcard.coq_Wildcard;
    ptrnNwSrc : nwAddr NetCore_Wildcard.coq_Wildcard;
    ptrnNwDst : nwAddr NetCore_Wildcard.coq_Wildcard;
    ptrnNwProto : nwProto NetCore_Wildcard.coq_Wildcard;
    ptrnNwTos : nwTos NetCore_Wildcard.coq_Wildcard;
    ptrnTpSrc : tpPort NetCore_Wildcard.coq_Wildcard;
    ptrnTpDst : tpPort NetCore_Wildcard.coq_Wildcard;
    ptrnInPort : port NetCore_Wildcard.coq_Wildcard }

(* accessors *)
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

let all =
  { ptrnDlSrc = NetCore_Wildcard.WildcardAll;
    ptrnDlDst = NetCore_Wildcard.WildcardAll;
    ptrnDlType = NetCore_Wildcard.WildcardAll;
    ptrnDlVlan = NetCore_Wildcard.WildcardAll;
    ptrnDlVlanPcp = NetCore_Wildcard.WildcardAll;
    ptrnNwSrc = NetCore_Wildcard.WildcardAll;
    ptrnNwDst = NetCore_Wildcard.WildcardAll;
    ptrnNwProto = NetCore_Wildcard.WildcardAll;
    ptrnNwTos = NetCore_Wildcard.WildcardAll;
    ptrnTpSrc = NetCore_Wildcard.WildcardAll;
    ptrnTpDst = NetCore_Wildcard.WildcardAll;
    ptrnInPort = NetCore_Wildcard.WildcardAll }

let empty =
  { ptrnDlSrc = NetCore_Wildcard.WildcardNone;
    ptrnDlDst = NetCore_Wildcard.WildcardNone;
    ptrnDlType = NetCore_Wildcard.WildcardNone;
    ptrnDlVlan = NetCore_Wildcard.WildcardNone;
    ptrnDlVlanPcp = NetCore_Wildcard.WildcardNone;
    ptrnNwSrc = NetCore_Wildcard.WildcardNone;
    ptrnNwDst = NetCore_Wildcard.WildcardNone;
    ptrnNwProto = NetCore_Wildcard.WildcardNone;
    ptrnNwTos = NetCore_Wildcard.WildcardNone;
    ptrnTpSrc = NetCore_Wildcard.WildcardNone;
    ptrnTpDst = NetCore_Wildcard.WildcardNone;
    ptrnInPort = NetCore_Wildcard.WildcardNone }

let is_empty pat =
     NetCore_Wildcard.Wildcard.is_empty pat.ptrnDlSrc
  || NetCore_Wildcard.Wildcard.is_empty pat.ptrnDlDst
  || NetCore_Wildcard.Wildcard.is_empty pat.ptrnDlType
  || NetCore_Wildcard.Wildcard.is_empty pat.ptrnDlVlan
  || NetCore_Wildcard.Wildcard.is_empty pat.ptrnDlVlanPcp
  || NetCore_Wildcard.Wildcard.is_empty pat.ptrnNwSrc
  || NetCore_Wildcard.Wildcard.is_empty pat.ptrnNwDst
  || NetCore_Wildcard.Wildcard.is_empty pat.ptrnNwProto
  || NetCore_Wildcard.Wildcard.is_empty pat.ptrnNwTos
  || NetCore_Wildcard.Wildcard.is_empty pat.ptrnTpSrc
  || NetCore_Wildcard.Wildcard.is_empty pat.ptrnTpDst
  || NetCore_Wildcard.Wildcard.is_empty pat.ptrnInPort

let wild_to_opt = function
  | NetCore_Wildcard.WildcardExact x ->
    Some (Some x)
  | NetCore_Wildcard.WildcardAll ->
    Some None
  | NetCore_Wildcard.WildcardNone ->
    None

let to_match pat =
  Maybe.bind (wild_to_opt pat.ptrnDlSrc) (fun dlSrc ->
    Maybe.bind (wild_to_opt pat.ptrnDlDst) (fun dlDst ->
      Maybe.bind (wild_to_opt pat.ptrnDlType) (fun typ ->
        Maybe.bind (wild_to_opt pat.ptrnDlVlan) (fun vlan ->
          Maybe.bind (wild_to_opt pat.ptrnDlVlanPcp) (fun pcp ->
            Maybe.bind (wild_to_opt pat.ptrnNwSrc) (fun nwSrc ->
              Maybe.bind (wild_to_opt pat.ptrnNwDst) (fun nwDst ->
                Maybe.bind (wild_to_opt pat.ptrnNwProto) (fun nwProto ->
                  Maybe.bind (wild_to_opt pat.ptrnNwTos) (fun nwTos ->
                    Maybe.bind (wild_to_opt pat.ptrnTpSrc) (fun tpSrc ->
                      Maybe.bind (wild_to_opt pat.ptrnTpDst) (fun tpDst ->
                        Maybe.bind (wild_to_opt pat.ptrnInPort) (fun pt ->
                          Maybe.bind
                            (match pt with
                             | Some pt' ->
                               (match opt_portId pt' with
                               | Some phys -> Some (Some phys)
                               | None -> None)
                             | None -> Some None) (fun pt'' ->
                               let open Match in
                            Maybe.ret { dlSrc = dlSrc;
                                        dlDst = dlDst;
                                        dlTyp = typ;
                                        dlVlan = vlan;
                                        dlVlanPcp = pcp;
                                        nwSrc = nwSrc;
                                        nwDst = nwDst;
                                        nwProto = nwProto;
                                        nwTos = nwTos;
                                        tpSrc = tpSrc;
                                        tpDst = tpDst;
                                        inPort = pt'' })))))))))))))

let inter pat pat' =
  { ptrnDlSrc = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnDlSrc pat'.ptrnDlSrc;
    ptrnDlDst = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnDlDst pat'.ptrnDlDst;
    ptrnDlType = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnDlType pat'.ptrnDlType;
    ptrnDlVlan = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnDlVlan pat'.ptrnDlVlan;
    ptrnDlVlanPcp = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnDlVlanPcp pat'.ptrnDlVlanPcp;
    ptrnNwSrc = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnNwSrc pat'.ptrnNwSrc;
    ptrnNwDst = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnNwDst pat'.ptrnNwDst;
    ptrnNwProto = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnNwProto pat'.ptrnNwProto;
    ptrnNwTos = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnNwTos pat'.ptrnNwTos;
    ptrnTpSrc = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnTpSrc pat'.ptrnTpSrc;
    ptrnTpDst = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnTpDst pat'.ptrnTpDst;
    ptrnInPort = NetCore_Wildcard.Wildcard.inter (=) pat.ptrnInPort pat'.ptrnInPort }

let exact_pattern pk pt =
  { ptrnDlSrc = NetCore_Wildcard.WildcardExact pk.pktDlSrc;
    ptrnDlDst = NetCore_Wildcard.WildcardExact pk.pktDlDst;
    ptrnDlType = NetCore_Wildcard.WildcardExact pk.pktDlTyp;
    ptrnDlVlan = NetCore_Wildcard.WildcardExact pk.pktDlVlan;
    ptrnDlVlanPcp = NetCore_Wildcard.WildcardExact pk.pktDlVlanPcp;
    ptrnNwSrc = NetCore_Wildcard.WildcardExact (pktNwSrc pk);
    ptrnNwDst = NetCore_Wildcard.WildcardExact (pktNwDst pk);
    ptrnNwProto = NetCore_Wildcard.WildcardExact (pktNwProto pk);
    ptrnNwTos = NetCore_Wildcard.WildcardExact (pktNwTos pk);
    ptrnTpSrc = NetCore_Wildcard.WildcardExact (pktTpSrc pk);
    ptrnTpDst = NetCore_Wildcard.WildcardExact (pktTpDst pk);
    ptrnInPort = NetCore_Wildcard.WildcardExact pt }

let match_packet pt pk pat =
  not (is_empty (inter (exact_pattern pk pt) pat))

let is_exact pat =
     NetCore_Wildcard.Wildcard.is_exact pat.ptrnDlSrc
  && NetCore_Wildcard.Wildcard.is_exact pat.ptrnDlDst
  && NetCore_Wildcard.Wildcard.is_exact pat.ptrnDlType
  && NetCore_Wildcard.Wildcard.is_exact pat.ptrnDlVlan
  && NetCore_Wildcard.Wildcard.is_exact pat.ptrnDlVlanPcp
  && NetCore_Wildcard.Wildcard.is_exact pat.ptrnNwSrc
  && NetCore_Wildcard.Wildcard.is_exact pat.ptrnNwDst
  && NetCore_Wildcard.Wildcard.is_exact pat.ptrnNwProto
  && NetCore_Wildcard.Wildcard.is_exact pat.ptrnNwTos
  && NetCore_Wildcard.Wildcard.is_exact pat.ptrnTpSrc
  && NetCore_Wildcard.Wildcard.is_exact pat.ptrnTpDst
  && NetCore_Wildcard.Wildcard.is_exact pat.ptrnInPort

let supportedNwProto = [0x6; 0x7]

let supportedDlTyp = [0x800; 0x806]

let to_valid pat =
  let validDlType = match pat.ptrnDlType with
    | NetCore_Wildcard.WildcardExact n ->
      (=) n 0x800 || (=) n 0x806
    | _ -> false in
  let validNwProto = match pat.ptrnNwProto with
    | NetCore_Wildcard.WildcardExact n ->
      (=) n 0x6 || (=) n 0x7
    | _ -> false in
  { ptrnDlSrc = pat.ptrnDlSrc;
    ptrnDlDst = pat.ptrnDlDst;
    ptrnDlType = pat.ptrnDlType;
    ptrnDlVlan = pat.ptrnDlVlan;
    ptrnDlVlanPcp = pat.ptrnDlVlanPcp;
    ptrnNwSrc = if validDlType then pat.ptrnNwSrc else NetCore_Wildcard.WildcardAll;
    ptrnNwDst = if validDlType then pat.ptrnNwDst else NetCore_Wildcard.WildcardAll;
    ptrnNwProto = if validDlType then pat.ptrnNwProto else NetCore_Wildcard.WildcardAll;
    ptrnNwTos = if validDlType then pat.ptrnNwTos else NetCore_Wildcard.WildcardAll;
    ptrnTpSrc = if validNwProto then pat.ptrnTpSrc else NetCore_Wildcard.WildcardAll;
    ptrnTpDst = if validNwProto then pat.ptrnTpDst else NetCore_Wildcard.WildcardAll;
    ptrnInPort = pat.ptrnInPort }

let to_all w = function
  | true -> NetCore_Wildcard.WildcardAll
  | false -> w

let setDlSrc dlSrc pat =
  to_valid { pat with ptrnDlSrc = NetCore_Wildcard.WildcardExact dlSrc }

let wildcardDlSrc pat = { pat with ptrnDlSrc = NetCore_Wildcard.WildcardAll }

let setDlDst dlDst pat =
  to_valid { pat with ptrnDlDst = NetCore_Wildcard.WildcardExact dlDst }

let wildcardDlDst pat = { pat with ptrnDlDst = NetCore_Wildcard.WildcardAll }

let setDlVlan dlVlan pat =
  to_valid { pat with ptrnDlVlan = NetCore_Wildcard.WildcardExact dlVlan }

let wildcardDlVlan pat = { pat with ptrnDlVlan = NetCore_Wildcard.WildcardAll }

let setPort port pat = { pat with ptrnInPort = NetCore_Wildcard.WildcardExact port }

let wildcardPort pat = { pat with ptrnInPort = NetCore_Wildcard.WildcardAll }

let inPort pt =
  { all with ptrnInPort = NetCore_Wildcard.WildcardExact pt }

let dlSrc mac =
  { all with ptrnDlSrc = NetCore_Wildcard.WildcardExact mac }

let dlDst mac =
  { all with ptrnDlDst = NetCore_Wildcard.WildcardExact mac }

let dlType typ =
  { all with ptrnDlType = NetCore_Wildcard.WildcardExact typ }

let dlVlan vlan =
  { all with ptrnDlVlan = NetCore_Wildcard.WildcardExact vlan }

let dlVlanPcp pcp =
  { all with ptrnDlVlanPcp = NetCore_Wildcard.WildcardExact pcp }

let ipSrc ip =
  { all with
    ptrnDlType = NetCore_Wildcard.WildcardExact 0x800;
    ptrnNwSrc = NetCore_Wildcard.WildcardExact ip }

let ipDst ip =
  { all with
    ptrnDlType = NetCore_Wildcard.WildcardExact 0x800;
    ptrnNwDst = NetCore_Wildcard.WildcardExact ip }

let ipProto proto =
  { all with
    ptrnDlType = NetCore_Wildcard.WildcardExact 0x800;
    ptrnNwProto = NetCore_Wildcard.WildcardExact proto }

let tpSrcPort proto tpPort =
  { all with
    ptrnDlType = NetCore_Wildcard.WildcardExact 0x800;
    ptrnNwProto = NetCore_Wildcard.WildcardExact proto;
    ptrnTpSrc = NetCore_Wildcard.WildcardExact tpPort }

let tpDstPort proto tpPort =
  { all with
    ptrnDlType = NetCore_Wildcard.WildcardExact 0x800;
    ptrnNwProto = NetCore_Wildcard.WildcardExact proto;
    ptrnTpDst = NetCore_Wildcard.WildcardExact tpPort }

let tcpSrcPort = tpSrcPort 0x6

let tcpDstPort = tpDstPort 0x6

let udpSrcPort = tpSrcPort 0x7

let udpDstPort = tpDstPort 0x7

(* JNF: why isn't this in NetCore_Wildcard? *)
let wildcard_to_string to_string w =
  let open NetCore_Wildcard in
  match w with
  | WildcardExact a -> to_string a
  | WildcardAll -> "*"
  | WildcardNone -> "None"

let to_string pat =
  if is_empty pat then
    "<empty>"
  else
    let reflections =
      [ ("DlSrc", wildcard_to_string dlAddr_to_string pat.ptrnDlSrc)
      ; ("DlDst", wildcard_to_string dlAddr_to_string pat.ptrnDlDst)
      ; ("DlType", wildcard_to_string dlTyp_to_string pat.ptrnDlType)
      ; ("DlVlan", wildcard_to_string dlVlan_to_string pat.ptrnDlVlan)
      ; ("DlVlanPcp",
         wildcard_to_string dlVlanPcp_to_string pat.ptrnDlVlanPcp)
      ; ("NwSrc", wildcard_to_string nwAddr_to_string pat.ptrnNwSrc)
      ; ("NwDst", wildcard_to_string nwAddr_to_string pat.ptrnNwDst)
      ; ("NwProto", wildcard_to_string nwProto_to_string pat.ptrnNwProto)
      ; ("NwTos", wildcard_to_string nwTos_to_string pat.ptrnNwTos)
      ; ("TpSrc", wildcard_to_string tpPort_to_string pat.ptrnTpSrc)
      ; ("TpDst", wildcard_to_string tpPort_to_string pat.ptrnTpDst)
      ; ("InPort", wildcard_to_string string_of_port pat.ptrnInPort)
      ] in
    let non_top =
      List.filter (fun (field, wild) -> wild <> "*") reflections in
    let rvs = List.map (fun (f,v) -> Printf.sprintf "%s %s" f v) non_top in
    if List.length rvs > 0 then
      "{" ^ (String.concat ", " rvs) ^ "}"
    else
      "{*}"
