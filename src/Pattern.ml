open Monad
open NetworkPacket
open OpenFlow0x01Types
open Word

type port = 
| Here
| Physical of portId
| Bucket of int
    
let opt_portId = function
  | Physical pt -> Some pt
  | _ -> None
    
let string_of_port = function
  | Here -> "Here"
  | Physical pid -> "P" ^ (portId_to_string pid)
  | Bucket n -> "B" ^ (string_of_int n)


  
type t = 
  { ptrnDlSrc : dlAddr Wildcard.coq_Wildcard;
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
  { ptrnDlSrc = Wildcard.WildcardAll; 
    ptrnDlDst = Wildcard.WildcardAll;
    ptrnDlType = Wildcard.WildcardAll; 
    ptrnDlVlan = Wildcard.WildcardAll;
    ptrnDlVlanPcp = Wildcard.WildcardAll; 
    ptrnNwSrc = Wildcard.WildcardAll;
    ptrnNwDst = Wildcard.WildcardAll; 
    ptrnNwProto = Wildcard.WildcardAll;
    ptrnNwTos = Wildcard.WildcardAll; 
    ptrnTpSrc = Wildcard.WildcardAll;
    ptrnTpDst = Wildcard.WildcardAll; 
    ptrnInPort = Wildcard.WildcardAll }
  
  let empty =
    { ptrnDlSrc = Wildcard.WildcardNone; 
      ptrnDlDst = Wildcard.WildcardNone;
      ptrnDlType = Wildcard.WildcardNone; 
      ptrnDlVlan = Wildcard.WildcardNone;
      ptrnDlVlanPcp = Wildcard.WildcardNone; 
      ptrnNwSrc = Wildcard.WildcardNone; 
      ptrnNwDst = Wildcard.WildcardNone; 
      ptrnNwProto = Wildcard.WildcardNone; 
      ptrnNwTos = Wildcard.WildcardNone; 
      ptrnTpSrc = Wildcard.WildcardNone; 
      ptrnTpDst = Wildcard.WildcardNone; 
      ptrnInPort = Wildcard.WildcardNone }
  
  let is_empty pat =
       Wildcard.Wildcard.is_empty pat.ptrnDlSrc 
    || Wildcard.Wildcard.is_empty pat.ptrnDlDst 
    || Wildcard.Wildcard.is_empty pat.ptrnDlType 
    || Wildcard.Wildcard.is_empty pat.ptrnDlVlan
    || Wildcard.Wildcard.is_empty pat.ptrnDlVlanPcp
    || Wildcard.Wildcard.is_empty pat.ptrnNwSrc
    || Wildcard.Wildcard.is_empty pat.ptrnNwDst
    || Wildcard.Wildcard.is_empty pat.ptrnNwProto
    || Wildcard.Wildcard.is_empty pat.ptrnNwTos
    || Wildcard.Wildcard.is_empty pat.ptrnTpSrc
    || Wildcard.Wildcard.is_empty pat.ptrnTpDst
    || Wildcard.Wildcard.is_empty pat.ptrnInPort 

  let wild_to_opt = function
    | Wildcard.WildcardExact x -> 
      Some (Some x)
    | Wildcard.WildcardAll -> 
      Some None
    | Wildcard.WildcardNone -> 
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
                              Maybe.ret { matchDlSrc = dlSrc; 
                                          matchDlDst = dlDst; 
                                          matchDlTyp = typ; 
                                          matchDlVlan = vlan; 
                                          matchDlVlanPcp = pcp; 
                                          matchNwSrc = nwSrc; 
                                          matchNwDst = nwDst; 
                                          matchNwProto = nwProto; 
                                          matchNwTos = nwTos; 
                                          matchTpSrc = tpSrc; 
                                          matchTpDst = tpDst; 
                                          matchInPort = pt'' })))))))))))))

    let inter pat pat' =
      { ptrnDlSrc = Wildcard.Wildcard.inter Word48.eq_dec pat.ptrnDlSrc pat'.ptrnDlSrc;
        ptrnDlDst = Wildcard.Wildcard.inter Word48.eq_dec pat.ptrnDlDst pat'.ptrnDlDst;
        ptrnDlType = Wildcard.Wildcard.inter Word16.eq_dec pat.ptrnDlType pat'.ptrnDlType;
        ptrnDlVlan = Wildcard.Wildcard.inter Word16.eq_dec pat.ptrnDlVlan pat'.ptrnDlVlan;
        ptrnDlVlanPcp = Wildcard.Wildcard.inter Word8.eq_dec pat.ptrnDlVlanPcp pat'.ptrnDlVlanPcp;
        ptrnNwSrc = Wildcard.Wildcard.inter Word32.eq_dec pat.ptrnNwSrc pat'.ptrnNwSrc;
        ptrnNwDst = Wildcard.Wildcard.inter Word32.eq_dec pat.ptrnNwDst pat'.ptrnNwDst;
        ptrnNwProto = Wildcard.Wildcard.inter Word8.eq_dec pat.ptrnNwProto pat'.ptrnNwProto;
        ptrnNwTos = Wildcard.Wildcard.inter Word8.eq_dec pat.ptrnNwTos pat'.ptrnNwTos;
        ptrnTpSrc = Wildcard.Wildcard.inter Word16.eq_dec pat.ptrnTpSrc pat'.ptrnTpSrc;
        ptrnTpDst = Wildcard.Wildcard.inter Word16.eq_dec pat.ptrnTpDst pat'.ptrnTpDst;
        ptrnInPort = Wildcard.Wildcard.inter (=) pat.ptrnInPort pat'.ptrnInPort }
  
  let exact_pattern pk pt =
    { ptrnDlSrc = Wildcard.WildcardExact pk.pktDlSrc; 
      ptrnDlDst = Wildcard.WildcardExact pk.pktDlDst; 
      ptrnDlType = Wildcard.WildcardExact pk.pktDlTyp; 
      ptrnDlVlan = Wildcard.WildcardExact pk.pktDlVlan; 
      ptrnDlVlanPcp = Wildcard.WildcardExact pk.pktDlVlanPcp; 
      ptrnNwSrc = Wildcard.WildcardExact (pktNwSrc pk); 
      ptrnNwDst = Wildcard.WildcardExact (pktNwDst pk); 
      ptrnNwProto = Wildcard.WildcardExact (pktNwProto pk); 
      ptrnNwTos = Wildcard.WildcardExact (pktNwTos pk); 
      ptrnTpSrc = Wildcard.WildcardExact (pktTpSrc pk); 
      ptrnTpDst = Wildcard.WildcardExact (pktTpDst pk); 
      ptrnInPort = Wildcard.WildcardExact pt }
  
  let match_packet pt pk pat =
    not (is_empty (inter (exact_pattern pk pt) pat))
  
  let is_exact pat =
       Wildcard.Wildcard.is_exact pat.ptrnDlSrc 
    && Wildcard.Wildcard.is_exact pat.ptrnDlDst 
    && Wildcard.Wildcard.is_exact pat.ptrnDlType 
    && Wildcard.Wildcard.is_exact pat.ptrnDlVlan
    && Wildcard.Wildcard.is_exact pat.ptrnDlVlanPcp
    && Wildcard.Wildcard.is_exact pat.ptrnNwSrc
    && Wildcard.Wildcard.is_exact pat.ptrnNwDst
    && Wildcard.Wildcard.is_exact pat.ptrnNwProto
    && Wildcard.Wildcard.is_exact pat.ptrnNwTos
    && Wildcard.Wildcard.is_exact pat.ptrnTpSrc
    && Wildcard.Wildcard.is_exact pat.ptrnTpDst
    && Wildcard.Wildcard.is_exact pat.ptrnInPort 

  let supportedNwProto = [0x6; 0x7]
  
  let supportedDlTyp = [0x800; 0x806]
  
  let to_valid pat =
    let validDlType = match pat.ptrnDlType with
      | Wildcard.WildcardExact n ->
        Word16.eq_dec n 0x800 || Word16.eq_dec n 0x806 
      | _ -> false in 
    let validNwProto = match pat.ptrnNwProto with 
      | Wildcard.WildcardExact n ->
        Word8.eq_dec n 0x6 || Word8.eq_dec n 0x7
      | _ -> false in 
    { ptrnDlSrc = pat.ptrnDlSrc; 
      ptrnDlDst = pat.ptrnDlDst; 
      ptrnDlType = pat.ptrnDlType;
      ptrnDlVlan = pat.ptrnDlVlan; 
      ptrnDlVlanPcp = pat.ptrnDlVlanPcp; 
      ptrnNwSrc = if validDlType then pat.ptrnNwSrc else Wildcard.WildcardAll;
      ptrnNwDst = if validDlType then pat.ptrnNwDst else Wildcard.WildcardAll; 
      ptrnNwProto = if validDlType then pat.ptrnNwProto else Wildcard.WildcardAll; 
      ptrnNwTos = if validDlType then pat.ptrnNwTos else Wildcard.WildcardAll; 
      ptrnTpSrc = if validNwProto then pat.ptrnTpSrc else Wildcard.WildcardAll; 
      ptrnTpDst = if validNwProto then pat.ptrnTpDst else Wildcard.WildcardAll; 
      ptrnInPort = pat.ptrnInPort }
  
  let to_all w = function
    | true -> Wildcard.WildcardAll
    | false -> w
  
  let setDlSrc dlSrc pat =
    to_valid { pat with ptrnDlSrc = Wildcard.WildcardExact dlSrc }
  
  let setDlDst dlDst pat =
    to_valid { pat with ptrnDlDst = Wildcard.WildcardExact dlDst }

  let setPort port pat = { pat with ptrnInPort = Wildcard.WildcardExact port }

  let wildcardPort pat = { pat with ptrnInPort = Wildcard.WildcardAll }
  
  let inPort pt =
    { all with ptrnInPort = Wildcard.WildcardExact pt }
  
  let dlSrc mac =
    { all with ptrnDlSrc = Wildcard.WildcardExact mac }

  let dlDst mac =
    { all with ptrnDlDst = Wildcard.WildcardExact mac }

  let dlType typ =
    { all with ptrnDlType = Wildcard.WildcardExact typ } 
  
  let dlVlan vlan =
    { all with ptrnDlVlan = Wildcard.WildcardExact vlan }
  
  let dlVlanPcp pcp =
    { all with ptrnDlVlanPcp = Wildcard.WildcardExact pcp }
  
  let ipSrc ip =
    { all with 
      ptrnDlType = Wildcard.WildcardExact 0x800; 
      ptrnNwSrc = Wildcard.WildcardExact ip }

  let ipDst ip =
    { all with 
      ptrnDlType = Wildcard.WildcardExact 0x800; 
      ptrnNwDst = Wildcard.WildcardExact ip }

  let ipProto proto =
    { all with 
      ptrnDlType = Wildcard.WildcardExact 0x800; 
      ptrnNwProto = Wildcard.WildcardExact proto }
  
  let tpSrcPort proto tpPort =
    { all with 
      ptrnDlType = Wildcard.WildcardExact 0x800; 
      ptrnNwProto = Wildcard.WildcardExact proto; 
      ptrnTpSrc = Wildcard.WildcardExact tpPort }

  let tpDstPort proto tpPort =
    { all with 
      ptrnDlType = Wildcard.WildcardExact 0x800; 
      ptrnNwProto = Wildcard.WildcardExact proto; 
      ptrnTpDst = Wildcard.WildcardExact tpPort }
  
  let tcpSrcPort = tpSrcPort 0x6
  
  let tcpDstPort = tpDstPort 0x6
  
  let udpSrcPort = tpSrcPort 0x7
  
  let udpDstPort = tpDstPort 0x7

  (* JNF: why isn't this in Wildcard? *)
  let wildcard_to_string to_string w =
    let open Wildcard in
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
