open Word

type bytes = Cstruct.t

(** val coq_Const_0x800 : int **)

let coq_Const_0x800 = 0x800

(** val coq_Const_0x806 : int **)

let coq_Const_0x806 = 0x806

(** val coq_Const_0x6 : int **)

let coq_Const_0x6 = 0x6

(** val coq_Const_0x7 : int **)

let coq_Const_0x7 = 0x7

(** val coq_Const_0x1 : int **)

let coq_Const_0x1 = 0x1

type portId = Word16.t

type dlAddr = Word48.t

type dlTyp = Word16.t

type dlVlan = Word16.t option

type dlVlanPcp = Word8.t

type nwAddr = Word32.t

type nwProto = Word8.t

type nwTos = Word8.t

type tpPort = Word16.t

type tcp = 
  { tcpSrc : tpPort; 
    tcpDst : tpPort; 
    tcpSeq : Word32.t;
    tcpAck : Word32.t; 
    tcpOffset : Word8.t; 
    tcpFlags : Word16.t;
    tcpWindow : Word16.t; 
    tcpChksum : Word8.t; 
    tcpUrgent : Word8.t;
    tcpPayload : bytes }

type icmp = 
  { icmpType : Word8.t; 
    icmpCode : Word8.t; 
    icmpChksum : Word16.t;
    icmpPayload : bytes }

type tpPkt =
| TpTCP of tcp
| TpICMP of icmp
| TpUnparsable of nwProto * bytes

type ip = 
  { pktIPVhl : Word8.t; 
    pktIPTos : nwTos; 
    pktIPLen : Word16.t;
    pktIPIdent : Word16.t; 
    pktIPFlags : Word8.t;
    pktIPFrag : Word16.t; 
    pktIPTtl : Word8.t; 
    pktIPProto : nwProto;
    pktIPChksum : Word16.t; 
    pktIPSrc : nwAddr; 
    pktIPDst : nwAddr;
    pktTpHeader : tpPkt }

type arp =
| ARPQuery of dlAddr * nwAddr * nwAddr
| ARPReply of dlAddr * nwAddr * dlAddr * nwAddr

type nw =
| NwIP of ip
| NwARP of arp
| NwUnparsable of dlTyp * bytes

type packet = 
  { pktDlSrc : dlAddr; 
    pktDlDst : dlAddr; 
    pktDlTyp : dlTyp;
    pktDlVlan : dlVlan; 
    pktDlVlanPcp : dlVlanPcp;
    pktNwHeader : nw }

let pktNwSrc pkt = match pkt.pktNwHeader with
  | NwIP ip -> ip.pktIPSrc
  | NwARP (ARPQuery (_,ip,_)) -> ip
  | NwARP (ARPReply (_,ip,_,_)) -> ip
  | NwUnparsable _ -> Word32.zero

let pktNwDst pkt = match pkt.pktNwHeader with
  | NwIP ip -> ip.pktIPDst
  | NwARP (ARPQuery (_,_,ip)) -> ip
  | NwARP (ARPReply (_,_,_,ip)) -> ip
  | NwUnparsable _ -> Word32.zero

let pktNwProto pkt = match pkt.pktNwHeader with 
  | NwIP ip -> ip.pktIPProto
  | _ -> Word8.zero

let pktNwTos pkt = match pkt.pktNwHeader with 
  | NwIP ip -> ip.pktIPTos
  | _ -> Word8.zero

let pktTpSrc pkt = match pkt.pktNwHeader with 
  | NwIP ip ->
    (match ip.pktTpHeader with
    | TpTCP frg -> frg.tcpSrc
    | _ -> Word16.zero)
  | _ -> Word16.zero

let pktTpDst pkt = match pkt.pktNwHeader with 
  | NwIP ip ->
    (match ip.pktTpHeader with
    | TpTCP frg -> frg.tcpDst
    | _ -> Word16.zero)
  | _ -> Word16.zero

let setDlSrc pkt dlSrc =
  { pkt with pktDlSrc = dlSrc }

let setDlDst pkt dlDst =
  { pkt with pktDlDst = dlDst }

let setDlVlan pkt dlVlan =
  { pkt with pktDlVlan = dlVlan }

let setDlVlanPcp pkt dlVlanPcp =
  { pkt with pktDlVlanPcp = dlVlanPcp }

let nw_setNwSrc nwPkt src = match nwPkt with
  | NwIP ip ->
    NwIP { ip with pktIPSrc = src }
  | nw -> 
    nw

let nw_setNwDst nwPkt dst = match nwPkt with
  | NwIP ip ->
    NwIP { ip with pktIPDst = dst }
  | nw -> 
    nw

let nw_setNwTos nwPkt tos =
  match nwPkt with
  | NwIP ip ->
    NwIP { ip with pktIPTos = tos }
  | nw -> 
    nw
    
let setNwSrc pkt nwSrc =
  { pkt with pktNwHeader = nw_setNwSrc pkt.pktNwHeader nwSrc }

let setNwDst pkt nwDst = 
  { pkt with pktNwHeader = nw_setNwDst pkt.pktNwHeader nwDst }

let setNwTos pkt nwTos =
  { pkt with pktNwHeader = nw_setNwTos pkt.pktNwHeader nwTos }

let tp_setTpSrc tp src = match tp with 
  | TpTCP tcp ->
    TpTCP { tcp with tcpSrc = src } (* JNF: checksum? *)
  | tp -> 
    tp

let tp_setTpDst tp dst = match tp with 
  | TpTCP tcp ->
    TpTCP { tcp with tcpDst = dst } (* JNF: checksum? *)
  | tp -> 
    tp

let nw_setTpSrc nwPkt tpSrc = match nwPkt with 
  | NwIP ip ->
    NwIP { ip with pktTpHeader = tp_setTpSrc ip.pktTpHeader tpSrc }
  | nw -> 
    nw

let nw_setTpDst nwPkt tpDst = match nwPkt with 
  | NwIP ip ->
    NwIP { ip with pktTpHeader = tp_setTpDst ip.pktTpHeader tpDst }
  | nw -> 
    nw

let setTpSrc pkt tpSrc =
  { pkt with pktNwHeader = nw_setTpSrc pkt.pktNwHeader tpSrc }

let setTpDst pkt tpDst =
  { pkt with pktNwHeader = nw_setTpDst pkt.pktNwHeader tpDst }

let portId_to_string = Word16.to_string

let dlAddr_to_string = Misc.string_of_mac

let dlTyp_to_string = Word16.to_string

let dlVlan_to_string = Misc.string_of_option Word16.to_string

let dlVlanPcp_to_string = Word8.to_string

let nwAddr_to_string = Word32.to_string

let nwProto_to_string = Word8.to_string

let nwTos_to_string = Word8.to_string

let tpPort_to_string = Word16.to_string

let nw_to_string nw = "NYI"

let packet_to_string 
    { pktDlSrc = pktDlSrc;
      pktDlDst = pktDlDst;
      pktDlTyp = pktDlTyp;
      pktDlVlan = pktDlVlan;
      pktDlVlanPcp = pktDlVlanPcp;
      pktNwHeader = pktNwHeader } = 
  Printf.sprintf "(%s, %s, %s, %s, %s, %s)"
    (dlAddr_to_string pktDlSrc)
    (dlAddr_to_string pktDlDst)
    (dlTyp_to_string pktDlTyp)
    (dlVlan_to_string pktDlVlan)
    (dlVlanPcp_to_string pktDlVlanPcp)
    (nw_to_string pktNwHeader)
