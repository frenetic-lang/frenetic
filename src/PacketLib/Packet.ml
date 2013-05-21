type bytes = Cstruct.t

type int8 = int

type int16 = int

type int48 = int64

type portId = int16

type dlAddr = int48

type dlTyp = int16

type dlVlan = int16 option

type dlVlanPcp = int8

type nwAddr = int32

type nwProto = int8

type nwTos = int8

type tpPort = int16

type tcp = 
  { tcpSrc : tpPort; 
    tcpDst : tpPort; 
    tcpSeq : int32;
    tcpAck : int32; 
    tcpOffset : int8; 
    tcpFlags : int16;
    tcpWindow : int16; 
    tcpChksum : int8; 
    tcpUrgent : int8;
    tcpPayload : bytes }

type icmp = 
  { icmpType : int8; 
    icmpCode : int8; 
    icmpChksum : int16;
    icmpPayload : bytes }

type tpPkt =
| TpTCP of tcp
| TpICMP of icmp
| TpUnparsable of nwProto * bytes

type ip = 
  { pktIPVhl : int8; 
    pktIPTos : nwTos; 
    pktIPLen : int16;
    pktIPIdent : int16; 
    pktIPFlags : int8;
    pktIPFrag : int16; 
    pktIPTtl : int8; 
    pktIPProto : nwProto;
    pktIPChksum : int16; 
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
  | NwUnparsable _ -> Int32.zero

let pktNwDst pkt = match pkt.pktNwHeader with
  | NwIP ip -> ip.pktIPDst
  | NwARP (ARPQuery (_,_,ip)) -> ip
  | NwARP (ARPReply (_,_,_,ip)) -> ip
  | NwUnparsable _ -> Int32.zero

let pktNwProto pkt = match pkt.pktNwHeader with 
  | NwIP ip -> ip.pktIPProto
  | _ -> 0

let pktNwTos pkt = match pkt.pktNwHeader with 
  | NwIP ip -> ip.pktIPTos
  | _ -> 0

let pktTpSrc pkt = match pkt.pktNwHeader with 
  | NwIP ip ->
    (match ip.pktTpHeader with
    | TpTCP frg -> frg.tcpSrc
    | _ -> 0)
  | _ -> 0

let pktTpDst pkt = match pkt.pktNwHeader with 
  | NwIP ip ->
    (match ip.pktTpHeader with
    | TpTCP frg -> frg.tcpDst
    | _ -> 0)
  | _ -> 0

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

let get_byte (n:int64) (i:int) : int =
  if i < 0 or i > 5 then
    raise (Invalid_argument "Int64.get_byte index out of range");
  Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical n (8 * i)))

let string_of_mac (x:int64) : string =
  Format.sprintf "%02x:%02x:%02x:%02x:%02x:%02x"
    (get_byte x 5) (get_byte x 4) (get_byte x 3)
    (get_byte x 2) (get_byte x 1) (get_byte x 0)

let bytes_of_mac (x:int64) : string =
  let byte n = Char.chr (get_byte x n) in
  Format.sprintf "%c%c%c%c%c%c"
    (byte 5) (byte 4) (byte 3)
    (byte 2) (byte 1) (byte 0)

let mac_of_bytes (str:string) : int64 =
  if String.length str != 6 then
    raise (Invalid_argument
             (Format.sprintf "mac_of_bytes expected six-byte string, got %d
                              bytes" (String.length str)));
  let byte n = Int64.of_int (Char.code (String.get str n)) in
  let open Int64 in
  logor (shift_left (byte 0) (8 * 5))
    (logor (shift_left (byte 1) (8 * 4))
       (logor (shift_left (byte 2) (8 * 3))
          (logor (shift_left (byte 3) (8 * 2))
             (logor (shift_left (byte 4) (8 * 1))
                (byte 5)))))

let get_byte32 (n : Int32.t) (i : int) : int = 
  let open Int32 in
  if i < 0 or i > 3 then
    raise (Invalid_argument "get_byte32 index out of range");
  to_int (logand 0xFFl (shift_right_logical n (8 * i)))

let string_of_ip (ip : Int32.t) : string = 
  Format.sprintf "%d.%d.%d.%d" (get_byte32 ip 3) (get_byte32 ip 2) 
    (get_byte32 ip 1) (get_byte32 ip 0)

let portId_to_string = string_of_int

let dlAddr_to_string = string_of_mac

let dlTyp_to_string = string_of_int

let dlVlan_to_string = function
  | None -> "None"
  | Some n -> "Some " ^ string_of_int n

let dlVlanPcp_to_string = string_of_int

let nwAddr_to_string = Int32.to_string

let nwProto_to_string = string_of_int

let nwTos_to_string = string_of_int

let tpPort_to_string = string_of_int

let nw_to_string nw = "Not yet implemented"

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
