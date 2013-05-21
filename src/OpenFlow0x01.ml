open Packet
open Word
open Misc

(** Internal module, only used to parse the wildcards bitfield *)
module Wildcards = struct

  type t = {
    in_port: bool; 
    dl_vlan: bool;
    dl_src: bool; 
    dl_dst: bool; 
    dl_type: bool; 
    nw_proto: bool; 
    tp_src: bool; 
    tp_dst: bool; 
    nw_src: int; (* XXX *)
    nw_dst: int; (* XXX *)
    dl_vlan_pcp: bool;
    nw_tos: bool;
  }

  let set_nw_mask (f:int32) (off : int) (v : int) : int32 = 
    let value = (0x3f land v) lsl off in
    (Int32.logor f (Int32.of_int value))

  (* TODO(arjun): this is different from mirage *)
  let get_nw_mask (f : int32) (off : int) : int = 
    (Int32.to_int (Int32.shift_right f off)) land 0x3f

  let marshal m = 
    let ret = Int32.zero in 
    let ret = bit ret 0 m.in_port in
    let ret = bit ret 1 m.dl_vlan in 
    let ret = bit ret 2 m.dl_src in
    let ret = bit ret 3 m.dl_dst in
    let ret = bit ret 4 m.dl_type in
    let ret = bit ret 5 m.nw_proto in
    let ret = bit ret 6 m.tp_src in
    let ret = bit ret 7 m.tp_dst in
    let ret = set_nw_mask ret 8 m.nw_src in
    let ret = set_nw_mask ret 14 m.nw_dst in
    let ret = bit ret 20 m.dl_vlan_pcp in 
    let ret = bit ret 21 m.nw_tos in
    ret

  let to_string h = 
    Format.sprintf
      "in_port:%s,dl_vlan:%s,dl_src:%s,dl_dst:%s,dl_type:%s,\
       nw_proto:%s,tp_src:%s,tp_dst:%s,nw_src:%d,nw_dst:%d,\
       dl_vlan_pcp:%s,nw_tos:%s" 
      (string_of_bool h.in_port) 
      (string_of_bool h.dl_vlan) (string_of_bool h.dl_src)
      (string_of_bool h.dl_dst) (string_of_bool h.dl_type)
      (string_of_bool h.nw_proto) (string_of_bool h.tp_src)
      (string_of_bool h.tp_dst) h.nw_src
      h.nw_dst (string_of_bool h.dl_vlan_pcp) 
      (string_of_bool h.nw_tos)
      
  let parse bits = 
    { nw_tos = test_bit 21 bits;
      dl_vlan_pcp = test_bit 20 bits;
      nw_dst = get_nw_mask bits 14; 
      nw_src = get_nw_mask bits 8; 
      tp_dst = test_bit 7 bits; 
      tp_src = test_bit 6 bits; 
      nw_proto = test_bit 5 bits; 
      dl_type = test_bit 4 bits; 
      dl_dst = test_bit 3 bits; 
      dl_src = test_bit 2 bits; 
      dl_vlan = test_bit 1 bits; 
      in_port = test_bit 0 bits;
    }
end

module Match = struct

  type t = {
    dlSrc : dlAddr option; 
    dlDst : dlAddr option;
    dlTyp : dlTyp option; 
    dlVlan : dlVlan option;
    dlVlanPcp : dlVlanPcp option;
    nwSrc : nwAddr option; 
    nwDst : nwAddr option;
    nwProto : nwProto option; 
    nwTos : nwTos option;
    tpSrc : tpPort option; 
    tpDst : tpPort option;
    inPort : portId option 
  }

  cstruct ofp_match {
    uint32_t wildcards;        
    uint16_t in_port;          
    uint8_t dl_src[6];
    uint8_t dl_dst[6];
    uint16_t dl_vlan;          
    uint8_t dl_vlan_pcp;       
    uint8_t pad1[1];           
    uint16_t dl_type;          
    uint8_t nw_tos;            
    uint8_t nw_proto;          
    uint8_t pad2[2];           
    uint32_t nw_src;           
    uint32_t nw_dst;           
    uint16_t tp_src;           
    uint16_t tp_dst
  } as big_endian

  let size = sizeof_ofp_match

  let all = {
    dlSrc = None;
    dlDst = None;
    dlTyp = None;
    dlVlan = None;
    dlVlanPcp = None;
    nwSrc = None; 
    nwDst = None;
    nwProto = None;
    nwTos = None;
    tpSrc = None;
    tpDst = None;
    inPort = None
  }
  let is_none x = match x with
    | None -> true
    | Some _ -> false

  let wildcards_of_match (m : t) : Wildcards.t =
    { Wildcards.in_port = is_none m.inPort;
      Wildcards.dl_vlan = is_none m.dlVlan;
      Wildcards.dl_src = is_none m.dlSrc;
      Wildcards.dl_dst = is_none m.dlDst;
      Wildcards.dl_type = is_none m.dlTyp;
      Wildcards.nw_proto = is_none m.nwProto;
      Wildcards.tp_src = is_none m.tpSrc;
      Wildcards.tp_dst = is_none m.tpDst;
      (* TODO(arjun): support IP prefixes *)
      Wildcards.nw_src = if is_none m.nwSrc then 32 else 0x0;
      Wildcards.nw_dst = if is_none m.nwDst then 32 else 0x0;
      Wildcards.dl_vlan_pcp = is_none m.dlVlanPcp;
      Wildcards.nw_tos = is_none m.nwTos;
  }

  let if_some16 x = match x with
    | Some n -> n
    | None -> 0

  let if_some8 x = match x with
    | Some n -> n
    | None -> 0

  let if_some32 x = match x with
    | Some n -> n
    | None -> 0l

  let if_word48 x = match x with
    | Some n -> n
    | None -> Int64.zero

 let marshal m bits = 
   set_ofp_match_wildcards bits (Wildcards.marshal (wildcards_of_match m));
   set_ofp_match_in_port bits (if_some16 m.inPort); 
   set_ofp_match_dl_src (bytes_of_mac (if_word48 m.dlSrc)) 0 bits;
   set_ofp_match_dl_dst (bytes_of_mac (if_word48 m.dlDst)) 0 bits;
   let vlan = 
     match m.dlVlan with
     | Some (Some v) -> v
     | Some None -> Packet_Parser.vlan_none
     | None -> 0 in
   set_ofp_match_dl_vlan bits (vlan);
   set_ofp_match_dl_vlan_pcp bits (if_some8 m.dlVlanPcp);
   set_ofp_match_dl_type bits (if_some16 m.dlTyp);
   set_ofp_match_nw_tos bits (if_some8 m.nwTos);
   set_ofp_match_nw_proto bits (if_some8 m.nwProto);
   set_ofp_match_nw_src bits (if_some32 m.nwSrc);
   set_ofp_match_nw_dst bits (if_some32 m.nwDst);
   set_ofp_match_tp_src bits (if_some16 m.tpSrc);
   set_ofp_match_tp_dst bits (if_some16 m.tpDst); 
   sizeof_ofp_match 

  let parse bits = 
    let w = Wildcards.parse (get_ofp_match_wildcards bits) in
    { dlSrc = 
        if w.Wildcards.dl_src then 
          None
        else
          Some (mac_of_bytes
                  (Cstruct.to_string (get_ofp_match_dl_src bits)));
      dlDst = 
        if w.Wildcards.dl_dst then 
          None
        else
          Some (mac_of_bytes
                  (Cstruct.to_string (get_ofp_match_dl_dst bits)));
      dlVlan =
        if w.Wildcards.dl_vlan then
          None
        else
          begin
            let vlan = get_ofp_match_dl_vlan bits in
            if vlan = Packet_Parser.vlan_none then 
              Some None 
            else 
              Some (Some vlan)
          end;
      dlVlanPcp = 
        if w.Wildcards.dl_vlan_pcp then
          None
        else
          Some (get_ofp_match_dl_vlan_pcp bits);
      dlTyp =
        if w.Wildcards.dl_type then
          None
        else
          Some (get_ofp_match_dl_type bits);
      nwSrc = 
        if w.Wildcards.nw_src = 0x3f then (* TODO(arjun): prefixes *)
          None
        else
          Some (get_ofp_match_nw_src bits);
      nwDst = 
        if w.Wildcards.nw_dst = 0x3f then (* TODO(arjun): prefixes *)
          None
        else
          Some (get_ofp_match_nw_dst bits);
      nwProto =
        if w.Wildcards.nw_proto then
          None
        else
          Some (get_ofp_match_nw_proto bits);
      nwTos = 
        if w.Wildcards.nw_tos then 
          None
        else 
          Some (get_ofp_match_nw_tos bits);
      tpSrc =
        if w.Wildcards.tp_src then
          None
        else
          Some (get_ofp_match_tp_src bits);
      tpDst =
        if w.Wildcards.tp_dst then
          None
        else
          Some (get_ofp_match_tp_dst bits);
      inPort =
        if w.Wildcards.in_port then
          None
        else
          Some (get_ofp_match_in_port bits);
    }

end

type capabilities = 
  { flow_stats : bool; 
    table_stats : bool;
    port_stats : bool; 
    stp : bool; 
    ip_reasm : bool;
    queue_stats : bool; 
    arp_match_ip : bool }

type actions = { output : bool; 
                 set_vlan_id : bool; 
                 set_vlan_pcp : bool;
                 strip_vlan : bool; 
                 set_dl_src : bool; 
                 set_dl_dst : bool;
                 set_nw_src : bool; 
                 set_nw_dst : bool; 
                 set_nw_tos : bool;
                 set_tp_src : bool; 
                 set_tp_dst : bool; 
                 enqueue : bool;
                 vendor : bool }

type features = 
  { switch_id : Word64.t; 
    num_buffers : Word32.t;
    num_tables : Word8.t;
    supported_capabilities : capabilities;
    supported_actions : actions }

type flowModCommand =
| AddFlow
| ModFlow
| ModStrictFlow
| DeleteFlow
| DeleteStrictFlow

type switchId = Word64.t

let string_of_switchId = Word64.to_string

type priority = Word16.t

type bufferId = Word32.t

type pseudoPort =
| PhysicalPort of portId
| InPort
| Flood
| AllPorts
| Controller of Word16.t

type action =
| Output of pseudoPort
| SetDlVlan of dlVlan
| SetDlVlanPcp of dlVlanPcp
| StripVlan
| SetDlSrc of dlAddr
| SetDlDst of dlAddr
| SetNwSrc of nwAddr
| SetNwDst of nwAddr
| SetNwTos of nwTos
| SetTpSrc of tpPort
| SetTpDst of tpPort

type actionSequence = action list

type timeout =
| Permanent
| ExpiresAfter of Word16.t

type flowMod = 
  { mfModCmd : flowModCommand; 
    mfMatch : Match.t;
    mfPriority : priority; 
    mfActions : actionSequence;
    mfCookie : Word64.t; 
    mfIdleTimeOut : timeout;
    mfHardTimeOut : timeout; 
    mfNotifyWhenRemoved : bool;
    mfApplyToPacket : bufferId option;
    mfOutPort : pseudoPort option; 
    mfCheckOverlap : bool }

type reason = 
| NoMatch
| ExplicitSend

type packetIn = 
  { packetInBufferId : bufferId option;
    packetInTotalLen : Word16.t; 
    packetInPort : portId;
    packetInReason : reason; 
    packetInPacket : packet }

type xid = Word32.t

type packetOut = 
  { pktOutBufOrBytes : (bufferId, bytes) Misc.sum;
    pktOutPortId : portId option;
    pktOutActions : actionSequence }

(* Component types of stats_request messages. *)

type table_id = Word8.t

module IndividualFlowRequest = struct
  type t = { of_match : Match.t;
             table_id : table_id;
             port : pseudoPort }
end

module AggregateFlowRequest = struct
  type t = { of_match : Match.t;
             table_id : table_id;
             port : pseudoPort }
end
  
(* Component types of stats_reply messages. *)

module DescriptionStats = struct
  type t = { manufacturer : string;
             hardware : string;
             software : string;
             serial_number : string;
             datapath : string }
end
  
module IndividualFlowStats = struct
  type t = { table_id : table_id;
             of_match : Match.t;
             duration_sec : int;
             duration_msec : int;
             priority : int;
             idle_timeout : int;
             hard_timeout : int;
             cookie : int;
             byte_count : int;
             actions : actionSequence }
end
  
module AggregateFlowStats = struct
  type t = { packet_count : int;
             byte_count : int;
             flow_count : int }
end
  
module TableStats = struct
  type t = { table_id : table_id;
             name : string;
             wildcards : Word32.t;
             max_entries : int;
             active_count : int;
             lookup_count : int;
             matched_count : int }
end
  
module PortStats = struct
  type t = { port_no : pseudoPort;
             rx_packets : int;
             tx_packets : int;
             rx_bytes : int;
             tx_bytes : int;
             rx_dropped : int;
             tx_dropped : int;
             rx_errors : int;
             tx_errors : int;
             rx_frame_err : int;
             rx_over_err : int;
             rx_crc_err : int;
             collisions : int }
end

type statsRequest =
| DescriptionReq
| IndividualFlowReq of IndividualFlowRequest.t
| AggregateFlowReq of AggregateFlowRequest.t
| TableReq
| PortReq of pseudoPort
(* TODO(cole): queue and vendor stats requests. *)
    
type statsReply =
| DescriptionRep of DescriptionStats.t
| IndividualFlowRep of IndividualFlowStats.t
| AggregateFlowRep of AggregateFlowStats.t
| TableRep of TableStats.t
| PortRep of PortStats.t
    
(* A subset of the OpenFlow 1.0 messages defined in Section 5.1 of the spec. *)
    
type message =
| Hello of bytes
| EchoRequest of bytes
| EchoReply of bytes
| FeaturesRequest
| FeaturesReply of features
| FlowModMsg of flowMod
| PacketInMsg of packetIn
| PacketOutMsg of packetOut
| BarrierRequest
| BarrierReply
| StatsRequestMsg of statsRequest
| StatsReplyMsg of statsReply
  
let delete_all_flows = 
  FlowModMsg {
    mfModCmd = DeleteFlow;
    mfMatch = Match.all;
    mfPriority = 0;
    mfActions = [];
    mfCookie = 0L;
    mfIdleTimeOut = Permanent;
    mfHardTimeOut = Permanent;
    mfNotifyWhenRemoved = false;
    mfApplyToPacket = None;
    mfOutPort = None;
    mfCheckOverlap = false
  }
    
let add_flow match_ actions = 
  FlowModMsg {
    mfModCmd = AddFlow;
    mfMatch = match_;
    mfPriority = 0;
    mfActions = actions;
    mfCookie = 0L;
    mfIdleTimeOut = Permanent;
    mfHardTimeOut = Permanent;
    mfNotifyWhenRemoved = false;
    mfApplyToPacket = None;
    mfOutPort = None;
    mfCheckOverlap = false
  }
  
module type PLATFORM = sig
  exception SwitchDisconnected of switchId 
  val send_to_switch : switchId -> xid -> message -> unit Lwt.t
  val recv_from_switch : switchId -> (xid * message) Lwt.t
  val accept_switch : unit -> features Lwt.t
end

