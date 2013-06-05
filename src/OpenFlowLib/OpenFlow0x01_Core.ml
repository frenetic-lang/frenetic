open Packet

type switchId = int64

type portId = int16

type xid = int32

type pattern =  
    { dlSrc : dlAddr option
    ; dlDst : dlAddr option
    ; dlTyp : dlTyp option
    ; dlVlan : dlVlan option
    ; dlVlanPcp : dlVlanPcp option
    ; nwSrc : nwAddr option
    ; nwDst : nwAddr option
    ; nwProto : nwProto option
    ; nwTos : nwTos option
    ; tpSrc : tpPort option
    ; tpDst : tpPort option
    ; inPort : portId option
    }


type pseudoPort =
  | PhysicalPort of portId
  | AllPorts
  | InPort
  | Flood
  | Controller of int


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

type timeout =
  | Permanent
  | ExpiresAfter of int16

type flowModCommand =
  | AddFlow
  | ModFlow
  | ModStrictFlow
  | DeleteFlow 
  | DeleteStrictFlow 

type flowMod =
    { command : flowModCommand
    ; pattern: pattern 
    ; priority : int16
    ; actions : action list
    ; cookie : int64
    ; idle_timeout : timeout
    ; hard_timeout : timeout
    ; notify_when_removed : bool
    ; apply_to_packet : int32 option
    ; out_port : pseudoPort option
    ; check_overlap : bool
    }

type payload =
  | Buffered of int32 * bytes 
  | NotBuffered of bytes

type packetInReason =
  | NoMatch
  | ExplicitSend

type packetIn =
    { input_payload : payload
    ; total_len : int16
    ; port : portId
    ; reason : packetInReason
    }

type packetOut =
    { output_payload : payload
    ; port_id : portId option
    ; apply_actions : action list
    }

let add_flow prio pat actions = 
  { command = AddFlow;
    pattern = pat;
    priority = prio;
    actions = actions;
    cookie = 0L;
    idle_timeout = Permanent;
    hard_timeout = Permanent;
    notify_when_removed = false;
    out_port =  None;
    apply_to_packet = None;
    check_overlap = false
  }

let match_all = {
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

let delete_all_flows =
  { command = DeleteFlow
  ; pattern = match_all
  ; priority = 0
  ; actions = []
  ; cookie = 0L
  ; idle_timeout = Permanent
  ; hard_timeout = Permanent
  ; notify_when_removed = false
  ; apply_to_packet = None
  ; out_port = None
  ; check_overlap = false }


let parse_payload = function
  | Buffered (_, b)
  | NotBuffered b -> 
    Packet.parse b
