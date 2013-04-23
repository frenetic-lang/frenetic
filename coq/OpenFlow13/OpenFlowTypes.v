Set Implicit Arguments.

Require Import Coq.Structures.Equalities.
Require Import Word.WordInterface.
Require Import Network.NetworkPacket.

Definition VLAN_NONE : dlVlan := @Word16.Mk 65535 eq_refl.
Extract Constant VLAN_NONE => "65535".

(** Types for OpenFlow 1.3.1, based on
    
    https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.3.1.pdf

    Using this as a guide:

    https://github.com/CPqD/ofsoftswitch13/blob/master/include/openflow/openflow.h

*)

Record mask A := Mask {
  m_value : A;
  m_mask : option A
}.

Definition xid := Word32.t.

Definition val_to_mask {A} (v : A) := Mask v None.

Definition switchId := Word64.t.
Definition groupId  := Word32.t.
Definition portId   := Word32.t.
Definition tableId  := Word8.t.
Definition bufferId := Word32.t.

(** See Table 11 of the specification *)
Inductive oxm : Type := 
  | OxmInPort    :      portId   -> oxm
  | OxmInPhyPort :      portId   -> oxm
  | OxmMetadata  : mask Word64.t -> oxm
  | OxmEthType   :      Word16.t -> oxm
  | OxmEthDst    : mask Word48.t -> oxm
  | OxmEthSrc    : mask Word48.t -> oxm
  | OxmVlanVId   : mask Word12.t -> oxm
  | OxmVlanPcp   :      Word8.t  -> oxm
  | OxmIPProto   :      Word8.t  -> oxm
  | OxmIPDscp    :      Word8.t  -> oxm (* 6 bits *)
  | OxmIPEcn     :      Word8.t  -> oxm (* 2 bits *)
  | OxmIP4Src    : mask Word32.t -> oxm
  | OxmIP4Dst    : mask Word32.t -> oxm
  | OxmTCPSrc    : mask Word16.t -> oxm
  | OxmTCPDst    : mask Word16.t -> oxm
  | OxmARPOp     :      Word16.t -> oxm
  | OxmARPSpa    : mask Word32.t -> oxm
  | OxmARPTpa    : mask Word32.t -> oxm
  | OxmARPSha    : mask Word48.t -> oxm
  | OxmARPTha    : mask Word48.t -> oxm
  | OxmICMPType  :      Word8.t  -> oxm
  | OxmICMPCode  :      Word8.t  -> oxm
  | OxmMPLSLabel :      Word32.t -> oxm (* 20 bits *)
  | OxmMPLSTc    :      Word8.t  -> oxm (* 3 bits *)
  | OxmTunnelId  : mask Word64.t -> oxm.

(**  Hard-codes OFPMT_OXM as the match type, since OFPMT_STANDARD is deprecated.
*)
Definition oxmMatch := list oxm.

Inductive pseudoPort : Type :=
  | PhysicalPort : portId -> pseudoPort
  | InPort : pseudoPort
  | Flood : pseudoPort
  | AllPorts : pseudoPort
  | Controller : Word16.t -> pseudoPort (* number of bytes to send *)
  | Any : pseudoPort.

Inductive action : Type :=
  | Output : pseudoPort -> action
  | Group : groupId -> action
  | PopVlan : action
  | PushVlan : action
  | PopMpls : action
  | PushMpls : action
  | SetField : oxm -> action.

Definition actionSequence := list action.

Inductive instruction : Type :=
  | GotoTable : tableId -> instruction
  | ApplyActions : actionSequence -> instruction
  | WriteActions : actionSequence -> instruction.

Record bucket := Bucket {
  bu_weight : Word16.t;
  bu_watch_port : option portId;
  bu_watch_group : option groupId;
  bu_actions : actionSequence
}.

Inductive groupType : Type :=
  | All : groupType (* All (multicast/broadcast) group. *)
  | Select : groupType (* Select group. *)
  | Indirect : groupType (* Indirect group. *)
  | FF : groupType. (* Fast failover group. *)

Inductive groupMod : Type :=
  | AddGroup : groupType -> groupId -> list bucket -> groupMod
  | DeleteGroup : groupType -> groupId -> groupMod.

Inductive timeout : Type :=
| Permanent : timeout
| ExpiresAfter : forall (n : Word16.t), 
    Word16.to_nat n > Word16.to_nat Word16.zero -> 
    timeout.

Inductive flowModCommand : Type :=
| AddFlow : flowModCommand
| ModFlow : flowModCommand
| ModStrictFlow : flowModCommand
| DeleteFlow : flowModCommand
| DeleteStrictFlow : flowModCommand.

Record flowModFlags : Type := FlowModFlags {
  fmf_send_flow_rem : bool;
  fmf_check_overlap : bool;
  fmf_reset_counts : bool;
  fmf_no_pkt_counts : bool;
  fmf_no_byt_counts : bool
}.

Record flowMod := FlowMod { 
  mfCookie : mask Word64.t;
  mfTable_id : tableId;
  mfCommand : flowModCommand;
  mfIdle_timeout : timeout;
  mfHard_timeout : timeout;
  mfPriority : Word16.t;
  mfBuffer_id : option bufferId;
  mfOut_port : option pseudoPort;
  mfOut_group : option groupId;
  mfFlags : flowModFlags;
  mfOfp_match : oxmMatch;
  mfInstructions : list instruction
}.

Inductive packetInReason : Type :=
| NoMatch : packetInReason
| ExplicitSend : packetInReason.

Record packetIn : Type := PacketIn {
  pi_buffer_id : option Word32.t;
  pi_total_len : Word16.t;
  pi_reason : packetInReason;
  pi_table_id : tableId;
  pi_cookie : Word64.t;
  pi_ofp_match : oxmMatch;
  pi_pkt :  option packet
}.

Record capabilities : Type := Capabilities {
  flow_stats : bool;
  table_stats : bool;
  port_stats : bool; 
  group_stats : bool;
  ip_reasm : bool;
  queue_stats : bool;
  port_blocked : bool
}.

Record features : Type := Features {
  datapath_id : Word64.t; 
  num_buffers : Word32.t;
  num_tables : Word8.t;
  aux_id : Word8.t;
  supported_capabilities : capabilities
}.

Inductive packetOut : Type := PacketOut {
  po_buffer_id : option bufferId;
  po_in_port : pseudoPort;
  po_actions : actionSequence;
  po_pkt : option packet
}.


Inductive message : Type :=
  | Hello : message
  | EchoRequest : bytes -> message
  | EchoReply : bytes -> message
  | FeaturesRequest : message
  | FeaturesReply : features -> message
  | FlowModMsg : flowMod -> message
  | GroupModMsg : groupMod -> message
  | PacketInMsg : packetIn -> message
  | PacketOutMsg : packetOut -> message
  | BarrierRequest : message
  | BarrierReply : message.
