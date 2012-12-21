Set Implicit Arguments.

Require Import Coq.Structures.Equalities.

Require Import Word.WordInterface.

Axiom bytes : Type.

Extract Constant bytes => "Cstruct.buf".

Definition portId := Word16.t.
Definition dlAddr := Word48.t.
Definition dlTyp := Word16.t.
Definition dlVlan := Word16.t.
Definition dlVlanPcp := Word8.t. (* 3 bits *)
Definition nwAddr := Word32.t.
Definition nwProto := Word8.t.
Definition nwTos := Word8.t. (** 6 bits *)
Definition tpPort := Word16.t.

Record of_match : Type := Match {
  matchDlSrc : option dlAddr;
  matchDlDst : option dlAddr;
  matchDlTyp : option dlTyp;
  matchDlVlan : option dlVlan;
  matchDlVlanPcp : option dlVlanPcp;
  matchNwSrc : option nwAddr;
  matchNwDst : option nwAddr;
  matchNwProto : option nwProto;
  matchNwTos : option nwTos;
  matchTpSrc : option tpPort;
  matchTpDst : option tpPort;
  matchInPort : option portId
}.

Record capabilities : Type := Capabilities {
  flow_stats: bool;   
  table_stats: bool;
  port_stats: bool;    
  stp: bool;
  ip_reasm: bool;
  queue_stats: bool;    
  arp_match_ip: bool
}.

Record actions : Type := Actions {
  output: bool;
  set_vlan_id: bool;
  set_vlan_pcp: bool;
  strip_vlan: bool;
  set_dl_src: bool;
  set_dl_dst: bool;
  set_nw_src: bool;
  set_nw_dst: bool;
  set_nw_tos: bool;
  set_tp_src: bool;
  set_tp_dst: bool;
  enqueue: bool;
  vendor: bool
}.

Record features : Type := Features {
  switch_id : Word64.t;
  num_buffers: Word32.t;
  num_tables: Word8.t;
  supported_capabilities: capabilities;
  supported_actions: actions
  (* TODO(arjun): physical ports go here  *)
}.

Definition xid : Type := Word32.t.

Inductive message : Type :=
| Hello : bytes -> message
| EchoRequest : bytes -> message
| EchoReply : bytes -> message
| FeaturesRequest : message
| FeaturesReply : features -> message.


(*
Inductive OnlyFromSwitchMessage : Type :=
| BarrierReply : OnlyFromSwitchMessage
| PacketIn : option BufferID -> Port -> Packet -> OnlyFromSwitchMessage.

Inductive OnlyFromControllerMessage : Type :=
| ModifyFlow : ModFlowRecord -> OnlyFromControllerMessage
| BarrierRequest : OnlyFromControllerMessage
| PacketOut : (BufferID + Packet) -> 
    option Port -> 
    ActionSequence -> 
    OnlyFromControllerMessage.

Definition FromSwitchMessage := 
  (SymmetricMessage + OnlyFromSwitchMessage) % type.

Definition FromControllerMessage := 
  (SymmetricMessage + OnlyFromControllerMessage) % type.
*)
