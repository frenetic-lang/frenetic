(** Library for building the underlying infrastructure of Frenetic controllers.
*)

open Packet
open NetCore_Pattern

type switchId = int64
type portId = int32
type queueId = int32

val string_of_portId : portId -> string
val string_of_switchId : switchId -> string
val string_of_queueId : queueId -> string


type 'a wildcard = 'a NetCore_Wildcard.wildcard

type lp = switchId * port * packet

type port = NetCore_Pattern.port

type ptrn = NetCore_Pattern.t

type 'a match_modify = ('a * 'a) option

(** Note that OpenFlow does not allow the [dlTyp] and [nwProto]
    fields to be modified. *)
type output = {
  outDlSrc : dlAddr match_modify;
  outDlDst : dlAddr match_modify;
  outDlVlan : dlVlan match_modify;
  outDlVlanPcp : dlVlanPcp match_modify;
  outNwSrc : nwAddr match_modify;
  outNwDst : nwAddr match_modify;
  outNwTos : nwTos match_modify;
  outTpSrc : tpPort match_modify;
  outTpDst : tpPort match_modify;
  outPort : port 
}

val id : output

type get_packet_handler = switchId -> port -> packet -> action

(* Packet count -> Byte count -> unit. *)
and get_count_handler = Int64.t -> Int64.t -> unit

and action_atom =
  | SwitchAction of output
  | ControllerAction of get_packet_handler
  | ControllerQuery of float * get_count_handler

and action = action_atom list

type pred =
  | Hdr of ptrn
  | OnSwitch of switchId
  | Or of pred * pred
  | And of pred * pred
  | Not of pred
  | Everything
  | Nothing

type capabilities = { flow_stats : bool; table_stats : bool;
                      port_stats : bool; group_stats : bool; 
                      ip_reasm : bool; queue_stats : bool; 
                      port_blocked : bool }

type switchFeatures = { datapath_id : switchId; num_buffers : int;
                        num_tables : int; supported_capabilities : capabilities;
                        ports : portId list }

type switchEvent =
  | SwitchUp of switchId * switchFeatures
  | SwitchDown of switchId

type pol =
  | HandleSwitchEvent of (switchEvent -> unit)
  | Action of action
  | ActionChoice of action list
  | Filter of pred
  | Union of pol * pol
  | Seq of pol * pol
  | ITE of pred * pol * pol
  | Choice of pol * pol


type value =
  | Pkt of switchId * port * packet * OpenFlow0x01.Payload.t
