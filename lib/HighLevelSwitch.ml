type t =
  | OF0x01 of HighLevelSwitch0x01.t
  | OF0x04 of HighLevelSwitch0x04.t
  
let initialize (fd : Lwt_unix.file_descr) : t option Lwt.t =
	match_lwt OpenFlow0x01_Switch.handshake fd with
	| None -> Lwt.return None
	| Some h -> Lwt.return (Some (OF0x01 (HighLevelSwitch0x01.from_handle h)))

let setup_flow_table (t : t) = match t with
	| OF0x01 u -> HighLevelSwitch0x01.setup_flow_table u
	| OF0x04 u -> HighLevelSwitch0x04.setup_flow_table u

let flow_stats_request (t : t) = match t with
  | OF0x01 u -> HighLevelSwitch0x01.flow_stats_request u
  | OF0x04 u -> HighLevelSwitch0x04.flow_stats_request u

let packet_in (t : t) = match t with
  | OF0x01 u -> HighLevelSwitch0x01.packet_in u
  | OF0x04 u -> HighLevelSwitch0x04.packet_in u

let packet_out (t : t) = match t with
  | OF0x01 u -> HighLevelSwitch0x01.packet_out u
  | OF0x04 u -> HighLevelSwitch0x04.packet_out u

let disconnect (t : t) = match t with
  | OF0x01 u -> HighLevelSwitch0x01.disconnect u
  | OF0x04 u -> HighLevelSwitch0x04.disconnect u

let features (t : t) = match t with
  | OF0x01 u -> HighLevelSwitch0x01.features u
  | OF0x04 u -> HighLevelSwitch0x04.features u