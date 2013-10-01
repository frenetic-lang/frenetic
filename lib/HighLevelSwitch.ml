open Cstruct

type t =
  | OF0x01 of HighLevelSwitch0x01.t
  | OF0x04 of HighLevelSwitch0x04.t

exception Unparsable of string

module OFHandshake = struct

  module Header = struct

    type t = 
      { ver : int;
        typ : int;
        len : int;
        xid : int32 }

    cstruct ofp_header {
      uint8_t version;    
      uint8_t typ;   
      uint16_t length;    
      uint32_t xid
    } as big_endian

    let size = sizeof_ofp_header
    let ver hdr = hdr.ver
    let typ hdr = hdr.typ
    let len hdr = hdr.len

    (** [parse buf] assumes that [buf] has size [sizeof_ofp_header]. *)
    let parse buf_str =
      let buf = Cstruct.of_string buf_str in
      { ver = get_ofp_header_version buf;
        typ = get_ofp_header_typ buf;
        len = get_ofp_header_length buf;
        xid = get_ofp_header_xid buf
      }

  end

  let recv_hello_from_switch_fd (fd : Lwt_unix.file_descr) : Header.t option Lwt.t =
    let ofhdr_str = String.create Header.size in
    match_lwt Socket.recv fd ofhdr_str 0 Header.size with
      | false -> Lwt.return None
      | true ->
        lwt hdr = Lwt.wrap (fun () -> Header.parse ofhdr_str) in
        let body_len = Header.len hdr - Header.size in
        let body_buf = String.create body_len in
        match_lwt Socket.recv fd body_buf 0 body_len with
          | false -> Lwt.return None
          | true ->
            begin
              match Header.typ hdr with
              | 0 (* HELLO *) -> Lwt.return (Some hdr)
              | _ -> raise (Unparsable "unrecognized message code")
            end

  let switch_handshake (fd : Lwt_unix.file_descr) : t option Lwt.t =
    match_lwt recv_hello_from_switch_fd fd with
    | Some hdr ->
      begin
        match Header.ver hdr with
        | 0x01 ->
          begin
            match_lwt OpenFlow0x01_Switch.handshake fd with
          	| None -> Lwt.return None
          	| Some h -> Lwt.return (Some (OF0x01 (HighLevelSwitch0x01.from_handle h)))
          end
        | 0x04 -> 
          begin
            match_lwt OpenFlow0x04_Switch.handshake fd with
          	| None -> Lwt.return None
          	| Some h -> Lwt.return (Some (OF0x04 (HighLevelSwitch0x04.from_handle h)))
          end
        | _ -> raise (Unparsable "unrecognized version")
      end
    | None ->
      Lwt.return None

end

let initialize (fd : Lwt_unix.file_descr) : t option Lwt.t =
  OFHandshake.switch_handshake fd

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
