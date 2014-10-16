module NetKAT = NetKAT_Types

module Event = struct
  open NetKAT

  type t = event

  let to_json (event : t) : Yojson.Safe.json =
    match event with
    | PacketIn (pipe, sw_id, pt_id, payload, len) ->
      let buffer = Base64.encode (Cstruct.to_string (SDN_Types.payload_bytes payload)) in 
      `Assoc [
          ("type", `String "packet_in");
          ("pipe", `String pipe);
          ("switch_id", `Intlit (Int64.to_string sw_id));
          ("port_id",   `Intlit (Int32.to_string pt_id));
          ("payload",   `Assoc [
              ("buffer", `String buffer);
              ("id", match payload with
                | SDN_Types.Buffered(id, _) -> `Intlit (Int32.to_string id)
                | _                         -> `Null)
          ]);
          ("length",    `Intlit (string_of_int len));
      ]
    | Query (name, pkt_count, byte_count) ->
      `Assoc [
          ("type", `String "query");
          ("packet_count", `Intlit (Int64.to_string pkt_count));
          ("byte_count", `Intlit (Int64.to_string byte_count))]
    | SwitchUp sw_id ->
      `Assoc [
          ("type", `String "switch_up");
          ("switch_id", `Intlit (Int64.to_string sw_id))]
    | SwitchDown sw_id ->
      `Assoc [
          ("type", `String "switch_down");
          ("switch_id", `Intlit (Int64.to_string sw_id))]
    | PortUp (sw_id, pt_id) ->
      `Assoc [
          ("type", `String "port_up");
          ("switch_id", `Intlit (Int64.to_string sw_id));
          ("port_id",   `Intlit (Int32.to_string pt_id))]
    | PortDown (sw_id, pt_id) ->
      `Assoc [
          ("type", `String "port_down");
          ("switch_id", `Intlit (Int64.to_string sw_id));
          ("port_id",   `Intlit (Int32.to_string pt_id))]
    | LinkUp ((sw_id1, pt_id1), (sw_id2, pt_id2)) ->
      `Assoc [
          ("type", `String "link_up");
          ("src", `Assoc [("switch_id", `Intlit (Int64.to_string sw_id1));
                          ("port_id",   `Intlit (Int32.to_string pt_id1))]);
          ("dst", `Assoc [("switch_id", `Intlit (Int64.to_string sw_id2));
                          ("port_id",   `Intlit (Int32.to_string pt_id2))])]
    | LinkDown ((sw_id1, pt_id1), (sw_id2, pt_id2)) ->
      `Assoc [
          ("type", `String "link_down");
          ("src", `Assoc [("switch_id", `Intlit (Int64.to_string sw_id1));
                          ("port_id",   `Intlit (Int32.to_string pt_id1))]);
          ("dst", `Assoc [("switch_id", `Intlit (Int64.to_string sw_id2));
                          ("port_id",   `Intlit (Int32.to_string pt_id2))])]
    | HostUp ((sw_id, pt_id), (dlAddr, nwAddr)) ->
      `Assoc [
          ("type", `String "host_up");
          ("switch_id", `Intlit (Int64.to_string sw_id));
          ("port_id",   `Intlit (Int32.to_string pt_id));
          ("dl_addr",   `String (Packet.string_of_dlAddr dlAddr));
          ("nw_addr",   `String (Packet.string_of_nwAddr nwAddr))]
    | HostDown ((sw_id, pt_id), (dlAddr, nwAddr)) ->
      `Assoc [
          ("type", `String "host_down");
          ("switch_id", `Intlit (Int64.to_string sw_id));
          ("port_id",   `Intlit (Int32.to_string pt_id));
          ("dl_addr",   `String (Packet.string_of_dlAddr dlAddr));
          ("nw_addr",   `String (Packet.string_of_nwAddr nwAddr))]


  let of_json (json : Yojson.Safe.json) : t =
    match json with
    | `Assoc [("type", `String "switch_up");
              ("switch_id", `Intlit sw_id)] ->
      SwitchUp (Int64.of_string sw_id)
    | `Assoc [("type", `String "switch_down");
              ("switch_id", `Intlit sw_id)] ->
      SwitchDown (Int64.of_string sw_id)
    | `Assoc [("type", `String "port_up");
              ("switch_id", `Intlit sw_id);
              ("port_id",   `Intlit pt_id)] ->
      PortUp (Int64.of_string sw_id, Int32.of_string pt_id)
    | `Assoc [("type", `String "port_down");
              ("switch_id", `Intlit sw_id);
              ("port_id",   `Intlit pt_id)] ->
      PortDown (Int64.of_string sw_id, Int32.of_string pt_id)
    | _ -> 
      failwith (Printf.sprintf "of_json: unexpected input %s" (Yojson.Safe.to_string json))

  let of_json_string (str : string) : t =
    of_json (Yojson.Safe.from_string str)

  let to_json_string (t : t) : string =
    Yojson.Safe.to_string (to_json t)
end

open Async.Std
open Cohttp_async

let initialize host port path =
  let uri = Uri.make ~host ~port ~path () in
  Client.post uri >>| fun (response, body) ->
  let code = Cohttp.Code.code_of_status (response.Response.status) in 
    if not (code = 201) then
      failwith (Printf.sprintf "unexpected response: %d" code);
    Cohttp.Header.get response.Response.headers "Location"

let handler uri event =
  let body = Body.of_string (Event.to_json_string event) in
  Client.put ~body uri >>= fun (response, body) ->
  match Cohttp.Code.code_of_status (response.Response.status) with
  | 200 ->  (* 200 OK           *)
    begin
      Body.to_string body >>= fun str -> 
      let json = Yojson.Safe.from_string str in 
      match json with 
      | `Assoc [
          ("type", `String "policy");
          ("data", `String data)] -> 
        let lex = Lexing.from_string data in 
        let pol = NetKATParser.program NetKAT_Lexer.token lex in 
        return (Some pol)
      | _ -> 
        failwith "NYI"
    end
  | 202    (* 202 Accepted      *)
  | 204 -> (* 204 No Content    *)
    return None
  | code   (* 2xx Sucessful     *)
      when code >= 200 && code < 300 ->
    return None
  | code   (* 1xx Informational *)
      when code >= 100 && code < 200 ->
    failwith "unhandled informational"
  | code   (* 4xx Client Error  *)
      when code >= 400 && code < 500 ->
    failwith "client error"
  | code   (* 5xx Server Error  *)
      when code >= 500 && code < 600 ->
    failwith "server error"
  | _ -> assert false

let create policy host port ?(path="/netkat/app") () : Async_NetKAT.Policy.t Deferred.t =
  initialize host port path
  >>| function
    | None       -> failwith "no location provided"
    | Some(loc) ->
      let uri = Uri.(of_string (pct_decode loc)) in
      let pipes = Async_NetKAT.PipeSet.singleton "python" in 
      Async_NetKAT.Policy.create ~pipes:pipes policy (fun _ _ () -> handler uri)
