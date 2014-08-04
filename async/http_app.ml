module NetKAT = NetKAT_Types

module Event = struct
  open NetKAT

  type t = event

  let to_json (event : t) : Yojson.Safe.json =
    match event with
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

  let of_json_string (str : string) : t =
    of_json (Yojson.Safe.from_string str)

  let to_json_string (t : t) : string =
    Yojson.Safe.to_string (to_json t)
end

let create policy host port =
  let open Async.Std in
  let uri = Uri.make ~host ~port () in
  Async_NetKAT.create policy (fun _ _ () e ->
    let open Cohttp_async in
    let body = Body.of_string (Event.to_json_string e) in
    Client.post ~body uri >>= fun (response, body) ->
    match Cohttp.Code.code_of_status (response.Response.status) with
    | 200 ->  (* 200 OK           *)
      Body.to_string body
      >>| Lexing.from_string
      >>| NetKAT_Parser.program NetKAT_Lexer.token
      >>| fun pol -> Some pol
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
    | _ -> assert false)
