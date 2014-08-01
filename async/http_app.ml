module NetKAT = NetKAT_Types

module Event = struct
  type t = NetKAT.event
  open NetKAT

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
end

let create policy host port =
  let open Async.Std in
  let uri = Uri.make ~host ~port () in
  Async_NetKAT.create policy (fun _ _ () e ->
    let open Cohttp_async in
    let body = Body.of_string (Yojson.Safe.to_string (Event.to_json e)) in
    Client.post ~body uri >>= fun (response, body) ->
    match response.Response.status with
    | `Code 200 ->
      Body.to_string body
      >>| Lexing.from_string
      >>| NetKAT_Parser.program NetKAT_Lexer.token
      >>| fun pol -> Some pol
    | `Code 201 ->
      return None
    | _ ->
      failwith "error")
