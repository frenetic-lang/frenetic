open Core
open Async
open Cohttp_async

let profile f =
  let t1 = Unix.gettimeofday () in
  let r = f () in
  let t2 = Unix.gettimeofday () in
  (t2 -. t1, r)


let printf ?(level : [ `Debug | `Info | `Error ] = `Info)
  (fmt :  ('a, unit, string, unit) format4) =
  Logging.printf ~level ~tags:[("frenetic", "http")] fmt

(* Extract the path, split on slashes, and remove empty strings caused by
   repeated slashes *)
let extract_path (req : Request.t) : string list =
    List.filter ~f:(fun str -> not (String.is_empty str))
      (String.split ~on:'/'
         (Uri.path (Request.uri req)))

(* Note: cannot create a variable called parser because of some Camlp4 thing
   we are using. *)
let handle_parse_errors
  (body : Cohttp_async.Body.t)
  (body_parser : Cohttp_async.Body.t -> 'a Deferred.t)
  (handler : 'a -> Cohttp_async.Server.response Deferred.t) :
  Cohttp_async.Server.response Deferred.t =
  try_with (fun () -> body_parser body)
  >>= function
  | Ok x -> handler x
  | Error exn ->
      printf ~level:`Error "Invalid message from client %s" (Exn.to_string exn);
      Cohttp_async.Server.respond `Bad_request

let handle_parse_errors'
  (body : Cohttp_async.Body.t)
  (body_parser : string -> 'a)
  (handler : 'a -> Cohttp_async.Server.response Deferred.t) :
  Cohttp_async.Server.response Deferred.t =
  Body.to_string body
  >>= fun body_str ->
  try_with (fun () -> return (body_parser body_str))
  >>= function
  | Ok x -> handler x
  | Error exn ->
      printf ~level:`Error "Invalid message from client %s:\n%s" (Exn.to_string exn) body_str;
      Cohttp_async.Server.respond `Bad_request

let parse_update body = Body.to_string body >>= fun pol_str ->
  return (Frenetic_netkat.Parser.pol_of_string pol_str)

let parse_update_json body =
  Body.to_string body >>= fun str ->
  return (Frenetic_netkat.Json.pol_of_json_string str)

let parse_config_json body =
  Body.to_string body >>= fun str ->
  return (Frenetic_netkat.Local_compiler.options_from_json_string str)
