open Core.Std
open Async.Std
open Cohttp_async

let printf ?(level : [ `Debug | `Info | `Error ] = `Info) (fmt :  ('a, unit, string, unit) format4) =
  Async_OpenFlow.Log.printf ~level ~tags:[("http", "http")] fmt

(* Extract the path, split on slashes, and remove empty strings caused by
   repeated slashes *)
let extract_path (req : Request.t) : string list =
    List.filter ~f:(fun str -> not (String.is_empty str))
      (String.split ~on:'/'
         (Uri.path req.uri))

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
      printf "Error parsing message from client";
      Cohttp_async.Server.respond `Bad_request

let parse_update body = Body.to_string body >>= fun str ->
  let json = Yojson.Basic.from_string str in
  match json with
  | `String data ->
    let lex = Lexing.from_string data in
    let pol = NetKAT_Parser.program NetKAT_Lexer.token lex in
    return pol
  | _ -> failwith "expected a NetKAT policy string"
