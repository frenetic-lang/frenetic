open Core.Std
open Async.Std
open Cohttp_async
open NetKAT_Types
module Server = Cohttp_async.Server

let printf ?(level : [ `Debug | `Info | `Error ] = `Info) (fmt :  ('a, unit, string, unit) format4) =
  Async_OpenFlow.Log.printf ~level ~tags:[("http", "http")] fmt

let extract_path (req : Request.t) : string list =
    List.filter ~f:(fun str -> not (String.is_empty str))
      (String.split ~on:'/'
         (Uri.path req.uri))

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

let policy = ref NetKAT_Types.drop

let parse_update body = Body.to_string body >>= fun str ->
  let json = Yojson.Basic.from_string str in
  match json with
  | `String data ->
    let lex = Lexing.from_string data in
    let pol = NetKAT_Parser.program NetKAT_Lexer.token lex in
    return pol
  | _ -> failwith "expected a NetKAT policy string"

let handle_request
  ~(body : Cohttp_async.Body.t)
   (client_addr : Socket.Address.Inet.t)
   (request : Request.t) : Server.response Deferred.t =
  match request.meth, extract_path request with
    | `POST, ["update"] ->
      printf "POST /update";
      handle_parse_errors body parse_update
        (fun p ->
           policy := p;
           Cohttp_async.Server.respond `OK)
    | `GET, [switchId; "flow_table"] ->
       let sw = Int64.of_string switchId in
       NetKAT_LocalCompiler.compile !policy |>
         NetKAT_LocalCompiler.to_table sw |>
         NetKAT_SDN_Json.flowTable_to_json |>
         Yojson.Basic.to_string ~std:true |>
         Cohttp_async.Server.respond_with_string
    | _, _ ->
       printf "Malformed request from cilent";
       Cohttp_async.Server.respond `Not_found

let listen ?(port=9000) =
  ignore (Cohttp_async.Server.create (Tcp.on_port port) handle_request)

let main (args : string list) : unit = match args with
  | [ "--port"; p ] | [ "-p"; p ] ->
    listen ~port:(Int.of_string p)
  | [] -> listen ~port:9000
  |  _ -> (print_endline "Invalid command-line arguments"; Shutdown.shutdown 1)
