open Core.Std
open Async.Std
open Cohttp_async
open NetKAT_Types
module Server = Cohttp_async.Server
open Common

let policy = ref NetKAT_Types.drop

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
