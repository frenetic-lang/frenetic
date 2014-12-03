open Core.Std
open Async.Std
open Cohttp_async
open NetKAT_Types
module Server = Cohttp_async.Server

let _ =
  let open Async_OpenFlow.Log in
  set_output [make_filtered_output [("http", "http")]];
  set_level `Info

let printf ?(level: [ `Debug | `Info | `Error ] = `Info) (fmt :  ('a, unit, string, unit) format4) =
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

let handle_request
  ~(body : Cohttp_async.Body.t)
   (client_addr : Socket.Address.Inet.t)
   (request : Request.t) : Server.response Deferred.t =
  match request.meth, extract_path request with
    | `POST, ["update"] ->
      printf "POST /update";
      handle_parse_errors body NetKAT_Json.parse_update
        (fun p -> 
           policy := p;
           Cohttp_async.Server.respond `OK)
    | `GET, [switchId; "flow_table"] -> 
       let sw = Int64.of_string switchId in        
       let t = NetKAT_LocalCompiler.(to_table (compile sw !policy)) in 
       Cohttp_async.Server.respond_with_string (Yojson.Safe.to_string (FlowTable_Json.to_json t))
    | _, _ -> 
       printf "Malformed request from cilent";
       Cohttp_async.Server.respond `Not_found

let listen ?(port=9000) =
  ignore (Cohttp_async.Server.create (Tcp.on_port port) handle_request)

let main () : unit =
  printf ~level:`Info "Starting frenetic-http...";
  listen ~port:9000

let () =
  never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ())
