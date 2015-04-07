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
    | `POST, ["compile"] ->
      printf "POST /compile";
      handle_parse_errors body
        (fun body ->
           Body.to_string body >>= fun str ->
           return (NetKAT_Json.policy_from_json_string str))
        (fun pol ->
         let fdd = NetKAT_LocalCompiler.compile pol in
         let sws = NetKAT_Misc.switches_of_policy pol in
         let sws = if List.length sws = 0 then [0L] else sws in
         let tbls = List.map sws ~f:(fun sw ->
          let tbl_json = NetKAT_SDN_Json.flowTable_to_json
            (NetKAT_LocalCompiler.to_table sw fdd) in
          `Assoc [("switch_id", `Int (Int64.to_int_exn sw));
                  ("tbl", tbl_json)]) in
         let resp = Yojson.Basic.to_string ~std:true (`List tbls) in
         Cohttp_async.Server.respond_with_string resp)
    | `POST, ["update"] ->
      printf "POST /update";
      handle_parse_errors body parse_update_json
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

let main (http_port : int) () : unit = listen ~port:http_port

