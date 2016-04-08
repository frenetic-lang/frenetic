open Core.Std
open Async.Std
open Cohttp_async
open Frenetic_NetKAT
module Server = Cohttp_async.Server
open Frenetic_Common
module Comp = Frenetic_NetKAT_Compiler

let policy = ref Frenetic_NetKAT.drop

let current_compiler_options = ref { Comp.default_compiler_options with optimize = false }

let compile_respond pol =
  (* Compile pol to tables and time everything. *)
  let (time, tbls) = profile (fun () ->
  let fdd = Comp.compile_local ~options:!current_compiler_options pol in
  let sws =
    let sws = Frenetic_NetKAT_Semantics.switches_of_policy pol in
    if List.length sws = 0 then [0L] else sws in
  List.map sws ~f:(fun sw ->
    (sw, Comp.to_table ~options:!current_compiler_options sw fdd))) in
  (* JSON conversion is not timed. *)
  let json_tbls = List.map tbls ~f:(fun (sw, tbl) ->
  `Assoc [("switch_id", `Int (Int64.to_int_exn sw));
         ("tbl", Frenetic_NetKAT_SDN_Json.flowTable_to_json tbl)]) in
  let resp = Yojson.Basic.to_string ~std:true (`List json_tbls) in
  let headers = Cohttp.Header.init_with
  "X-Compile-Time" (Float.to_string time) in
  Cohttp_async.Server.respond_with_string ~headers resp

let handle_request
  ~(body : Cohttp_async.Body.t)
   (client_addr : Socket.Address.Inet.t)
   (request : Request.t) : Server.response Deferred.t =
  match request.meth, extract_path request with
    | `POST, ["compile_pretty"] -> handle_parse_errors body
        parse_update
        compile_respond
    | `POST, ["compile"] ->
      printf "POST /compile";
      handle_parse_errors body
        (fun body ->
           Body.to_string body >>= fun str ->
           return (Frenetic_NetKAT_Json.policy_of_json_string str))
        compile_respond
    | `POST, ["update"] ->
      printf "POST /update";
      handle_parse_errors body parse_update_json
        (fun p ->
           policy := p;
           Cohttp_async.Server.respond `OK)
    | `GET, [switchId; "flow_table"] ->
       let sw = Int64.of_string switchId in
       Comp.compile_local ~options:!current_compiler_options !policy |>
         Comp.to_table ~options:!current_compiler_options sw |>
         Frenetic_NetKAT_SDN_Json.flowTable_to_json |>
         Yojson.Basic.to_string ~std:true |>
         Cohttp_async.Server.respond_with_string
    | `POST, ["config"] ->
       printf "POST /config";
       handle_parse_errors body parse_config_json
        (fun conf -> current_compiler_options := conf; Cohttp_async.Server.respond `OK)
    | `GET, ["config"] ->
       printf "GET /config";
       Comp.options_to_json_string !current_compiler_options |>
       Cohttp_async.Server.respond_with_string
    | _, _ ->
       printf "Malformed request from cilent";
       Cohttp_async.Server.respond `Not_found

let listen ?(port=9000) () =
  ignore (Cohttp_async.Server.create (Tcp.on_port port) handle_request)

let main (http_port : int) () : unit = listen ~port:http_port ()

