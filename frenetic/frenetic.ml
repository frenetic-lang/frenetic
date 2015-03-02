open Core.Std
open Cmdliner

let http_port : int Term.t =
  let open Arg in
  let doc = "The HTTP port on which to listen for new policies" in
  value & opt int 9000 & info ["http-port"] ~docv:"PORT" ~doc

let openflow_port : int Term.t =
  let open Arg in
  let doc = "The OpenFlow port to switch switches connect" in
  value & opt int 6633 & info ["openflow-port"] ~docv:"PORT" ~doc

let verbosity : [ `Debug | `Error | `Info ] Term.t =
  let open Arg in
  let level = enum [("info", `Info); ("debug", `Debug); ("error", `Error)] in
  let doc = "Set logging verbosity" in
  value & opt level `Info & info ["verbosity"] ~docv:"LEVEL" ~doc

(* Starts the async scheduler and sets up the async logger. *)
let async_init (cmd : (unit -> unit) Term.t) : unit Term.t =
  let open Async.Std in
  let open Term in
  let cmd' (verbosity : [ `Debug | `Error | `Info ]) (f : unit -> unit) : unit =
    let main () =
      Async_OpenFlow.Log.set_level verbosity;
      f () in
    never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ()) in
  pure cmd' $ verbosity $ cmd

let compile_server : unit Term.t * Term.info =
  let open Term in
  let doc = "Run the compile server" in
  (async_init (pure Compile_Server.main $ http_port),
   info "compile-server" ~doc)

let http_controller : unit Term.t * Term.info =
  let open Term in
  let doc = "Run the HTTP controller" in
  (async_init (pure Http_Controller.main $ http_port $ openflow_port),
   info "http-controller" ~doc)

(* Add new commands here. *)
let top_level_commands = [
  compile_server;
  http_controller
]

let () =
  match Term.eval_choice compile_server top_level_commands with
  | `Error _ -> exit 1
  | _ -> exit 0
