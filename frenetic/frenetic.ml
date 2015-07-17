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

let log_output : (string * Async.Std.Log.Output.t Lazy.t) Term.t =
  let open Async.Std in
  let open Async_extended in
  let open Arg in
  let stderr_output = ("stderr", lazy (Extended_log.Console.output (Lazy.force Writer.stderr))) in
  let a_parser (str : string) = match str with
    | "stderr" -> `Ok stderr_output
    | "stdout" -> `Ok ("stdout", lazy (Extended_log.Console.output (Lazy.force Writer.stdout)))
    | filename -> `Ok (filename, lazy (Log.Output.file `Text filename)) in
  let a_printer fmt (str, _) = Format.pp_print_string fmt str in
  value (opt (a_parser, a_printer) stderr_output (info ["log"]))

(* Starts the async scheduler and sets up the async logger. *)
let async_init (cmd : (unit -> unit) Term.t) : unit Term.t =
  let open Async.Std in
  let open Term in
  let cmd' (verbosity : [ `Debug | `Error | `Info ])
           ((_, log_output) : (string * Log.Output.t Lazy.t))
           (f : unit -> unit) : unit =
    let main () =
      Frenetic_Log.set_level verbosity;
      Frenetic_Log.set_output [Lazy.force log_output];
      f () in
    never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ()) in
  app (app (app (pure cmd') verbosity) log_output) cmd

let compile_server : unit Term.t * Term.info =
  let open Term in
  let doc = "Run the compile server" in
  (async_init (app (pure Frenetic_Compile_Server.main) http_port),
   info "compile-server" ~doc)

let http_controller : unit Term.t * Term.info =
  let open Term in
  let doc = "Run the HTTP controller" in
  (async_init (app (app (pure Frenetic_Http_Controller.main) http_port) openflow_port),
   info "http-controller" ~doc)

let virtual_cmd : unit Term.t * Term.info =
  let open Term in  
  let doc = "run virtual compiler to produce fabric" in
  let vpolicy =
    let doc = "file containing the local virtual policy (containing no links)" in
    Arg.(required & (pos 0 (some file) None) & info [] ~docv:"VPOLICY" ~doc)
  in
  let vrel =
    let doc = "file containing the virtual relation" in
    Arg.(required & (pos 1 (some file) None) & info [] ~docv:"VREL" ~doc)
  in
  let vtopo =
    let doc = "file containing the virtual topology" in
    Arg.(required & (pos 2 (some file) None) & info [] ~docv:"VTOPO" ~doc)
  in
  let ving_pol =
    let doc = "file containing the virtual ingress policy" in
    Arg.(required & (pos 3 (some file) None) & info [] ~docv:"VINGRESSPOL" ~doc)
  in
  let ving =
    let doc = "file containing the virtual ingress predicate" in
    Arg.(required & (pos 4 (some file) None) & info [] ~docv:"VINGRESS" ~doc)
  in
  let veg =
    let doc = "file containing the virtual eggress predicate" in
    Arg.(required & (pos 5 (some file) None) & info [] ~docv:"VINGRESS" ~doc)
  in
  let ptopo =
    let doc = "file containing the virtual topology" in
    Arg.(required & (pos 6 (some file) None) & info [] ~docv:"PTOPO" ~doc)
  in
  let ping =
    let doc = "file containing the virtual ingress predicate" in
    Arg.(required & (pos 7 (some file) None) & info [] ~docv:"PINGRESS" ~doc)
  in
  let peg =
    let doc = "file containing the virtual eggress predicate" in
    Arg.(required & (pos 8 (some file) None) & info [] ~docv:"PEGRESS" ~doc)
  in
  (async_init ( app (app (app (app (app (app (app (app (app (pure Frenetic_Virtual.main) vpolicy) vrel) vtopo)  ving_pol) ving) veg) ptopo) ping)  peg),
  info "virtual" ~doc)


let shell : unit Term.t * Term.info =
  let open Term in
  let doc = "Run the Frenetic Shell" in
  (async_init (app (app (pure Frenetic_Shell.main) http_port) openflow_port),
   info "shell" ~doc)


(* Add new commands here. *)
let top_level_commands = [
  compile_server;
  http_controller;
  shell;
  virtual_cmd
]

let () =
  match Term.eval_choice compile_server top_level_commands with
  | `Error _ -> exit 1
  | _ -> exit 0
