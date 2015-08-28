open Core.Std

let run_types : [
  `Http_Controller | `Compile_Server | `Shell | `Openflow13_Controller | `Openflow13_Fault_Tolerant_Controller
] Command.Spec.Arg_type.t = 
  Command.Spec.Arg_type.create
    (fun run_type_arg ->
      match run_type_arg with 
      | "http-controller" -> `Http_Controller
      | "compile-server" -> `Compile_Server
      | "shell" -> `Shell
      | "openflow13" -> `Openflow13_Controller
      | "openflow13_fault_tolerant" -> `Openflow13_Fault_Tolerant_Controller
      | _ -> 
        eprintf "'%s' is not a legal frenetic command\n" run_type_arg;
        exit 1
    )

let verbosity_levels : Async.Std.Log.Level.t Command.Spec.Arg_type.t = 
  Command.Spec.Arg_type.create
    (function
      | "info" -> `Info
      | "debug" -> `Debug
      | "error" -> `Error
      | verbosity_level -> 
        eprintf "'%s' is not a legal verbosity level.  Choose info (default), debug or error\n" verbosity_level;
        exit 1)

let default_log_device = 
  ("stderr", lazy (Async_extended.Extended_log.Console.output (Lazy.force Async.Std.Writer.stderr)))

let log_outputs : (string * Async.Std.Log.Output.t Lazy.t) Command.Spec.Arg_type.t =
  Command.Spec.Arg_type.create
    (function
      | "stderr" -> default_log_device
      | "stdout" -> ("stdout", lazy (Async_extended.Extended_log.Console.output (Lazy.force Async.Std.Writer.stdout)))
      | filename -> (filename, lazy (Async.Std.Log.Output.file `Text filename)) )
(*
let policy : string Term.t =
  let open Arg in
  let doc = "NetKat policy to apply to the network" in
  value & pos 0 string "" & info [] ~docv:"POLICY" ~doc
*)
let table : Frenetic_NetKAT_Local_Compiler.flow_layout Term.t =
  let open Arg in
  let open Frenetic_Fdd.Field in
  let doc = "Undocumented 1.3 flag" in
  let opts = [ ("switch", Switch); ("vlan", Vlan); ("pcp", VlanPcp);
    ("ethtype", EthType); ("ipproto", IPProto); ("ethsrc", EthSrc);
    ("ethdst", EthDst); ("ip4src", IP4Src); ("ip4dst", IP4Dst);
    ("tcpsrc", TCPSrcPort); ("tcpdst", TCPDstPort); ("location", Location) ] in
  let conv = Arg.list ~sep:';' (Arg.list ~sep:',' (Arg.enum opts)) in
  let default = [Frenetic_NetKAT_Local_Compiler.Field.get_order ()] in
  value & opt conv default & info ["table"] ~docv:"TABLE" ~doc

(* TODO(mulias): cmdliner fiel arg type would be more appropriate than a string 
let policy_file : string Term.t =
  let open Arg in
  let doc = "file contianing NetKat policy to apply to the network" in
  value & pos 0 string "" & info [] ~docv:"POLICY" ~doc
*)
(* TODO(mulias): cmdliner fiel arg type would be more appropriate than a string 
let topology_file : string Term.t =
  let open Arg in
  let doc = "file containing .dot topology of network" in
  value & pos 1 string "" & info [] ~docv:"TOPOLOGY" ~doc
*)

let spec = 
  let open Command.Spec in 
  empty
  +> flag "--http-port" (optional_with_default 9000 int) ~doc:"int HTTP port on which to listen for new policies"
  +> flag "--openflow-port" (optional_with_default 6633 int) ~doc:"int Port to listen on for OpenFlow switches"
  +> flag "--rpc-port" (optional_with_default 8984 int) ~doc:"int TCP port to serve on for communicating with higher-level controller"
  +> flag "--verbosity" (optional_with_default `Info verbosity_levels) ~doc:"level verbosity level = {debug, error, info}"
  +> flag "--log" (optional_with_default default_log_device log_outputs) ~doc: "file path to write logs, 'stdout' or 'stderr'"
  +> flag "--policy" (optional string) ~doc: "NetKat policy to apply to the network"
  +> flag "--table" (optional Frenetic_NetKAT_Local_Compiler.Field.get_order () field_orders) ~doc:"Undocumented 1.3 flag"
  +> flag "--policy-file" (optional file) ~doc: "File contianing NetKat policy to apply to the network"
  +> flag "--topology-file" (optional file) ~doc: "File contianing .dot topology of network"
  +> anon ("[flags] {http-controller | compile_server | shell | openflow13 | fault_tolerant}" %: run_types) 

let command =
  Command.basic
    ~summary: "Frenetic NetKAT-to-OpenFlow compiler"
    spec
    (fun http_port openflow_port rpc_port verbosity log fixed_policy table_fields policy_path topology_path run_type () -> 
      let (log_path, log_output) = log in
      (* Creating an async compatible command runner where the functions have different shapes is messy, 
         hence the duplication here *)
      let main = 
        match run_type with 
        | `Shell -> 
          fun () ->
            Frenetic_Log.set_level verbosity;
            Frenetic_Log.set_output [Lazy.force log_output];
            Frenetic_Shell.main openflow_port ()
        | `Compile_Server -> 
          fun () -> 
            Frenetic_Log.set_level verbosity;
            Frenetic_Log.set_output [Lazy.force log_output];
            Frenetic_Compile_Server.main http_port ()
        | `Http_Controller -> 
          fun () -> 
            Frenetic_Log.set_level verbosity;
            Frenetic_Log.set_output [Lazy.force log_output];
            Frenetic_Http_Controller.main http_port openflow_port () 
        | `Openflow13_Controller -> 
          fun () -> 
            Frenetic_Log.set_level verbosity;
            Frenetic_Log.set_output [Lazy.force log_output];
            Frenetic_Openflow0x04_Controller.main openflow_port policy_path table_fields () 
        | `Openflow13_Fault_Tolerant_Controller -> 
          fun () -> 
            Frenetic_Log.set_level verbosity;
            Frenetic_Log.set_output [Lazy.force log_output];
            Frenetic_Openflow0x04_Controller.fault_tolerant_main openflow_port policy_path topology_path () 
      in
      ignore (main ());
      Core.Std.never_returns (Async.Std.Scheduler.go ())
    )

(*
let openflow13 : unit Term.t * Term.info =
  let open Term in
  let doc = "OpenFlow 1.3 work-in-progress" in
  (async_init (app (app (app (pure Frenetic_OpenFlow0x04_Controller.main)
   openflow_port) policy_file) table), info "openflow13" ~doc)

let fault_tolerant : unit Term.t * Term.info =
  let open Term in
  let doc = "Fault tolerant networking work-in-progress" in
  (async_init (app (app (app (pure Frenetic_OpenFlow0x04_Controller.fault_tolerant_main)
   openflow_port) policy_file) topology_file), 
   info "fault-tolerant" ~doc)
*)
(* Add new commands here. 
let top_level_commands = [
  compile_server;
  http_controller;
  shell;
  openflow13;
  fault_tolerant
]
*)
>>>>>>> master

let () =
  Command.run ~version: "4.0" ~build_info: "RWO" command
