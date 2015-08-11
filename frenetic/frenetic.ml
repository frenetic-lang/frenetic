open Core.Std

let run_types : [`Http_Server | `Compile_Server | `Shell] Command.Spec.Arg_type.t = 
  Command.Spec.Arg_type.create
    (fun run_type_arg ->
      match run_type_arg with 
      | "http-server" -> `Http_Server
      | "compile-server" -> `Compile_Server
      | "shell" -> `Shell
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
        eprintf "'%s' is not a legal verbosity level.  Choose info, debug or error\n" verbosity_level;
        exit 1)

let default_actions : [`Drop | `Controller | `Nop] Command.Spec.Arg_type.t = 
  Command.Spec.Arg_type.create
    (fun run_type_arg ->
      match run_type_arg with 
      | "drop" -> `Drop
      | "controller" -> `Controller
      | "nop" -> `Nop
      | _ -> 
        eprintf "'%s' is not a legal default action.  Choose drop, controller or nop\n" run_type_arg;
        exit 1
    )

let default_log_device = 
  ("stderr", lazy (Async_extended.Extended_log.Console.output (Lazy.force Async.Std.Writer.stderr)))

let log_outputs : (string * Async.Std.Log.Output.t Lazy.t) Command.Spec.Arg_type.t =
  Command.Spec.Arg_type.create
    (function
      | "stderr" -> default_log_device
      | "stdout" -> ("stdout", lazy (Async_extended.Extended_log.Console.output (Lazy.force Async.Std.Writer.stdout)))
      | filename -> (filename, lazy (Async.Std.Log.Output.file `Text filename)) )

let spec = 
  let open Command.Spec in 
  empty
  +> flag "--http-port" (optional_with_default 9000 int) ~doc:"int HTTP port on which to listen for new policies"
  +> flag "--openflow-port" (optional_with_default 6633 int) ~doc:"int Port to listen on for OpenFlow switches"
  +> flag "--rpc-port" (optional_with_default 8984 int) ~doc:"int TCP port to serve on for communicating with higher-level controller"
  +> flag "--openflow-log" (optional_with_default "./openflow.log" file) ~doc:"string log path"
  +> flag "--openflow-executable" (optional_with_default "./openflow.native" file) ~doc:"file path to openflow executable"
  +> flag "--verbosity" (optional_with_default `Info verbosity_levels) ~doc:"level verbosity level = {debug, error, info}"
  +> flag "--log" (optional_with_default default_log_device log_outputs) ~doc: "file path to write logs, 'stdout' or 'stderr'"
  +> flag "--default-action" (optional_with_default `Drop default_actions) ~doc: "action default action for policy misses = {drop, controller, nop}"
  +> anon ("[flags] {http_server | compile_server | shell}" %: run_types) 

let command =
  Command.basic
    ~summary: "Frenetic NetKAT-to-OpenFlow compiler"
    spec
    (fun http_port openflow_port rpc_port openflow_log openflow_executable verbosity log default_action run_type () -> 
      let (log_path, log_output) = log in
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
        | `Http_Server -> 
          fun () -> 
            Frenetic_Log.set_level verbosity;
            Frenetic_Log.set_output [Lazy.force log_output];
            Frenetic_Http_Controller.main http_port openflow_port openflow_executable openflow_log () 
      in
      ignore (main ());
      Core.Std.never_returns (Async.Std.Scheduler.go ())
    )

let () =
  Command.run ~version: "4.0" ~build_info: "RWO" command
