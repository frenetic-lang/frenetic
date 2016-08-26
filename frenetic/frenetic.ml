open Core.Std

(*===========================================================================*)
(* AUXILLIARY FUNCTIONS                                                      *)
(*===========================================================================*)

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

(*===========================================================================*)
(* FLAGS                                                                     *)
(*===========================================================================*)

module Flag = struct
  open Command.Spec

  let verbosity =
    flag "--verbosity" (optional_with_default `Info verbosity_levels)
      ~doc:"level verbosity level = {debug, error, info}"

  let log =
    flag "--log" (optional_with_default default_log_device log_outputs)
      ~doc:"file path to write logs, 'stdout' or 'stderr'"

  let http_port =
    flag "--http-port" (optional_with_default 9000 int)
      ~doc:"int HTTP port on which to listen for new policies. Defaults to 9000."

  let openflow_port =
    flag "--controller-port" (optional_with_default 6633 int)
      ~doc:"int Port to listen on for OpenFlow switches. Defaults to 6633."

  let table_fields =
    flag "--table" (optional_with_default "policy.tbl" file)
      ~doc:"Partition of fields into P4 tables, e.g. location;ethsrc,ethdst;ipsrc,ipdst,..."

  let policy_file =
    flag "--policy" (optional_with_default "policy.kat" file)
    ~doc:"File containing NetKAT policy to apply to the network. Defaults to \"policy.kat\"."

  let topology_file =
    flag "--topology" (optional_with_default "topology.dot" file)
      ~doc:"File containing .dot topology of network. Defaults to \"topology.kat\"."
end


(*===========================================================================*)
(* COMMANDS                                                                  *)
(*===========================================================================*)

let default_spec =
  Command.Spec.(empty +> Flag.verbosity +> Flag.log)

let run cmd verbosity log =
  let (log_path, log_output) = log in
  Frenetic_Log.set_level verbosity;
  Frenetic_Log.set_output [Lazy.force log_output];
  ignore (cmd ());
  never_returns (Async.Std.Scheduler.go ())

let shell : Command.t =
  Command.basic
    ~summary:"Invokes frenetic shell."
    Command.Spec.(empty
      +> Flag.openflow_port
      ++ default_spec)
    (fun openflow_port ->
      run (Frenetic_Shell.main openflow_port))

let compile_server : Command.t =
  Command.basic
    ~summary:"Invokes compile server."
    Command.Spec.(empty
      +> Flag.http_port
      ++ default_spec)
    (fun http_port ->
      run (Frenetic_Compile_Server.main http_port))

let http_controller : Command.t =
  Command.basic
    ~summary:"Invokes http controler."
    Command.Spec.(empty
      +> Flag.http_port
      +> Flag.openflow_port
      ++ default_spec)
    (fun http_port openflow_port ->
      run (Frenetic_Http_Controller.main http_port openflow_port))

let openflow13_controller : Command.t =
  Command.basic
    ~summary:"Invokes openflow 1.3 controller."
    Command.Spec.(empty
      +> Flag.openflow_port
      +> Flag.policy_file
      ++ default_spec)
    (fun openflow_port policy_file ->
      run (Frenetic_OpenFlow0x04_Plugin.main openflow_port policy_file [Frenetic_Fdd.Field.get_order ()]))

let p4_controller : Command.t =
  Command.basic
    ~summary:"Invokes P4 controller."
    Command.Spec.(empty
      +> Flag.policy_file
      +> Flag.table_fields
      ++ default_spec)
    (fun policy_file table_fields ->
      run (Frenetic_P4_Plugin.main policy_file table_fields))

let main : Command.t =
  Command.group
    ~summary:"Invokes the specified Frenetic module."
    [ ("shell", shell)
    ; ("compile-server", compile_server)
    ; ("http-controller", http_controller)
    ; ("openflow13", openflow13_controller)
    ; ("p4", p4_controller)
    ; ("dump", Dump.main)]

let () =
  Command.run ~version: "5.0" ~build_info: "RWO" main
