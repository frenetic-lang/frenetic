open Core.Std

let run_types : [
  `Http_Controller | `Compile_Server | `Shell |
  `Openflow13_Controller | `Openflow13_Fault_Tolerant_Controller |
  `Dump
] Command.Spec.Arg_type.t =
  Command.Spec.Arg_type.create
    (fun run_type_arg ->
      match run_type_arg with
      | "http-controller" -> `Http_Controller
      | "compile-server" -> `Compile_Server
      | "shell" -> `Shell
      | "openflow13" -> `Openflow13_Controller
      | "fault_tolerant" -> `Openflow13_Fault_Tolerant_Controller
      | "dump" -> `Dump
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

let table_fields : Frenetic_NetKAT_Compiler.flow_layout Command.Spec.Arg_type.t =
  let open Frenetic_Fdd.Field in
  Command.Spec.Arg_type.create
    (fun table_field_string ->
      let opts = [ ("switch", Switch); ("vlan", Vlan); ("pcp", VlanPcp);
        ("ethtype", EthType); ("ipproto", IPProto); ("ethsrc", EthSrc);
        ("ethdst", EthDst); ("ip4src", IP4Src); ("ip4dst", IP4Dst);
        ("tcpsrc", TCPSrcPort); ("tcpdst", TCPDstPort); ("location", Location) ] in
      (* Break each table def into a string of fields ["ethsrc,ethdst", "ipsrc,ipdst"] *)
      let table_list = Str.split (Str.regexp "[;]" ) table_field_string in
      (* Break each string of fields into a list of fields: [["ethsrc","ethdst"],["ipsrc","ipdst"]] *)
      let field_list_list = List.map ~f:(fun t_str -> Str.split (Str.regexp "[,]") t_str) table_list in
      (* This takes a field list [ ethsrc,ethdst ] and converts to Field.t definition *)
      let table_to_fields = List.map ~f:(fun f_str -> List.Assoc.find_exn opts f_str) in
      (* Applies the above to each table definition *)
      List.map ~f:table_to_fields field_list_list
    )

let spec =
  let open Command.Spec in
  empty
  +> flag "--http-port" (optional_with_default 9000 int) ~doc:"int HTTP port on which to listen for new policies"
  +> flag "--openflow-port" (optional_with_default 6633 int) ~doc:"int Port to listen on for OpenFlow switches"
  +> flag "--rpc-port" (optional_with_default 8984 int) ~doc:"int TCP port to serve on for communicating with higher-level controller"
  +> flag "--verbosity" (optional_with_default `Info verbosity_levels) ~doc:"level verbosity level = {debug, error, info}"
  +> flag "--log" (optional_with_default default_log_device log_outputs) ~doc: "file path to write logs, 'stdout' or 'stderr'"
  +> flag "--policy" (optional string) ~doc: "NetKat policy to apply to the network"
  +> flag "--table" (optional_with_default [Frenetic_Fdd.Field.get_order ()] table_fields) ~doc:"Partition of fields into Openflow 1.3 tables, e.g. ethsrc,ethdst;ipsrc,ipdst"
  +> flag "--policy-file" (optional_with_default "policy.kat" file) ~doc: "File containing NetKat policy to apply to the network"
  +> flag "--topology-file" (optional_with_default "topology.dot" file) ~doc: "File containing .dot topology of network"
  +> anon ("[flags] {http-controller | compile-server | shell | openflow13 | fault_tolerant | dump}" %: run_types)

let command : Command.t =
  Command.basic
    ~summary: "Frenetic NetKAT-to-OpenFlow compiler"
    spec
    (fun http_port openflow_port rpc_port verbosity log fixed_policy table_fields
        policy_path topology_path run_type () ->
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
            Frenetic_OpenFlow0x04_Controller.main openflow_port policy_path table_fields ()
        | `Openflow13_Fault_Tolerant_Controller ->
          fun () ->
            Frenetic_Log.set_level verbosity;
            Frenetic_Log.set_output [Lazy.force log_output];
            Frenetic_OpenFlow0x04_Controller.fault_tolerant_main openflow_port policy_path topology_path ()
        | `Dump -> fun () -> ()
      in
      ignore (main ());
      Core.Std.never_returns (Async.Std.Scheduler.go ())
    )

let main : Command.t =
  Command.group
    ~summary:"Invokes the specified Frenetic module."
    [("old", command); ("dump", Dump.main)]

let () =
  Command.run ~version: "5.0" ~build_info: "RWO" main
