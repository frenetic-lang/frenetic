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

let topology_name : Topologies.topo_name Command.Spec.Arg_type.t =
  let open Topologies in
  let num_arg = ",\\([1-9][0-9]*\\)" in
  let tree = Str.regexp ("tree" ^ num_arg ^ num_arg ^ "$") in
  let linear = Str.regexp ("linear" ^ num_arg ^ "$") in
  let single = Str.regexp ("single" ^ num_arg ^ "$") in
  let minimal = Str.regexp "minimal$" in
  Command.Spec.Arg_type.create
    (function x ->
      if Str.string_match tree x 0 then
        Tree (Int.of_string (Str.matched_group 1 x), Int.of_string (Str.matched_group 2 x))
      else if Str.string_match linear x 0 then
        Linear (Int.of_string (Str.matched_group 1 x))
      else if Str.string_match single x 0 then
        Single (Int.of_string (Str.matched_group 1 x))
      else if Str.string_match minimal x 0 then
        Minimal
      else
        (eprintf "'%s' is not a legal topology name.  Choose 'tree,n,o', 'linear,n', 'single,n', 'minimal' \n" x;
        exit 1))

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
    flag "--openflow-port" (optional_with_default 6633 int)
      ~doc:"int Port to listen on for OpenFlow switches. Defaults to 6633."

  let table_fields =
    flag "--table" (optional_with_default [Frenetic_Fdd.Field.get_order ()] table_fields)
      ~doc:"Partition of fields into Openflow 1.3 tables, e.g. ethsrc,ethdst;ipsrc,ipdst"

  let policy_file =
    flag "--policy-file" (optional_with_default "policy.kat" file)
    ~doc:"File containing NetKAT policy to apply to the network. Defaults to \"policy.kat\"."

  let topology_file =
    flag "--topology-file" (optional_with_default "topology.dot" file)
      ~doc:"File containing .dot topology of network. Defaults to \"topology.kat\"."

  let topology_name =
    flag "--topo" (required topology_name)
      ~doc:"topology_name The name of the topology. Same as mn --topo value."

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
    ~summary:"Invokes openflow 1.3 controler."
    Command.Spec.(empty
      +> Flag.openflow_port
      +> Flag.policy_file
      +> Flag.table_fields
      ++ default_spec)
    (fun openflow_port policy_file table_fields ->
      run (Frenetic_OpenFlow0x04_Plugin.main openflow_port policy_file table_fields))

let openflow13_fault_tolerant_controller : Command.t =
  Command.basic
    ~summary:"Invokes fault-tolerant openflow 1.3 controler."
    Command.Spec.(empty
      +> Flag.openflow_port
      +> Flag.policy_file
      +> Flag.topology_file
      ++ default_spec)
    (fun openflow_port policy_file topology_file ->
      run (Frenetic_OpenFlow0x04_Plugin.fault_tolerant_main
        openflow_port policy_file topology_file))

let start_controller : Command.t =
  Command.basic
    ~summary:"Starts a controller with specified topology and installed rules generated from portless policy."
    Command.Spec.(empty
                  +> Flag.openflow_port
                  +> Flag.topology_name
                  +> Flag.policy_file
                  ++ default_spec)
    (fun openflow_port topology_name policy_file ->
       run (
         let pol = Frenetic_NetKAT_Portless_Parser.pol_of_file policy_file in
         let topo = Topologies.topo_from_name topology_name in

         let module Controller = Frenetic_NetKAT_Controller.Make (Frenetic_OpenFlow0x01_Plugin) in
         Controller.start openflow_port;
         Async.Std.Deferred.don't_wait_for (Controller.update (Frenetic_NetKAT_Portless_Compiler.compile pol topo));
         never_returns (Async.Std.Scheduler.go ());
       )
    )

let main : Command.t =
  Command.group
    ~summary:"Invokes the specified Frenetic module."
    [ ("shell", shell)
    ; ("compile-server", compile_server)
    ; ("http-controller", http_controller)
    ; ("openflow13", openflow13_controller)
    ; ("fault-tolerant", openflow13_fault_tolerant_controller)
    ; ("dump", Dump.main)
    ; ("start-controller", start_controller)]

let () =
  Frenetic_Util.pp_exceptions ();
  Command.run ~version: "5.0" ~build_info: "RWO" main
