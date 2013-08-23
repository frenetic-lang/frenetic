open NetCore_Types

module Compiler = NetCore_Compiler.NetCoreCompiler

(* Yes! This is user-requesting monitoring, so just print to stdout!!! *)
let printf = Printf.printf

let monitor_pol pol = 
  printf "policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol pol);
  pol

let monitor_tbl sw pol = 
  let tbl = Compiler.compile_pol pol sw in
  printf "Flow table at switch %Ld is:\n%!" sw;
  List.iter
    (fun (m,a) -> printf " %s => %s\n%!"
        (NetCore_Pretty.string_of_pattern m)
        (NetCore_Action.Output.string_of_action a))
    tbl;
  pol

let monitor_load (window : float) (label : string) =
  let monitor_load_handler packets bytes =
    printf "[%s] %Ld packets and %Ld bytes in the last %f seconds.\n%!"
      label packets bytes window in
  NetCore_Action.Output.query window monitor_load_handler

let monitor_packets (label : string) : action = 
  let monitor_packet_handler sw port pkt = 
    printf "[%s] packet %s on switch %Ld port %s\n%!"
      label
      (Packet.to_string pkt) 
      sw 
      (NetCore_Pretty.string_of_port port);
    NetCore_Action.Output.drop in
  NetCore_Action.Output.controller monitor_packet_handler
