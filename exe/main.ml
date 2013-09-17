open DOT_Types
open DOT_Parser
open Topology

let _ =
  let fname = Sys.argv.(1) in
  Printf.printf "Attempting to topology from file: %s\n%!" fname;
  let ast = Converter.parse_dotfile fname in
  let topo = Converter.topo_from_ast ast in
  Printf.printf "Dot Topology: \n%s\n" (Topology.to_dot topo);
  Printf.printf "Mininet Script: \n%s\n" (Topology.to_mininet topo);
  topo

