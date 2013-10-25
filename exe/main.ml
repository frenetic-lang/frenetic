open GML_Parser
open DOT_Types
open Topology

type modeType =
  | DotMode
  | GmlMode
  | DefaultMode

let infname = ref ""
let mode = ref DefaultMode

let arg_spec =
  [
    ("--dot",
     Arg.Unit (fun () -> mode := DotMode),
     "\tParse a file in DOT format"
    )
    ; ("--gml",
     Arg.Unit (fun () -> mode := GmlMode),
     "\tParse a file in GML format"
    )
]

let usage = Printf.sprintf "usage: %s [OPTIONS] filename" Sys.argv.(0)

let _ =
  Arg.parse arg_spec (fun fn -> infname := fn) usage ;
  Printf.printf "Attempting to topology from file: %s\n%!" !infname;
  let topo = match !mode with
     | DotMode ->
      Printf.printf "Parsing file as DOT format\n";
      DOT_Parser.dot_parse !infname
     | GmlMode ->
      Printf.printf "Parsing file as GML format\n";
      GML_Parser.gml_parse !infname
    | DefaultMode ->
      Printf.printf "Unspecified file format. Parsing as DOT\n";
      DOT_Parser.dot_parse !infname
  in
  Printf.printf "DOT representation: %s\n" (Topology.to_dot topo)
