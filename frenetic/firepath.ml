open Core.Std
open Frenetic_NetKAT
open Frenetic_Network
open Frenetic_Circuit_NetKAT

module Shell  = Frenetic_Autoshell
module Fabric = Frenetic_Fabric
module Parser = Frenetic_NetKAT_Parser
module Compiler = Frenetic_NetKAT_Compiler
module Log = Frenetic_Log

let topo_path = "examples/firepath/topo.kat"
let circ_path = "examples/firepath/circuit.kat"
let fab_path  = "examples/firepath/fabric.kat"
let pol_path  = "examples/firepath/policy.pkat"
let bounce_path = "examples/firepath/bounce.kat"

let fedge = [(4L,2l); (4L,3l); (5L,2l); (5L,3l); (6L,3l); (6L,4l)]

let compile =
  let open Compiler in
  compile_local ~options:{ default_compiler_options with cache_prepare = `Keep }

let union = Frenetic_NetKAT_Optimize.mk_big_union

let bounce = Parser.policy_of_file bounce_path

let () =
  let topo = Parser.policy_of_file topo_path in
  let circuit = Parser.policy_of_file circ_path |>
                config_of_policy in
  let fab = match circuit with
    | Error e -> failwith e
    | Ok c -> local_policy_of_config c in
  let fabric = Fabric.assemble fab topo fedge fedge in
  let path_string =
    try In_channel.input_all (In_channel.create pol_path)
    with Sys_error msg -> failwith msg in
  match Fabric.Path.of_string path_string with
  | Error e -> failwith e
  | Ok paths ->
    try
      let ins, outs = Fabric.Path.project paths (Fabric.Dyad.of_policy fabric) topo in
      let edge = Frenetic_NetKAT.Union (union ins, union outs)in
      Shell.install_fdd (compile fab)  [4L;5L;6L] |> Shell.print_deferred_results;
      Shell.install_fdd (compile edge) [1L;2L;3L] |> Shell.print_deferred_results;
      Shell.install_fdd (compile bounce) [7L]     |> Shell.print_deferred_results;
      never_returns (Async.Std.Scheduler.go ())
    with Fabric.IncompletePlace msg ->
      print_endline msg
