open Core.Std
open Async.Std
open Frenetic_NetKAT
open Frenetic_Network
open Frenetic_Circuit_NetKAT

module Shell  = Frenetic_Autoshell
module Parser = Frenetic_NetKAT_Parser
module Fabric = Frenetic_Fabric
module Compiler = Frenetic_NetKAT_Compiler
module Log = Frenetic_Log

let topo_path = "examples/firepath/topo.kat"
let circ_path = "examples/firepath/circuit.kat"
let fab_path  = "examples/firepath/fabric.kat"
let pol_path  = "examples/firepath/policy.pkat"
let bounce_path = "examples/firepath/bunce.kat"

let fins = []
let fouts = []

let compile_local =
  let open Compiler in
  compile_local ~options:{ default_compiler_options with cache_prepare = `Keep }

let union = Frenetic_NetKAT_Optimize.mk_big_union

let bounce = Parser.policy_of_file bounce_path |> compile_local

let () =
  let topo = Parser.policy_of_file topo_path in
  let fabric = Fabric.assemble (Parser.policy_of_file fab_path) topo fins fouts in
  let circuit = Parser.policy_of_file circ_path |>
                config_of_policy in
  let path_string = try
      let chan = In_channel.create pol_path in
      In_channel.input_all chan
    with Sys_error msg -> failwith msg in
  match Fabric.paths_of_string path_string with
    | Error e -> failwith e
    | Ok paths ->
      let ins, outs = Fabric.project paths (Fabric.streams_of_policy fabric) topo in
      let edge    = Frenetic_NetKAT.Union (union ins, union outs) in
      let fdd = compile_local edge in
      Shell.install_fdd fdd [1L;2L;3L] |> Shell.print_deferred_results;
      Shell.install_fdd bounce [4L] |> Shell.print_deferred_results;
