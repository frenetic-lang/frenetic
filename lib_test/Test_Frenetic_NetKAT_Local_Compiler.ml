open OUnitHack
open Frenetic_OpenFlow
open Frenetic_NetKAT
open Frenetic_NetKAT_Pretty
open Frenetic_NetKAT_Compiler

TEST "Can test locations, even when they are set to pipes" =
  let p = Filter (Test (Location (Pipe "web"))) in
  let opt = { default_compiler_options with remove_tail_drops = false } in
  List.length (to_table 0L ~options:opt (compile_local ~options:opt p)) == 1 (* that drops everything *)

TEST "clearing cache fails" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile_local (Filter a) in
  let compiler_options_empty = { default_compiler_options with cache_prepare = `Empty } in
  let compiler_options_keep = { default_compiler_options with cache_prepare = `Keep } in
  let fdd2 = compile_local ~options:compiler_options_empty (Filter b) in
  try
    seq fdd1 fdd2 != compile_local ~options:compiler_options_keep (Filter (And (a, b)))
  with
    Not_found -> true

TEST "keeping cache_prepare works" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile_local (Filter a) in
  let compiler_options_keep = { default_compiler_options with cache_prepare = `Keep } in
  let fdd2 = compile_local ~options:compiler_options_keep (Filter b) in
  seq fdd1 fdd2 = compile_local ~options:compiler_options_keep (Filter (And (a, b)))

TEST "keeping reachable nodes in cache_prepare works" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile_local (Filter a) in
  let compiler_options_keep = { default_compiler_options with cache_prepare = `Keep } in
  let compiler_options_preserve = { default_compiler_options with cache_prepare = `Preserve fdd1 } in
  let fdd2 = compile_local ~options:compiler_options_preserve (Filter b) in
  seq fdd1 fdd2 = compile_local ~options:compiler_options_keep (Filter (And (a, b)))
