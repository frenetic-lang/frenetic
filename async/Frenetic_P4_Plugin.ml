open Core.Std
open Async.Std

module Log = Frenetic_Log

open Frenetic_NetKAT_Compiler

let main pol_file layout () = 
  Log.info "Starting P4 controller";
  Log.info "Policy File: %s" pol_file;
  Log.info "Table Layout: %s" (Multitable.layout_to_string layout);
  let pol_str = In_channel.read_all pol_file in 
  let pol = Frenetic_NetKAT_Parser.policy_of_string pol_str in 
  let compiler_opts = {default_compiler_options with field_order = `Static (List.concat layout)} in
  let fdd = compile_local pol ~options:compiler_opts in
  Log.info "Done!";
  ()
