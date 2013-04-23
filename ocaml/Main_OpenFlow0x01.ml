open Printf
open Unix
open OpenFlow0x01Types

module Controller = Modules.Repeater.Make (OpenFlow0x01.Platform)

(* configuration state *)
let controller = ref "learn"

(* command-line arguments *)
let arg_specs = 
  [ ("-c", 
     Arg.Set_string controller, 
     "<controller> run a specific controller")
  ]
 
let arg_rest rest = ()

let usage = 
  "desmoines [options]"

let () = Arg.parse arg_specs arg_rest usage

let main () = 
  let pol = Marshal.from_channel (open_in "out") in
  let handlers = Hashtbl.create 100 in
  let core_pol = NetCoreSyntax.desugar_policy pol handlers in
  printf "%s\n%!" (NetCoreSyntax.policy_to_string pol);
  let tbl = NetCoreCompiler.compile_opt core_pol 1L in
  printf "Classifier length:%d\n%!" (List.length tbl)
      
let _ = main ()
