open Printf
open OpenFlow0x01Parser
open Platform
open Unix
open MessagesDef
open NetCoreSyntax
open VerifiedNetCore

module PolGen = NetCorePolicyGen
module Z = Mininet

let rec n_switches n = 
  if n = 0L then []
  else n :: (n_switches (Int64.pred n))

module Repeater : POLICY = struct

  open NetCoreSyntax

  let g = PolGen.from_mininet "tree_2_2.topo"

  let switches = PolGen.switches g

  let policy = PolGen.all_pairs_shortest_paths g

end

let _ = printf "%s\n%!" (policy_to_string Repeater.policy)

module Controller = VerifiedNetCore.Make (OpenFlowPlatform) (Repeater)

let _ = OpenFlowPlatform.init_with_port 6633
let _ = Lwt_main.run (Controller.start (Controller.init_packet_out ()))


(*
module Z = VerifiedNetCore
module Controller = Repeater.Make (OpenFlowPlatform)

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
  Sys.catch_break true;
  try 
    OpenFlowPlatform.init_with_port 6633;
    (* Printexc.record_backtrace (); *)
    Lwt_main.run (Controller.start ())
  with exn -> 
    Printf.eprintf "[main] exception: %s\n%s\n%!" 
      (Printexc.to_string exn) (Printexc.get_backtrace ());
    OpenFlowPlatform.shutdown ();
    exit 1
      
let _ = main ()
*)
