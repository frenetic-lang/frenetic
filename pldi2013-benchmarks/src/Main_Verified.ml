(* TODO: Bit-rotted file, see VerifiedBenchmarks for everythiing but throughput. *)
open Printf
module PolGen = NetCorePolicyGen

(* Add new policies here. Nothing else should need to change. *)
let select_policy graph (name : string) = 
  match name with
    | "sp" -> PolGen.all_pairs_shortest_paths graph
    | str -> failwith ("invalid policy: " ^ str)


let graph = ref (PolGen.G.empty ())
let policy = ref NetCore.Syntax.Empty
let use_flow_mod = ref false

let _ =
  Arg.parse
    [("-throughput",
      Arg.Unit (fun () ->
        let open PolGen.G in
        let open NetCore.Syntax in
        let open Mininet.Types in
        for i = 1 to 16 do
          add_edge !graph (Switch 1L) i (Switch (Int64.of_int i))
        done;
        let flood = List.fold_left (fun lst a -> (To a)::lst) []
          [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16] in
        policy := Pol (All, flood)),
      "throughput test (do not set -topo or -policy)");
     ("-flowmod",
      Arg.Unit (fun () -> use_flow_mod := true),
      "use a controller that emits flow-mods (defaults to PktOut)");
     ("-topo", 
      Arg.String (fun str -> 
        graph := PolGen.from_mininet str),
      "topology file (output from the net command in Mininet)");
     ("-policy",
       Arg.String (fun name ->
         policy := select_policy !graph name),
      "sp (all pairs shortest paths)")]
    (fun str -> failwith ("invalid argument " ^ str))
    "Usage: Main_Verified.native -topo <filename> -policy <policy-name>"

module Policy : VerifiedNetCore.POLICY = struct
  let switches = PolGen.switches !graph
  let policy = !policy
end

module Controller = VerifiedNetCore.Make (Platform.OpenFlowPlatform) (Policy)

let init = match !use_flow_mod with
  | true -> Controller.init_flow_mod ()
  | false -> Controller.init_packet_out ()

let _ = 
  Platform.OpenFlowPlatform.init_with_port 6633;
  Lwt_main.run (Controller.start init)

