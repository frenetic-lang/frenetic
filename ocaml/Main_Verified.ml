open Printf
module PolGen = NetCorePolicyGen

(* Add new policies here. Nothing else should need to change. *)
let select_policy graph (name : string) = 
  match name with
    | "sp" -> PolGen.all_pairs_shortest_paths graph
    | str -> failwith ("invalid policy: " ^ str)


let graph = ref (PolGen.G.empty ())
let policy = ref NetCoreSyntax.Empty
let _ =
  Arg.parse
    [("-topo", 
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

let _ = 
  Platform.OpenFlowPlatform.init_with_port 6633;
  Lwt_main.run (Controller.start (Controller.init_packet_out ()))

