open Printf
open Lwt
module PolGen = NetCorePolicyGen

(* Add new policies here. Nothing else should need to change. *)
let select_policy graph (name : string) = 
  match name with
    | "sp" -> PolGen.all_pairs_shortest_paths graph
    | str -> failwith ("invalid policy: " ^ str)


let custom = ref None
let topo = ref ""
let policy_name = ref ""
let use_flow_mod = ref false

let _ =
  Arg.parse
    [("-flowmod",
      Arg.Unit (fun () -> use_flow_mod := true),
      "use a controller that emits flow-mods (defaults to PktOut)");
     ("-custom",
      Arg.String (fun str -> custom := Some str),
      "custom topology file for Mininet");
     ("-topo", 
      Arg.String (fun str -> topo := str),
      "topology string for Mininet");
     ("-policy",
      Arg.String (fun str -> policy_name := str),
      "sp (all pairs shortest paths)")]
    (fun str -> failwith ("invalid argument " ^ str))
    "Usage: Main_Verified.native -topo <filename> -policy <policy-name>"

let main = 
  lwt mn = Mininet.create_mininet_process (?custom:!custom) !topo in
  lwt net = Mininet.net mn in
  let graph = PolGen.from_mininet_raw net in
  let module Policy : VerifiedNetCore.POLICY = struct
    let switches = PolGen.switches graph
    let policy = select_policy graph !policy_name
  end in
  let module Controller = 
        VerifiedNetCore.Make (Platform.OpenFlowPlatform) (Policy) in
  let init = match !use_flow_mod with
    | true -> Controller.init_flow_mod ()
    | false -> Controller.init_packet_out () in
  let _ = Platform.OpenFlowPlatform.init_with_port 6633 in
  lwt _ = Controller.start init in
  lwt _ = Mininet.ping_all mn in
  return ()

let _ = Lwt_main.run main in ()
                  

    
