open Printf
open Lwt
module PolGen = NetCorePolicyGen

(* Add new policies here. Nothing else should need to change. *)
let select_policy graph (name : string) = 
  match name with
    | "sp" -> PolGen.all_pairs_shortest_paths graph
    | str -> failwith ("invalid policy: " ^ str)


let custom = ref None
let topo = ref "tree,2,2"
let policy_name = ref "sp"
let use_flow_mod = ref false
let pcap_file = ref None

let _ =
  Arg.parse
    [("-flowmod",
      Arg.Unit (fun () -> use_flow_mod := true),
      "send FlowMod messages (defaults to PktOut)");
     ("-custom",
      Arg.String (fun str -> custom := Some str),
      "custom topology file for Mininet");
     ("-topo", 
      Arg.String (fun str -> topo := str),
      "topology string for Mininet");
     ("-policy",
      Arg.String (fun str -> policy_name := str),
      "sp (all pairs shortest paths)");
     ("-pcap",
      Arg.String (fun str -> pcap_file := Some str),
      "Save tcpdump of 6633")
    ]
    (fun str -> failwith ("invalid argument " ^ str))
    "Usage: Main_Verified.native -topo <filename> -policy <policy-name>"

let start_tcpdump (pcap_file : string) : unit = 
  let pid = 
    Unix.create_process "sudo"
      [| "sudo"; "tcpdump"; "-w"; pcap_file; "-i"; "any"; "tcp port 6633" |]
      Unix.stdin Unix.stdout Unix.stderr in
  let sigint_tcpdump () = 
    Unix.kill pid Sys.sigint;
    (* Let tcpdump finish and close the pcap file before we read it back. *)
    let _ = Unix.waitpid [] pid in
    let _ = Unix.system ("capinfos " ^ pcap_file) in
    () in
  at_exit sigint_tcpdump
    
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

  let _ = match !pcap_file with
    | None -> ()
    | Some fname -> start_tcpdump fname in
  let _ = Platform.OpenFlowPlatform.init_with_port 6633 in
  lwt _ = Controller.start init in
  lwt _ = Lwt_list.map_s (fun _ -> Mininet.ping_all mn) [1;2;3;4;5;6] in
  lwt _= Lwt_io.printf "done?\n%!" in
  return ()

let _ = Lwt_main.run main in ()
                  

    
