open! Core
open Probnetkat
open Probnetkat.Syntax
open Frenetic.Network

module Parameters = struct
  (* switch field *)
  let sw = "sw"

  (* port field *)
  let pt = "pt"

  (* up bit associated with link *)
  let up sw pt = sprintf "up_%d" pt

  (* link failure probabilities *)
  let failure_prob _sw _pt = PNK.(1 // 10)

  (* topology *)
  let topo = Topology.parse (Sys.argv.(1))

  let egress = PNK.(
    ???(sw, 1) & ???(pt, 1)
  )

  (* the actual program to run on the switches *)
  let sw_pol sw _inpt =
    (* random walk *)
    Net.Topology.vertex_to_ports topo sw
    |> Util.tap ~f:(fun pts ->
      printf "switch %d has %d outports\n" (Topology.sw_val topo sw) (Set.length pts))
    |> Set.to_list
    |> List.map ~f:(fun out_pt_id -> PNK.( !!(pt, Topology.pt_val out_pt_id) ))
    |> PNK.uniform

end

module Topo = Topology.Make(Parameters)
module Model = Model.Make(Parameters)

let () = begin
  let topo = Topo.parse (Sys.argv.(1)) in
  let topo_prog = Topo.to_probnetkat topo ~guard_links:true in
  Format.printf "%a\n\n" Syntax.pp_policy topo_prog;
  Util.timed "topo to Fdd" (fun () -> ignore (Symbolic.Fdd.of_pol topo_prog));
  let model = Util.timed "building model" (fun () -> Model.hop () ~final:false) in
  Format.printf "%a\n\n" Syntax.pp_policy model;
  Util.timed "model to Fdd" (fun () -> ignore (Symbolic.Fdd.of_pol model));
end
