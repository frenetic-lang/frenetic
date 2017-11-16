open! Core
open Probnetkat
open Probnetkat.Syntax
open Frenetic.Network
open Symbolic


module Parameters = struct

  let base_name = Sys.argv.(1)

  (* switch field *)
  let sw = "sw"

  (* port field *)
  let pt = "pt"

  (* counter field *)
  let counter = "failures"

  (* up bit associated with link *)
  let up sw pt = sprintf "up_%d" pt

  (* link failure probabilities *)
  let failure_prob _sw _pt = Prob.(1//10)

  (* Limit on maximum failures "encountered" by a packet. A packet encounters
     a failure if it occurs on a link that is incident to the current location
     of the packet, indepedently of whether the packet was planning to use that
     link or not. *)
  let max_failures = Some 1

  (* topology *)
  let topo = Topology.parse (base_name ^ ".dot")

  let destination = PNK.( ???(sw, 3) )

  (* different routing schemes *)
  module Schemes = struct

    let random_walk sw =
      Topology.vertex_to_ports topo sw ~dst_filter:(Topology.is_switch topo)
      |> List.map ~f:(fun out_pt_id -> PNK.( !!(pt, Topology.pt_val out_pt_id) ))
      |> PNK.uniform

    let resilient_random_walk sw =
      let pts = Topology.vertex_to_ports topo sw
        |> List.map ~f:Topology.pt_val
      in
      let good_pt = PNK.(
        List.map pts ~f:(fun pt_val -> ???(pt,pt_val) & ???(up sw pt_val, 1))
        |> mk_big_disj
      )
      in
      let choose_port = random_walk sw in
      PNK.( choose_port >> whl (neg good_pt) choose_port )

    (* let shortest_path sw = *)

  end

  (* the actual program to run on the switches *)
  let sw_pol = `Switchwise Schemes.resilient_random_walk


end

module Topo = Topology.Make(Parameters)
module Model = Model.Make(Parameters)

let () = begin
  let open Parameters in
  let topo_prog = Topo.to_probnetkat topo ~guard_links:true in
  Format.printf "%a\n\n" Syntax.pp_policy topo_prog;
  Util.timed "topo to Fdd" (fun () -> ignore (Fdd.of_pol topo_prog));
  let model = Util.timed "building model" (fun () -> Model.make ()) in
  Format.printf "%a\n\n" Syntax.pp_policy model;
  let fdd = Util.timed "model to Fdd" (fun () -> Fdd.of_pol model) in
  printf ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> DONE\n%!";
  let fdd = Fdd.modulo fdd [Parameters.pt; Parameters.counter] in
  printf "fdd mod final port = %s\n" Fdd.(to_string (simplify fdd));
  let teleport = Fdd.of_pol (Model.teleportation ()) in
  printf "teleport = %s\n" (Fdd.to_string teleport);
  let is_teleport = Fdd.equivalent fdd teleport in
  printf "equivalent to teleportation: %s\n" (Bool.to_string is_teleport);
end
