open! Core
open Probnetkat
open Probnetkat.Syntax
open Probnetkat.Symbolic
open Frenetic.Network

let base_name = Sys.argv.(1)

(* link failure probabilities *)
let failure_prob _sw _pt = Prob.zero

(* Limit on maximum failures "encountered" by a packet. A packet encounters
   a failure if it occurs on a link that is incident to the current location
   of the packet, indepedently of whether the packet was planning to use that
   link or not. *)
let max_failures = None

(* topology *)
let topo = Topology.parse (base_name ^ ".dot")
let topo' = Schemes.enrich_topo topo

(* the actual program to run on the switches *)
let sw_pol = `Switchwise (Schemes.ecmp topo' base_name)


(*===========================================================================*)
(* Analyses                                                                  *)
(*===========================================================================*)


(* report whether fdd is equivalent to teleportation, modulo fields *)
let equivalent_to_teleport =
  let teleport = Fdd.of_pol (Model.teleportation topo) in
  let modulo = [Params.pt; Params.counter] in
  fun fdd ->
    let is_teleport = Fdd.equivalent fdd teleport ~modulo in
    printf "equivalent to teleportation: %s\n" (Bool.to_string is_teleport);
    is_teleport


let () = begin
  let open Params in

  (* TEST TOPOLOGY *)
  (* let topo_prog = Topology.to_probnetkat topo ~guard_links:true in *)
  (* Format.printf "%a\n\n" Syntax.pp_policy topo_prog; *)
  (* Util.timed "topo to Fdd" (fun () -> ignore (Fdd.of_pol topo_prog)); *)

  (* TEST PARSING *)
  (* ignore (parse_trees @@ base_name ^ "-disjointtrees.trees"); *)

  (* show topology *)
  Util.show_dot_file ~engine:"fdp" ~format:"svg" ~title:base_name (base_name ^ ".dot");

  (* Make model and compile it into an Fdd. *)
  let model = Util.timed "building model"
    (Model.make ~topo ~max_failures ~failure_prob ~sw_pol)
  in
  Format.printf "%a\n\n" Syntax.pp_policy model;
  let fdd = Util.timed "model to Fdd" (fun () -> Fdd.of_pol model) in
  printf ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> COMPILATION DONE\n%!";

  (* erase final port and counter values *)
  (* SJS: we might want to look at the expected number of failures, actually *)
  let fdd' = Fdd.modulo fdd [Params.pt; Params.counter] in
  printf "fdd mod final port & counter =\n%s\n" Fdd.(to_string fdd');

  (* do we gurantee packet delivery? *)
  ignore (equivalent_to_teleport fdd');

  (* show fdd *)
  Fdd.render fdd' ~title:base_name ~format:"svg";

  (* FIXME: should really use hoare style reasoning instead of adhoc mechansim
     for this
  *)

  (* compute output distribution *)
  let input_dist = Topology.uniform_ingress topo ~dst:destination in
  let output_dist = Fdd.output_dist fdd ~input_dist in
  printf "input distribution: %s\n\n" (Packet.Dist.to_string input_dist);
  printf "output distribution: %s\n\n" (Packet.Dist.to_string output_dist);

  (* probability of delivery *)
  let min_p =
    Fdd.of_pol PNK.( ??(sw, destination) )
    |> Fdd.seq fdd
    |> Fdd.min_nondrop_prob ~support:(Packet.Dist.support input_dist)
  in
  let avg_p = Packet.Dist.prob output_dist ~f:(fun pk ->
    Packet.test pk (Fdd.abstract_field sw) destination)
  in
  printf "min prob of delivery: %s\n" (Prob.to_string min_p);
  printf "avg prob of delivery: %s\n" (Prob.to_string avg_p);

  (* print ingress *)
  Topology.ingress_locs topo ~dst:destination
  |> List.map ~f:fst
  |> List.map ~f:(Topology.sw_val topo)
  |> List.sort ~cmp:Int.compare
  |> List.dedup_and_sort
  |> List.map ~f:(sprintf "%d")
  |> String.concat ~sep:", "
  |> printf "ingress switches: %s.\n";
end
