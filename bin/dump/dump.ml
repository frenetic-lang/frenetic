open! Core
open Probnetkat
open Probnetkat.Syntax
open Probnetkat.Symbolic
open Frenetic.Network

(** raw data from experiment *)
type data = {
  topology : string;
  routing_scheme : string;
  max_failures : int; (* -1 means no limit *)
  failure_prob : int * int;
  equivalent_to_teleport : bool;
  min_prob_of_delivery : float;
  avg_prob_of_delivery : float;
  hop_count_cdf : float list;

  (* various times *)
  compilation_time : float;
  equivalence_time : float;
  hop_count_time   : float;
} [@@deriving yojson]

let empty = {
  topology = "undefined";
  routing_scheme = "undefined";
  max_failures = -2;
  failure_prob = (0, 0);
  equivalent_to_teleport = false;
  min_prob_of_delivery = -2.0;
  avg_prob_of_delivery = -2.0;
  hop_count_cdf = [];
  compilation_time = -2.0;
  equivalence_time = -2.0;
  hop_count_time = -2.0;
}

let dump_data data ~dir : unit =
  let scheme = String.tr data.routing_scheme ~target:' ' ~replacement:'_' in
  let dir = sprintf "%s/results/%s-%s" dir data.topology scheme in
  Unix.mkdir_p dir;
  let max_failures =
    if data.max_failures < 0 then "inf" else Int.to_string data.max_failures in
  let file = sprintf "%s/%s-%d-%d.json" dir
    max_failures (fst data.failure_prob) (snd data.failure_prob)
  in
  Out_channel.with_file file ~f:(fun chan ->
    data_to_yojson data
    |> Yojson.Safe.to_string
    |> Out_channel.output_string chan
  )


let rec analyze base_name ~failure_prob ~max_failures ~log : data list =
  let topo = Topology.parse (Params.topo_file base_name) in
  (* SJS: constant failure probabilities *)
  Schemes.get_all topo base_name
  |> List.map ~f:(analyze_scheme base_name ~failure_prob ~max_failures ~topo ~log)

and analyze_scheme base_name (routing_scheme, sw_pol) ~failure_prob ~max_failures ~topo ~log =
  printf "\n%s:\n" routing_scheme;
  Fdd.clear_cache ~preserve:Int.Set.empty;
  let _ = Util.timed' " - gc" ~log ~f:Gc.compact in
  let hop_count_time, hop_count_cdf =
    Util.timed' " - hop count" ~log  ~f:(fun () ->
      analyze_hop_count ~sw_pol ~topo
        ~failure_prob:(fun _ _ -> failure_prob)
        ~max_failures:(if max_failures = -1 then None else Some max_failures)
    )
  in
  Fdd.clear_cache ~preserve:Int.Set.empty;
  let _ = Util.timed' " - gc" ~log ~f:Gc.compact in
  let model = Model.make ~sw_pol ~topo
      ~failure_prob:(fun _ _ -> failure_prob)
      ~max_failures:(if max_failures = -1 then None else Some max_failures)
      ()
  in
  let _,data = Util.timed' " - other" ~log ~f:(fun () -> analyze_model model ~topo) in
  { data with
    routing_scheme;
    topology = Filename.basename base_name;
    max_failures;
    failure_prob = Z.(to_int Prob.(num failure_prob), to_int Prob.(den failure_prob));
    hop_count_cdf = List.map hop_count_cdf ~f:Prob.to_float;
    hop_count_time;
  }

and analyze_model model ~topo =

  let open Params in

  (* COMPILATION *)
  let compilation_time, fdd = Util.time Fdd.of_pol model in
  printf "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> COMPILATION DONE\n%!";

  (* EQUIVALENCE *)
  let equivalence_time, equivalent_to_teleport =
    Util.time (equivalent_to_teleport ~topo) fdd in

  (* OUTPUT DISTRIBUTION *)
  let input_dist = Topology.uniform_ingress topo ~dst:destination in
  let output_dist = Fdd.output_dist fdd ~input_dist in
  (* printf "input distribution: %s\n\n" (Packet.Dist.to_string input_dist); *)
  (* printf "output distribution: %s\n\n" (Packet.Dist.to_string output_dist); *)

  (* probability of delivery *)
  let min_p =
    Fdd.of_pol PNK.( ??(sw, destination) )
    |> Fdd.seq fdd
    |> Fdd.min_nondrop_prob ~support:(Packet.Dist.support input_dist)
  in
  let avg_p = Packet.Dist.prob output_dist ~f:(fun pk ->
    Packet.test pk (Fdd.abstract_field sw) destination)
  in
  (* printf "min prob of delivery: %s\n" (Prob.to_string min_p); *)
  (* printf "avg prob of delivery: %s\n" (Prob.to_string avg_p); *)

  { empty with
    compilation_time;
    equivalent_to_teleport;
    equivalence_time;
    min_prob_of_delivery = Prob.to_float min_p;
    avg_prob_of_delivery = Prob.to_float avg_p;
  }

and equivalent_to_teleport fdd ~topo =
  let teleport = Fdd.of_pol (Model.teleportation topo) in
  let modulo = [Params.pt; Params.counter] in
  let is_teleport = Fdd.equivalent fdd teleport ~modulo in
  (* printf "equivalent to teleportation: %s\n" (Bool.to_string is_teleport); *)
  is_teleport

and analyze_hop_count ~sw_pol ~topo ~failure_prob ~max_failures =
  let open Params in
  let bound = 2 * List.length (Topology.switches topo) in
  let model = Model.make ~bound ~sw_pol ~topo ~failure_prob ~max_failures () in
  let fdd = Fdd.of_pol model in
  let input_dist = Topology.uniform_ingress topo ~dst:destination in
  let output_dist = Fdd.output_dist fdd ~input_dist in
  List.init bound ~f:(fun ttl_val ->
    Packet.Dist.prob output_dist ~f:(fun pk ->
      Packet.test pk (Fdd.abstract_field sw) destination &&
      Packet.test_with ~f:(>=) pk (Fdd.abstract_field ttl) ttl_val
    )
  )
  |> List.rev


let parse_list l ~f =
  String.split l ~on:','
  |> List.map ~f:(fun x -> f (String.strip x))

let () =
  match Sys.argv with
  | [| _; base_name; max_failures; failure_probs; |] ->
    let dir = Filename.dirname base_name in
    let max_failures = parse_list max_failures ~f:Int.of_string in
    let failure_probs = parse_list failure_probs ~f:Q.of_string in
    Out_channel.with_file (dir ^ "/results.log") ~f:(fun log ->
      List.iter max_failures ~f:(fun max_failures ->
        List.iter failure_probs ~f:(fun failure_prob ->
          printf "\n%s, Pr[failure] = %s, max failures = %d\n" (Filename.basename base_name)
            (Prob.to_string failure_prob) max_failures;
          printf "================================================================\n";
          analyze base_name ~max_failures ~failure_prob ~log
          |> List.iter ~f:(dump_data ~dir);
          printf "\n";
        )
      )
    )
  | _ ->
    printf "usage: %s [topology] [max failures] [Pr(failure)]\n"
      Sys.argv.(0)


