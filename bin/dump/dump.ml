open! Core
open Probnetkat
open Probnetkat.Syntax
open Probnetkat.Symbolic
open Frenetic.Network

(* throw exception upon interrupt *)
let () = Caml.Sys.catch_break true


(*===========================================================================*)
(* Data from experiments                                                     *)
(*===========================================================================*)

(** raw data from experiment *)
type data = {
  topology : string;
  routing_scheme : string;
  max_failures : int; (* -1 means no limit *)
  failure_prob : int * int;
  mutable equivalent_to_teleport : bool;
  mutable min_prob_of_delivery : float;
  mutable avg_prob_of_delivery : float;
  mutable hop_count_cdf : float list;

  (* various times *)
  mutable compilation_time : float;
  mutable equivalence_time : float;
  mutable hop_count_time   : float;
} [@@deriving yojson]

let prob_to_frac p =
  let q = Prob.to_q p in
   Z.(to_int q.num, to_int q.den)

let empty ~routing_scheme ~topology ~max_failures ~failure_prob =
  { topology;
    routing_scheme ;
    max_failures;
    failure_prob = prob_to_frac failure_prob;
    equivalent_to_teleport = false;
    min_prob_of_delivery = -2.0;
    avg_prob_of_delivery = -2.0;
    hop_count_cdf = [];
    compilation_time = -2.0;
    equivalence_time = -2.0;
    hop_count_time = -2.0;
  }

let dump (data : data) ~file : unit =
  let dir = Filename.dirname file in
  Unix.mkdir_p dir;
  let json = data_to_yojson data in
  Out_channel.with_file file ~f:(fun chan ->
    Yojson.Safe.pretty_to_channel chan json
  )

let load_file file : data =
  Yojson.Safe.from_file file
  |> data_of_yojson
  |> Result.ok_or_failwith

(* returns file, data read from file, and if the file is fresh or existed already *)
let load base_name ~routing_scheme ~max_failures ~failure_prob
: string * data * [`Fresh | `Existed] =
  let scheme = String.tr routing_scheme ~target:' ' ~replacement:'_' in
  let dir, topology = Filename.split base_name in
  let dir = sprintf "%s/results/%s-%s" dir topology scheme in
  let mf = if max_failures < 0 then "inf" else Int.to_string max_failures in
  let p_num, p_den = prob_to_frac failure_prob in
  let file = sprintf "%s/%s-%d-%d.json" dir mf p_num p_den in
  if is_ok (Unix.access file [`Exists]) then
    let data = load_file file in
    file, data, `Existed
  else
    let data = empty ~routing_scheme ~topology ~max_failures ~failure_prob in
    file, data, `Fresh


(*===========================================================================*)
(* Analyses                                                                  *)
(*===========================================================================*)


let equivalent_to_teleport fdd ~topo =
  let teleport = Fdd.of_pol (Model.teleportation topo) in
  Fdd.equivalent fdd teleport ~modulo:[Params.pt; Params.counter]

let hop_count_analysis ~sw_pol ~topo ~failure_prob ~max_failures ~data =
  let open Params in
  let bound = 2 * List.length (Topology.switches topo) in
  let bound = min Params.max_ttl bound in
  let model = Model.make ~bound ~sw_pol ~topo ~failure_prob ~max_failures () in
  let fdd = Fdd.of_pol model in
  let input_dist = Topology.uniform_ingress topo ~dst:destination in
  let output_dist = Fdd.output_dist fdd ~input_dist in
  let cdf =
    List.init bound ~f:(fun ttl_val ->
      Packet.Dist.prob output_dist ~f:(fun pk ->
        Packet.test pk (Fdd.abstract_field sw) destination &&
        Packet.test_with ~f:(>=) pk (Fdd.abstract_field ttl) ttl_val
      )
    )
    |> List.rev
  in
  data.hop_count_cdf <- List.map cdf ~f:Prob.to_float
  (* hop_count[i] = Pr[π.ttl >= bound - (i+1) ]
                  = Pr[#hops(π) <= i+1]
   *)

let basic_analysis model ~topo ~data =

  let open Params in

  (* COMPILATION *)
  Format.printf "\nProbNetKAT model:\n# while loops = %d\n%a\n%!"
    (Syntax.nr_of_loops model)
    Syntax.pp_policy model;
  Fdd.set_order [Params.sw; Params.pt; Params.counter; Params.ttl ];
  let compilation_time, fdd = Util.time Fdd.of_pol model in
  data.compilation_time <- compilation_time;
  printf "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> COMPILATION DONE\n%!";

  (* EQUIVALENCE *)
  let equivalence_time, equivalent_to_teleport =
    Util.time (equivalent_to_teleport ~topo) fdd in
  data.equivalence_time <- equivalence_time;
  data.equivalent_to_teleport <- equivalent_to_teleport;

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
  data.min_prob_of_delivery <- Prob.to_float min_p;
  data.avg_prob_of_delivery <- Prob.to_float avg_p

let analyze_scheme base_name (routing_scheme, sw_pol)
  ~failure_prob ~max_failures ~topo ~log ~timeout ~data ~hopcount =

  (* clear memory *)
  Fdd.clear_cache ~preserve:Int.Set.empty;
  let _ = Util.timed' " - gc\t" ~log ~f:Gc.compact in

  (* hop count analysis *)
  if hopcount then Util.attempt (fun () ->
    let hop_count_time, () =
      Util.timed' " - hop count" ~log ~timeout ~f:(fun () ->
        hop_count_analysis ~sw_pol ~topo ~data
          ~failure_prob:(fun _ _ -> failure_prob)
          ~max_failures:(if max_failures = -1 then None else Some max_failures)

      )
    in
    data.hop_count_time <- hop_count_time;
  );

  (* other analyses *)
  if not hopcount then Util.attempt (fun () ->
    let model = Model.make ~sw_pol ~topo
        ~failure_prob:(fun _ _ -> failure_prob)
        ~max_failures:(if max_failures = -1 then None else Some max_failures)
        ()
    in
    ignore (Util.timed' " - analysis" ~log  ~timeout ~f:(fun () -> basic_analysis model ~topo ~data))
  )

let analyze_all base_name ~failure_prob ~max_failures ~timeout ~hopcount : unit =
  let topo = Topology.parse (Params.topo_file base_name) in
  Schemes.get_all topo base_name
  |> List.iter ~f:(fun ((routing_scheme, _) as scheme) ->
    try
      let file, data, history = load base_name ~routing_scheme ~max_failures ~failure_prob in
      begin match history with
      | `Fresh -> printf "\n%s (*):\n" routing_scheme
      | `Existed -> printf "\n%s:\n" routing_scheme
      end;
      let dir = Filename.dirname base_name in
      Out_channel.with_file (dir ^ "/results.log") ~f:(fun log ->
        analyze_scheme ~failure_prob ~max_failures ~topo ~log ~timeout ~data ~hopcount
          base_name scheme
      );
      dump data ~file;
    with Caml.Sys.Break ->
      ();
  )




(*===========================================================================*)
(* CLI                                                                       *)
(*===========================================================================*)

let parse_list l ~f =
  String.split l ~on:','
  |> List.map ~f:(fun x -> f (String.strip x))

let parse_flag flags =
  String.concat flags ~sep:" "
  |> String.split ~on:'-'
  |> List.map ~f:(String.split_on_chars ~on:[' '; '\t'])
  |> List.filter_map ~f:(function
    | [] -> None
    | flag::payload -> Some (flag, String.concat payload)
  )
  |> String.Table.of_alist_exn
  |> String.Table.find

let () =
  match Array.to_list Sys.argv with
  | _::base_name::max_failures::failure_probs::flags ->
    let parse_flag = parse_flag flags in
    let max_failures = parse_list max_failures ~f:Int.of_string in
    let failure_probs = parse_list failure_probs ~f:Prob.of_string in
    let timeout = Option.(
      parse_flag "timeout"
      >>| Int.of_string
      |> value ~default:(60 * 60 * 1) (* by default, timeout after 1h *)
    )
    in
    let hopcount = Option.is_some (parse_flag "hopcount") in
    List.iter max_failures ~f:(fun max_failures ->
      List.iter failure_probs ~f:(fun failure_prob ->
        printf "\n%s, Pr[failure] = %s, max failures = %d, timeout = %d sec\n"
          (Filename.basename base_name)
          (Prob.to_string failure_prob) max_failures timeout;
        printf "=======================================================================\n";
        analyze_all base_name ~max_failures ~failure_prob ~timeout ~hopcount;
        printf "\n";
      )
    )
  | _ ->
    printf "usage: %s <topology> <max failures> <Pr(failure)> [-hopcount] [-timeout s]\n"
      Sys.argv.(0)

