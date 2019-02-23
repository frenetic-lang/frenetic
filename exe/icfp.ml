open! Core
open Probnetkat
open Probnetkat.Syntax
open Probnetkat.Symbolic
open Frenetic.Network


(*===========================================================================*)
(* Data from experiments                                                     *)
(*===========================================================================*)

(** raw data from experiment *)
type data = {
  max_failures : int; (* -1 means no limit *)
  failure_prob : int * int;
  mutable invariant : bool * bool;
  mutable fattree_comp : bool;
  mutable refinement : bool list;
  mutable resilience : bool list;

} [@@deriving yojson]

let empty ~max_failures ~failure_prob =
  {
    max_failures;
    failure_prob = Prob.to_int_frac failure_prob;
    invariant = (false, false);
    fattree_comp = false;
    refinement = [];
    resilience = [];
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
let load ~max_failures ~failure_prob
: string * data * [`Fresh | `Existed] =
  let dir = sprintf "./examples/output/results/comparisons" in
  let mf = if max_failures < 0 then "inf" else Int.to_string max_failures in
  let p_num, p_den = Prob.to_int_frac failure_prob in
  let file = sprintf "%s/%s-%d-%d.json" dir mf p_num p_den in
  if is_ok (Unix.access file [`Exists]) then
    let data = load_file file in
    file, data, `Existed
  else
    let data = empty ~max_failures ~failure_prob in
    file, data, `Fresh


(*===========================================================================*)
(* Analyses                                                                  *)
(*===========================================================================*)


let equiv p1 p2 =
  Fdd.(equivalent (of_pol p1) (of_pol p2) ~modulo:Params.modulo)

let leq p1 p2 =
  let p1 = Fdd.of_pol p1 in
  let p2 = Fdd.of_pol p2 in
  Fdd.(less_than p1 p2 ~modulo:Params.modulo)

(*===========================================================================*)
(* CLI                                                                       *)
(*===========================================================================*)
let () =
  let max_failures = [0;1;2;3;4;-1] in
  let failure_prob = Prob.(1//16) in
  let timeout = 3600 in
  let fattree = "./examples/output/fattree_4_sw_20" in
  let abfattree = "./examples/output/abfattree_4_sw_20" in
  let ft = Topology.parse (Params.topo_file fattree) in
  let abft = Topology.parse (Params.topo_file abfattree) in
  let failure_locs_ft = Topology.core_agg_locs ft in
  let failure_locs_abft = Topology.core_agg_locs abft in
  let failure_prob_ft = fun s p ->
    if List.exists failure_locs_ft (fun x -> x = (s,p)) then failure_prob
    else Prob.zero in
  let failure_prob_abft = fun s p ->
    if List.exists failure_locs_abft (fun x -> x = (s,p)) then failure_prob
    else Prob.zero in



  List.iter max_failures ~f:(fun max_failures ->
      printf "\nPr[failure] = %s, max failures = %d, timeout = %d sec\n"
        (Prob.to_string failure_prob) max_failures timeout;
      printf "=======================================================================\n";
      (* load existing data *)
      (* prepare analysis *)

      let logfile = "./examples/output/results/icfp.log" in

      (* run analysis & dump data *)
      Util.log_and_sandbox ~timeout ~logfile "1. invariant" ~f:(fun () ->
          let file, data, history =
            load ~max_failures ~failure_prob
          in
          let max_failures = if max_failures = -1 then None else Some max_failures in

          let ft' = Schemes.enrich_topo ft in
          let abft' = Schemes.enrich_topo abft in
          let f10_ft = Model.make ~sw_pol:(`Portwise (Schemes.f10 ~s1:true ~s2:true ft' fattree)) ~topo:ft ~max_failures ~failure_prob:failure_prob_ft () in
          let f10_abft = Model.make ~sw_pol:(`Portwise (Schemes.f10 ~s1:true ~s2:true abft' abfattree)) ~topo:abft ~max_failures ~failure_prob:failure_prob_abft () in

          data.invariant <- (equiv f10_ft PNK.(f10_ft >> ??(Params.f10s2, 0)),
                             equiv f10_abft PNK.(f10_abft >> ??(Params.f10s2, 0)) );
          dump data file
        );
        (* run analysis & dump data *)
      printf "\n";
      Util.log_and_sandbox ~timeout ~logfile "2. FT local rerouting" ~f:(fun () ->
          let file, data, history =
            load ~max_failures ~failure_prob
          in
          let max_failures = if max_failures = -1 then None else Some max_failures in

          let topo = ft in
          let ft = Schemes.enrich_topo ft in
          let failure_prob = failure_prob_ft in
          let ecmp_ft = Model.make ~sw_pol:(`Portwise (Schemes.f10 ~s1:false ~s2:false ft fattree)) ~topo ~max_failures ~failure_prob () in
          let f10_s1_ft = Model.make ~sw_pol:(`Portwise (Schemes.f10 ~s1:true ~s2:false ft fattree)) ~topo ~max_failures ~failure_prob () in
          data.fattree_comp <- equiv ecmp_ft f10_s1_ft;
          dump data file
        );
      printf "\n";
      Util.log_and_sandbox ~timeout ~logfile "3. refinement" ~f:(fun () ->
          let file, data, history =
            load ~max_failures ~failure_prob
          in
          let max_failures = if max_failures = -1 then None else Some max_failures in

          let abft' = Schemes.enrich_topo abft in
          let f10_no_abft = Model.make ~sw_pol:(`Portwise (Schemes.f10 ~s1:false ~s2:false abft' abfattree)) ~topo:abft ~max_failures ~failure_prob:failure_prob_abft () in
          let f10_s1_abft = Model.make ~sw_pol:(`Portwise (Schemes.f10 ~s1:true ~s2:false abft' abfattree)) ~topo:abft ~max_failures ~failure_prob:failure_prob_abft () in
          let f10_abft = Model.make ~sw_pol:(`Portwise (Schemes.f10 ~s1:true ~s2:true abft' abfattree)) ~topo:abft ~max_failures ~failure_prob:failure_prob_abft () in

          data.refinement <- [leq f10_no_abft f10_s1_abft;
                              leq f10_s1_abft f10_abft;
                              leq f10_abft (Model.teleportation abft)];
          dump data file
        );
        (* run analysis & dump data *)
      printf "\n";
      Util.log_and_sandbox ~timeout ~logfile "4. k-resilience" ~f:(fun () ->
          let file, data, history =
            load ~max_failures ~failure_prob
          in
          let max_failures = if max_failures = -1 then None else Some max_failures in

          let abft' = Schemes.enrich_topo abft in
          let f10_no_abft = Model.make ~sw_pol:(`Portwise (Schemes.f10 ~s1:false ~s2:false abft' abfattree)) ~topo:abft ~max_failures ~failure_prob:failure_prob_abft () in
          let f10_s1_abft = Model.make ~sw_pol:(`Portwise (Schemes.f10 ~s1:true ~s2:false abft' abfattree)) ~topo:abft ~max_failures ~failure_prob:failure_prob_abft () in
          let f10_abft = Model.make ~sw_pol:(`Portwise (Schemes.f10 ~s1:true ~s2:true abft' abfattree)) ~topo:abft ~max_failures ~failure_prob:failure_prob_abft () in

          data.resilience <- [equiv f10_no_abft (Model.teleportation abft);
                              equiv f10_s1_abft (Model.teleportation abft);
                              equiv f10_abft (Model.teleportation abft)];
          dump data file
        );
        (* run analysis & dump data *)
      printf "\n";

    )
