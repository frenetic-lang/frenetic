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
  let timeout = 3600 in (* in seconds *)
  let topos = [
      (* 2, ""; *)
      4, "./examples/output/abfattree_4_sw_20";
      6, "./examples/output/abfattree_6_sw_45";
      8, "./examples/output/abfattree_8_sw_80";
    ]
  in

  List.iter max_failures ~f:(fun max_failures ->
    List.iter topos ~f:(fun (k,topo_file) ->
      printf "\nk = %d | Pr[failure] = %s | max failures = %d || (timeout = %d sec)\n"
        k (Prob.to_string failure_prob) max_failures timeout;
      printf "=======================================================================\n";
      
      (* prepare topo *)
      let abft = Topology.parse (Params.topo_file topo_file) in
      let abft' = Schemes.enrich_topo abft in
      let failure_locs = Topology.core_agg_locs abft in

      (* build syntactic model *)
      let model = Model.make 
        ~sw_pol:(`Portwise (Schemes.f10 abft' topo_file)) 
        ~topo:abft 
        ~max_failures:(if max_failures = -1 then None else Some max_failures)
        ~failure_prob:(fun s p ->
          if List.exists failure_locs (fun x -> x = (s,p)) then failure_prob
          else Prob.zero
        )
        ()
      in

      let logfile = "./examples/output/results/bench.log" in

      Fdd.clear_cache ~preserve:Int.Set.empty;
      (* first allocated fields; then auto order; then compile *)
      Symbolic.Field.auto_order model;

      (* run analysis & dump data *)
      Util.log_and_sandbox ~timeout ~logfile "fdd compilation" ~f:(fun () ->
        let fdd = Fdd.of_pol model in
        ()
      );
    )
  )
