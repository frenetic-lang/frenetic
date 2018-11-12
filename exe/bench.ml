open! Core
open Probnetkat
open Probnetkat.Symbolic

let () = if Array.length Sys.argv <> 3 then begin
  printf "usage: %s [k: int] [cps: true/false]" Sys.argv.(0);
  exit 1;
end
let k = Int.of_string Sys.argv.(1)
let cps = Bool.of_string Sys.argv.(2)

let () = begin
  let max_failures = -1 in
  let failure_prob = Prob.(1//16) in
  let timeout = 3600 in (* in seconds *)
  let topos = Int.Table.of_alist_exn [
      2, "./examples/output/abfattree_2_sw_5";
      4, "./examples/output/abfattree_4_sw_20";
      6, "./examples/output/abfattree_6_sw_45";
      8, "./examples/output/abfattree_8_sw_80";
      10, "./examples/output/abfattree_10_sw_125";
      12, "./examples/output/abfattree_12_sw_180";
      14, "./examples/output/abfattree_14_sw_245";
      16, "./examples/output/abfattree_16_sw_320";
      18, "./examples/output/abfattree_18_sw_405";
    ]
  in

  let topo_file = Hashtbl.find_exn topos k in
  printf "\nk = %d | Pr[failure] = %s | max failures = %d || timeout = %d sec\n"
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

  Util.print_times := true;

  List.iter [None; (* Some 4; Some 6; Some 8; Some 10; Some 12; Some 14; Some 16 *)] ~f:(fun bound ->
    printf "bound = %s\n" (Option.value_map bound ~f:Int.to_string ~default:"none");
    (* clear cache and set FDD order *)
    Fdd.clear_cache ~preserve:Int.Set.empty;
    Fdd.clear_stats ();
    Fdd.use_cps := cps;

    (* Util.log_and_sandbox ~timeout ~logfile topo_file ~f:(fun () -> *)
    ignore (Fdd.of_pol ~bound ~auto_order:true model);
    Fdd.print_stats ();
    printf "Done.\n%!"
    (* ); *)
  )
end
