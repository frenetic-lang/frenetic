open! Core
open Probnetkat
open Probnetkat.Symbolic

let ( |- ) x f = Util.tap ~f x


let () = begin
  let max_failures = [ -1 ] in
  let failure_prob = Prob.(1//16) in
  let timeout = 3600 in (* in seconds *)
  let topos = [
      2, "./examples/output/abfattree_2_sw_5";
      4, "./examples/output/abfattree_4_sw_20";
      6, "./examples/output/abfattree_6_sw_45";
    ]
  in

  List.iter max_failures ~f:(fun max_failures ->
    List.iter topos ~f:(fun (k,topo_file) ->
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
        (* |- Format.printf "%a\n\n%!" Syntax.pp_policy *)
        |> Fdd.allocate_fields
        (* |> Syntax.fix_observe *)
      in

      let logfile = "./examples/output/results/bench.log" in

      (* clear cache and set FDD order *)
      Fdd.clear_cache ~preserve:Int.Set.empty;
      Fdd.clear_stats ();
      Symbolic.Field.auto_order model;

      Util.log_and_sandbox ~timeout ~logfile topo_file ~f:(fun () ->
        Fdd.use_fast_obs := true;
        let fast = Fdd.of_symbolic_pol model in
        Fdd.print_stats ();
        Fdd.clear_stats ();

        Fdd.use_fast_obs := false;
        Fdd.clear_cache ~preserve:(Int.Set.singleton (fast :> int));
        let slow = Fdd.of_symbolic_pol model in

        Fdd.print_stats ();
        let equiv = Fdd.equivalent fast slow in
        printf "equivalent: %b\n%!" equiv;
        if not equiv then begin
          Fdd.render fast ~title:"fast";
          Fdd.render slow ~title:"slow";
          failwith "fast and slow observe give different results"
        end
      )
    )
  )
end
