open! Core
open Probnetkat
open Probnetkat.Symbolic


let () = begin
  let max_failures = [0; -1; ] in
  let failure_prob = Prob.(1//16) in
  let timeout = 3600 in (* in seconds *)
  let topos = [
      (* TODO: generate 2-ary fattree *)
      (* 2, ""; *)
      4, "./examples/output/abfattree_4_sw_20";
      6, "./examples/output/abfattree_6_sw_45";
      8, "./examples/output/abfattree_8_sw_80";
      10, "./examples/output/abfattree_10_sw_125";
      12, "./examples/output/abfattree_12_sw_180";
      14, "./examples/output/abfattree_12_sw_245";
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
        |> Fdd.allocate_fields
      in

      let logfile = "./examples/output/results/bench.log" in

      (* clear cache and set FDD order *)
      Fdd.clear_cache ~preserve:Int.Set.empty;
      Symbolic.Field.auto_order model;

      Util.log_and_sandbox ~timeout ~logfile topo_file ~f:(fun () ->
        Fdd.of_symbolic_pol model
        |> ignore
      );
    )
  )
end
