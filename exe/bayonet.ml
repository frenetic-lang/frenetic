(** Run Bayonet experiment using ProbNetKAT with native backend.  *)

open! Core
open Probnetkat
open Syntax


let () = if Array.length Sys.argv <> 4 then begin
  printf "usage: %s [dot file] [cps: true/false] [parallelize: true/false]\n"
    Sys.argv.(0);
  exit 1;
end

(** parameters *)
let failure_prob = Prob.(1//1000)
let topo = Topology.parse (Sys.argv.(1))
let use_cps = bool_of_string Sys.argv.(2)
let parallelize = bool_of_string Sys.argv.(3)
let timeout = 3600 (* in seconds *)



module Model = struct
  let policy sw =
    match sw mod 4 with
    | 0 ->
      PNK.(?@[
        !!(Params.pt, 2) @ 1//2;
        !!(Params.pt, 3) @ 1//2;
      ])
    | 1 ->
      PNK.(!!(Params.pt, 2))
    | 2 ->
      PNK.(?@[
        drop              @ failure_prob;
        !!(Params.pt, 2)  @ Prob.(one - failure_prob);
      ])
    | 3 ->
      PNK.(!!(Params.pt, 3))
    | _ ->
      assert false

  (* SJS: equivalent, but more factored *)
  let p' =
    let sws = Topology.switches topo |> List.map ~f:(Topology.sw_val topo) in
    let sws0, sws = List.partition_tf sws ~f:(fun sw -> sw mod 4 = 0) in
    let sws1, sws = List.partition_tf sws ~f:(fun sw -> sw mod 4 = 1) in
    let sws2, sws = List.partition_tf sws ~f:(fun sw -> sw mod 4 = 2) in
    let sws3, sws = List.partition_tf sws ~f:(fun sw -> sw mod 4 = 3) in
    assert (sws = []);
    let sws = [| sws0; sws1; sws2; sws3 |] in
    List.init 4 ~f:(fun i ->
      let guard =
        List.map sws.(i) ~f:(fun sw -> PNK.(???(Params.sw, sw)))
        |> PNK.mk_big_disj
      in
      (guard, policy i)
    )
    |> PNK.branch

  let t sw =
    Topology.links_from topo sw ~guard_links:false ~dst_filter:(Topology.is_switch topo)

  let src,dst =
    Topology.hosts topo
    |> List.sort ~compare:(fun h1 h2 ->
      Int.compare Topology.(host_val topo h1) Topology.(host_val topo h2)
    )
    |> List.map ~f:(fun h ->
      Frenetic.Network.Net.Topology.(
        neighbors topo h
        |> Set.to_list
        |> List.map ~f:(find_edge topo h)
        |> (function [e] -> e | _ -> assert false)
        |> edge_dst
        |> (fun (sw,pt) -> Topology.(sw_val topo sw, pt_val pt))
      )
    )
    |> function
      | [src;dst] -> src,dst
      | hs -> failwith (sprintf "expected 2 hosts; got %d" (List.length hs))

  let ingress =
    let sw,pt = src in
    PNK.((!!(Params.sw, sw)) >> (!!(Params.pt, pt)))

  let egress =
    let sw,pt = dst in
    PNK.(conj (???(Params.sw, sw)) (???(Params.pt, pt)))

  let model = PNK.(
    ingress >>
    whl (neg egress) (
      Topology.switches topo
      |> PNK.ite_cascade ~parallelize:true ~disjoint:true ~otherwise:PNK.drop ~f:(fun sw ->
        let sw_val = Topology.sw_val topo sw in
        PNK.(???(Params.sw, sw_val), policy sw_val >> (ite egress skip (t sw)))
      )
    )
  )
end

open Symbolic
let () = begin
  (* Format.printf "%a\n%!" Syntax.pp_policy Model.model; *)
  (* let logfile = "./examples/logs/bayonet.log" in *)
  Util.print_times := false;
  (* Util.log_and_sandbox ~timeout ~logfile Sys.argv.(1) ~f:(fun () -> *)
  Fdd.use_cps := use_cps;
  let fdd = Fdd.of_pol ~parallelize ~auto_order:true Model.model in
  Fdd.print_stats ();
  let p = Fdd.min_nondrop_prob' fdd in
  let q = Prob.to_q p in
  Format.printf "probability of delivery (precise): %a/%a\n%!"
    Z.pp_print (Q.num q) Z.pp_print (Q.den q);
  Format.printf "probability of delivery (approx): %f\n%!" (Prob.to_float p);
    (* Fdd.render fdd; *)
  (* ); *)
end


