open! Core
open Probnetkat
open Syntax

(** parameters *)
let failure_prob = Prob.(1//1000)
let topo = Topology.parse (Sys.argv.(1))
let use_cps = bool_of_string Sys.argv.(2)
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

  let p =
    Topology.switches topo
    |> PNK.ite_cascade ~otherwise:PNK.drop ~f:(fun sw ->
      let sw = Topology.sw_val topo sw in
      PNK.(???(Params.sw, sw)), policy sw
    )

  let t = Topology.to_probnetkat topo ~guard_links:false

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
      p >> (ite egress skip t)
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
    let fdd = Fdd.of_pol ~auto_order:true Model.model in
    let p = Fdd.min_nondrop_prob' fdd in
    let q = Prob.to_q p in
    Format.printf "probability of delivery (precise): %a/%a\n%!"
      Z.pp_print (Q.num q) Z.pp_print (Q.den q);
    Format.printf "probability of delivery (approx): %f\n%!" (Prob.to_float p);
    (* Fdd.render fdd; *)
  (* ); *)
end


