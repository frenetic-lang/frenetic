open! Core
open Probnetkat
open Syntax

(** parameters *)
let failure_prob = Prob.(1//1000)
let topo = Topology.parse (Sys.argv.(1))

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
    |> PNK.ite_cascade ~disjoint:true ~otherwise:PNK.drop ~f:(fun sw ->
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


let input_dist =
  let sw, pt = Model.src in
  let init = [ (Params.sw, sw); (Params.pt, pt) ] in
  [ (init, Prob.one) ]

let () = begin
  (* Prism.Code.of_pol Model.model ~input_dist *)
  Model.model
  |> Util.timed' "pol -> auto" (Prism.Automaton.of_pol ~input_dist)
  |> Util.timed' "auto -> cfg" Prism.CFG.of_automaton
  |> Util.timed' "cfg: basic blocks" (Util.tap ~f:Prism.CFG.merge_basic_blocks)
  |> Util.timed' "cfg -> auto" Prism.CFG.to_automaton
  |> (fun auto -> Prism.(Ast.model_of_auto auto (Domain.of_pol Model.model)))
  |> Prism.Code.of_model
  |> printf "%s"
  |> ignore
end
