open! Core
open Probnetkat
open Probnetkat.Syntax
open Frenetic.Network
open Symbolic

module Int2 = struct
  module T = struct
    type t = int*int [@@deriving sexp, hash, compare]
  end
  include T
  module Tbl = Hashtbl.Make(T)
  module Map = Map.Make(T)
end

module Parameters = struct

  let base_name = Sys.argv.(1)

  (* switch field *)
  let sw = "sw"

  (* port field *)
  let pt = "pt"

  (* counter field *)
  let counter = "failures"

  (* up bit associated with link *)
  let up sw pt = sprintf "up_%d" pt

  (* link failure probabilities *)
  let failure_prob _sw _pt = Prob.(1//10)

  (* Limit on maximum failures "encountered" by a packet. A packet encounters
     a failure if it occurs on a link that is incident to the current location
     of the packet, indepedently of whether the packet was planning to use that
     link or not. *)
  let max_failures = Some 2

  (* topology *)
  let topo = Topology.parse (base_name ^ ".dot")

  (* destination host *)
  let destination = 1


(*===========================================================================*)
(* AUXILLIARY                                                          *)
(*===========================================================================*)

  let switch_map : Net.Topology.vertex Int.Map.t =
    let open Net.Topology in
    fold_vertexes (fun v map ->
      let id = Topology.sw_val topo v in
      Int.Map.add map ~key:id ~data:v
    )
      topo
      Int.Map.empty

  (* (src_sw,dst_sw) |-> edge *)
  let edge_map : Net.Topology.edge Int2.Map.t =
    Net.Topology.fold_edges (fun edge map ->
      let (src,_) = Net.Topology.edge_src edge in
      let (dst,_) = Net.Topology.edge_dst edge in
      let key = Topology.(sw_val topo src, sw_val topo dst) in
      Map.add map ~key ~data:edge
    )
      topo
      Int2.Map.empty

  (* (sw,pt) |-> out edge *)
  let hop_map : Net.Topology.edge Int2.Map.t =
    Net.Topology.fold_edges (fun edge map ->
      let (src_sw, src_pt) = Net.Topology.edge_src edge in
      let key = Topology.(sw_val topo src_sw, pt_val src_pt) in
      Map.add map ~key ~data:edge
    )
      topo
      Int2.Map.empty

  let parse_sw sw =
    assert (String.get sw 0 = 's');
    String.slice sw 1 (String.length sw)
    |> Int.of_string

  (* switch to port mapping *)
  let parse_trees file : (int list) Int.Table.t =
    let tbl = Int.Table.create () in
    In_channel.(with_file file ~f:(iter_lines ~f:(fun l ->
      let l = String.strip l in
      if not (String.get l 0 = '#') then
      match String.split ~on:' ' l with
      | [src; "--"; dst] ->
        let src = parse_sw src in
        let dst = parse_sw dst in
        let edge = Map.find_exn edge_map (src,dst) in
        let (_, out_port) = Net.Topology.edge_src edge in
        (* find destination port *)
        Int.Table.add_multi tbl ~key:src ~data:(Topology.pt_val out_port)
      | _ ->
        failwith "unexpected format"
    )));
    tbl

  (* switch to port mapping *)
  let parse_nexthops file : (int list) Int.Table.t =
    let tbl = Int.Table.create () in
    In_channel.(with_file file ~f:(iter_lines ~f:(fun l ->
      let l = String.strip l in
      if not (String.get l 0 = '#') then
      match String.split ~on:' ' l with
      | src::":"::dsts ->
        let src = parse_sw src in
        List.map dsts ~f:(fun dst ->
          let dst = parse_sw dst in
          let edge = Map.find_exn edge_map (src,dst) in
          let (_, out_port) = Net.Topology.edge_src edge in
          Topology.pt_val out_port
        )
        |> fun data -> Int.Table.add_exn tbl ~key:src ~data
      | _ ->
        failwith "unexpected format"
    )));
    tbl

  (* am I at a good port? *)
  let at_good_pt sw pts = PNK.(
    List.map pts ~f:(fun pt_val -> ???(pt,pt_val) & ???(up sw pt_val, 1))
    |> mk_big_disj
  )

  (* given a current switch and the inport, what tree are we on? *)
  let mk_current_tree_map (port_map : (int list) Int.Table.t) : int Int2.Map.t =
    (* the port map maps a switch to the out_ports in order of the tree preference *)
    Hashtbl.fold port_map ~init:Int2.Map.empty ~f:(fun ~key:src_sw ~data:src_pts init ->
      List.foldi src_pts ~init ~f:(fun i init src_pt ->
        let edge = Map.find_exn hop_map (src_sw, src_pt) in
        let (dst_sw, dst_pt) = Net.Topology.edge_dst edge in
        let key = Topology.(sw_val topo dst_sw, Topology.pt_val dst_pt) in
        Map.add init ~key ~data:i
      )
    )


(*===========================================================================*)
(* ROUTING SCHEMES                                                           *)
(*===========================================================================*)

  (* different routing schemes *)
  module Schemes = struct

    let random_walk sw =
      Topology.vertex_to_ports topo sw ~dst_filter:(Topology.is_switch topo)
      |> List.map ~f:(fun out_pt_id -> PNK.( !!(pt, Topology.pt_val out_pt_id) ))
      |> PNK.uniform

    let resilient_random_walk sw =
      let pts = Topology.vertex_to_ports topo sw
        |> List.map ~f:Topology.pt_val
      in
      let choose_port = random_walk sw in
      PNK.( do_whl (neg (at_good_pt sw pts)) choose_port )

    let shortest_path : Net.Topology.vertex -> string policy =
      let port_map = parse_trees (base_name ^ "-spf.trees") in
      fun sw ->
        let sw_val = Topology.sw_val topo sw in
        match Hashtbl.find port_map sw_val with
        | Some (pt_val::_) -> PNK.( !!(pt, pt_val) )
        | _ ->
          eprintf "switch %d cannot reach destination\n" sw_val;
          failwith "network disconnected!"

    let ecmp : Net.Topology.vertex -> string policy =
      let port_map = parse_nexthops (base_name ^ "-allsp.nexthops") in
      fun sw ->
        let sw_val = Topology.sw_val topo sw in
        match Hashtbl.find port_map sw_val with
        | Some pts -> PNK.(
            List.map pts ~f:(fun pt_val -> !!(pt, pt_val))
            |> uniform
          )
        | _ ->
          eprintf "switch %d cannot reach destination\n" sw_val;
          failwith "network disconnected!"

    let reslient_ecmp : Net.Topology.vertex -> string policy =
      let port_map = parse_nexthops (base_name ^ "-allsp.nexthops") in
      fun sw ->
        let sw_val = Topology.sw_val topo sw in
        match Hashtbl.find port_map sw_val with
        | Some pts -> PNK.(
            do_whl (neg (at_good_pt sw pts)) (
              List.map pts ~f:(fun pt_val -> !!(pt, pt_val))
              |> uniform
            )
          )
        | _ ->
          eprintf "switch %d cannot reach destination\n" sw_val;
          failwith "network disconnected!"

    let deterministic_car : Net.Topology.vertex -> int -> string policy =
      let port_tbl = parse_trees (base_name ^ "-disjointtrees.trees") in
      let current_tree_map = mk_current_tree_map port_tbl in
      let port_tbl = Int.Table.map port_tbl ~f:Array.of_list in
      fun sw in_pt ->
        let sw_val = Topology.sw_val topo sw in
        match Map.find current_tree_map (sw_val, in_pt) with
        | None ->
          eprintf "verify that packets never enter switch %d at port %d\n" sw_val in_pt;
          PNK.( drop )
        | Some i ->
          let pts = Hashtbl.find_exn port_tbl sw_val in
          let n = Array.length pts in
          (* the order in which we should try ports, i.e. starting from i *)
          let pts = Array.init n (fun j -> pts.((i+j) mod n)) in
          PNK.(
            Array.to_list pts
            |> ite_cascade ~otherwise:drop ~f:(fun pt_val ->
                let guard = ???(up sw_val pt_val, 1) in
                let body = !!(pt, pt_val) in
                (guard, body)
              )
          )

  end

  (* the actual program to run on the switches *)
  let sw_pol = `Portwise Schemes.deterministic_car


end

module Topo = Topology.Make(Parameters)
module Model = Model.Make(Parameters)



(*===========================================================================*)
(* Analyses                                                                  *)
(*===========================================================================*)

let () = begin
  let open Parameters in

  (* TEST TOPOLOGY *)
  (* let topo_prog = Topo.to_probnetkat topo ~guard_links:true in *)
  (* Format.printf "%a\n\n" Syntax.pp_policy topo_prog; *)
  (* Util.timed "topo to Fdd" (fun () -> ignore (Fdd.of_pol topo_prog)); *)

  (* TEST PARSING *)
  (* ignore (parse_trees @@ base_name ^ "-disjointtrees.trees"); *)

  let model = Util.timed "building model" (fun () -> Model.make ()) in
  Format.printf "%a\n\n" Syntax.pp_policy model;
  let fdd = Util.timed "model to Fdd" (fun () -> Fdd.of_pol model) in
  printf ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> DONE\n%!";
  let fdd = Fdd.modulo fdd [Parameters.pt; Parameters.counter] in
  printf "fdd mod final port = %s\n" Fdd.(to_string (simplify fdd));
  let teleport = Fdd.of_pol (Model.teleportation ()) in
  printf "teleport = %s\n" (Fdd.to_string teleport);
  let is_teleport = Fdd.equivalent fdd teleport in
  printf "equivalent to teleportation: %s\n" (Bool.to_string is_teleport);
  Fdd.to_dotfile fdd (base_name ^ ".fdd.dot");

end
