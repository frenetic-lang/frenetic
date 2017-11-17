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

  let destination = PNK.( ???(sw, 3) )


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

  let edge_map : Net.Topology.edge Int2.Map.t =
    Net.Topology.fold_edges (fun edge map ->
      let (src,_) = Net.Topology.edge_src edge in
      let (dst,_) = Net.Topology.edge_dst edge in
      let key = Topology.(sw_val topo src, sw_val topo dst) in
      Map.add map ~key ~data:edge
    )
      topo
      Int2.Map.empty

  let parse_sw sw =
    assert (String.get sw 0 = 's');
    String.slice sw 1 (String.length sw)
    |> Int.of_string

  (* switch to port mapping *)
  let parse_spf_trees () : (int list) Int.Table.t =
    let tbl = Int.Table.create () in
    let spf_file = base_name ^ "-spf.trees" in
    In_channel.(with_file spf_file ~f:(iter_lines ~f:(fun l ->
      let l = String.strip l in
      if not (String.get l 0 = '#') then
      match String.split ~on:' ' l with
      | [src; _; dst] ->
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
      let good_pt = PNK.(
        List.map pts ~f:(fun pt_val -> ???(pt,pt_val) & ???(up sw pt_val, 1))
        |> mk_big_disj
      )
      in
      let choose_port = random_walk sw in
      PNK.( choose_port >> whl (neg good_pt) choose_port )

    (* let shortest_path sw = *)


  end

  (* the actual program to run on the switches *)
  let sw_pol = `Switchwise Schemes.resilient_random_walk


end

module Topo = Topology.Make(Parameters)
module Model = Model.Make(Parameters)

let () = begin
  let open Parameters in
(*   let topo_prog = Topo.to_probnetkat topo ~guard_links:true in
  Format.printf "%a\n\n" Syntax.pp_policy topo_prog;
  Util.timed "topo to Fdd" (fun () -> ignore (Fdd.of_pol topo_prog));
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
  Fdd.to_dotfile fdd (base_name ^ ".fdd.dot"); *)

  ignore (parse_spf_trees ());
end
