open Printf
open Mininet.Types

module G = PolicyGenerator.Make (Mininet.Types)

type t = G.t


let from_mininet_raw (lst : (node * portId * node) list) =
  let g = G.empty () in
  let len = 500 in
  let weight x y = match (x, y) with
    | (Host _, Host _) -> 1
    | (Host _, Switch _) -> len
    | (Switch _, Host _) -> len
    | (Switch _, Switch _) -> 1 in
  List.iter 
    (fun (src,portId,dst) ->  G.add_edge g src portId (weight src dst) dst) 
    lst;
  g

let from_mininet filename = 
  from_mininet_raw (Mininet.parse_from_chan (open_in filename) filename)

let hosts (g : G.t) : hostAddr list =
  List.fold_right 
    (fun node lst -> match node with
      | Switch _ -> lst
      | Host addr -> addr :: lst) 
    (G.all_nodes g) []

let switches (g : G.t) : switchId list =
  List.fold_right 
    (fun node lst -> match node with
      | Switch dpid -> dpid :: lst
      | Host addr -> lst) 
    (G.all_nodes g) []

open NetCore.Syntax

let hop_to_pol (pred : predicate) (hop : node * int * portId * node) : policy =
  match hop with
    | (Mininet.Types.Switch swId, _, pt, _) ->
      Pol (And (pred, Switch swId), [To pt])
    | _ -> Empty

let all_pairs_shortest_paths (g : G.t) = 
  let all_paths = G.floyd_warshall g in
  Misc.Log.printf "[NetCorePolicyGen.ml] building SP policy.\n%!";
  let pol = List.fold_right
    (fun p pol ->
      match p with
        | (Host src, path, Host dst) -> 
          let pred = And (DlSrc src, DlDst dst) in
          let labelled_path = G.path_with_edges g path in
          let path_pol = par (List.map (hop_to_pol pred) labelled_path) in
          (*
          Misc.Log.printf "Path from %Ld to %Ld is: %s\n%!"
            src dst (policy_to_string path_pol);
          Misc.Log.printf "Path is:\n";
          List.iter (fun (src,w,edge,dst) ->
            Misc.Log.printf "%s --%s--> %s (weight %d)\n%!"
              (string_of_node src)
              (string_of_edge_label edge)
              (string_of_node dst)
              w)
            labelled_path;
          *)
          Par (path_pol,pol)
        | _ -> pol)
    all_paths
    Empty in
  Misc.Log.printf "[NetCorePolicyGen.ml] done building SP policy.\n%!";
  pol

(** [g] must be a tree or this will freeze. *)
let broadcast (g : G.t) (src : hostAddr) : policy = 
  let pred = And (DlSrc src, DlDst 0xFFFFFFFFFFFFL) in
  let rec loop parent vx = match vx with
    | Host _ -> Empty
    | Mininet.Types.Switch swId -> 
      let succs = List.filter (fun (ch, _) -> ch <> parent) (G.succs g vx) in
      let ports =  List.map (fun (_, pt) -> To pt) succs in
      let hop_pol = Pol (And (pred, Switch swId), ports) in
      par (hop_pol :: List.map (fun (succ_vx, _) ->  loop vx succ_vx) succs) in
  (* Host 0L is a dummy value, but does not affect the policy *)
  let host = Host src in
  par (List.map (fun (vx, _) -> loop host vx) (G.succs g host))

let all_broadcast_trees (g : G.t) : policy = 
  let spanning_tree = G.prim g in
  par (List.map (broadcast spanning_tree) (hosts g))
