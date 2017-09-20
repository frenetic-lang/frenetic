open Core
open Netkat.Syntax
open Netkat.Optimize

module Net = Frenetic.Network.Net
module Topology = Net.Topology
module Node = Frenetic.Network.Node
module Path = Net.UnitPath

let is_host topo v =
  match Node.device (Topology.vertex_to_label topo v) with
    | Node.Host -> true
    | _ -> false

let is_switch topo v =
  match Node.device (Topology.vertex_to_label topo v) with
    | Node.Switch -> true
    | _ -> false

type topo = Net.Topology.t * Topology.VertexSet.t * Topology.VertexSet.t *
  Topology.VertexSet.t

let parse_topo_file ?(log=true) ?(hostlimit=1000000) filename : topo =
  let topo = Net.Parse.from_dotfile filename in
  let vertexes = Topology.vertexes topo in
  let hosts = Topology.VertexSet.filter ~f:(is_host topo) vertexes in
  (* TODO(arjun): @jnfoster what is this check for? *)
  let _,hosts = if hostlimit < 1000000 then
    (if log then Printf.printf "We are doing the thing!\n%!" else ();
     Topology.VertexSet.(fold ~f:(fun (cntr,acc) e ->
                                    if cntr < hostlimit
                                    then (cntr + 1, add acc e)
                                    else (cntr,acc))
                              ~init:(0,empty) hosts))
    else
      (0,hosts) in
  let switches = Topology.VertexSet.filter ~f:(is_switch topo) vertexes in
  if log then printf "Topology has %d hosts and %d switches.\n%!"
   (Topology.VertexSet.length hosts) (Topology.VertexSet.length switches)
  else ();
  (topo, vertexes, switches, hosts)

let shortest_paths ((topo, _, _, _) : topo) =
  printf "Calculating all-pairs shortest paths...\n%!";
  let paths = Path.all_pairs_shortest_paths ~topo ~f:(fun vx1 vx2 ->
    match (Node.device (Topology.vertex_to_label topo vx1),
           Node.device (Topology.vertex_to_label topo vx2)) with
      | Node.Host, Node.Host -> true
      | _ -> false) in
  printf "Calculating policy (linking %d pairs of hosts).\n%!..." (List.length paths);
  let route (_, _, dst, path) =
    let dst_ip = Node.ip (Topology.vertex_to_label topo dst) in
    List.map path ~f:(fun edge ->
      let v, pt = Topology.edge_src edge in
      let n = Topology.vertex_to_label topo v in
      match Node.device n with
      | Node.Switch ->
        let i = Node.id n in
        Seq (Filter (And (Test (Switch i), Test (IP4Dst (dst_ip, 32l)))),
            Mod (Location (Physical pt)))
      | _ -> Filter False) |> mk_big_union in
  List.map paths ~f:route |> mk_big_union

let vertex_to_switch_port topo (vertex, pt) =
    let node = Topology.vertex_to_label topo vertex in
    match Node.device node with
    | Node.Switch ->
      let sw = Node.id node in
      (sw, pt)
    | _ -> failwith "vertex_to_switch_port applied to a non-switch"

let match_loc sw pt = mk_and (Test (Switch sw)) (Test (Location (Physical pt))) |> mk_filter
let match_dst dst_ip = Test (IP4Dst (dst_ip, 32l)) |> mk_filter
let set_pt pt = Mod (Location (Physical pt))
let unwrap_edge topo e =
  let (src, src_port) = Topology.edge_src e in
  let (dst, dst_port) = Topology.edge_dst e in
  let src = Topology.vertex_to_label topo src in
  let dst = Topology.vertex_to_label topo dst in
  ((src, src_port), (dst, dst_port))


let shortest_paths_global_policy ((topo, _, _, _) : topo) =
  let vertex_to_label = Topology.vertex_to_label topo in
  let lcompare (node1, pt1) (node2, pt2) = match Node.compare node1 node2 with
    | 0 -> Int32.compare pt1 pt2
    | x -> x
  in
  let paths =
    Path.all_pairs_shortest_paths ~topo ~f:(fun vx1 vx2 ->
      match (Node.device (vertex_to_label vx1),
             Node.device (vertex_to_label vx2)) with
        | Node.Host, Node.Host -> true
        | _ -> false)
    |> List.filter_map ~f:(fun (_,_,dst,path) ->
      match path with
      | [] -> None
      | _ ->
        let path = List.rev_map path ~f:(unwrap_edge topo) in
        let dst = Node.ip (vertex_to_label dst) in
        Some (dst, path))
    |> List.sort ~cmp:(fun (ip,_) (ip',_) -> Int32.compare ip ip')
    |> List.group ~break:(fun (ip,_) (ip',_) -> not (Int32.equal ip ip'))
    |> List.map ~f:(function (ip,_)::_ as lst -> (ip, List.map lst ~f:snd))
    (* SJS: disable pattern not exhasutive warning *)
    [@@ocaml.warning "-8"]
  in
  let rec route paths =
    (* INVARIANT: all paths have same dst *)
    List.sort paths ~cmp:(fun ((src,_)::_) ((src',_)::_) -> lcompare src src')
    |> List.group ~break:(fun ((src,_)::_) ((src',_)::_) -> lcompare src src' <> 0)
    |> List.map ~f:(function (hop::_)::_ as group -> (hop, List.map group ~f:List.tl_exn))
    |> List.map ~f:mk_hop
    |> mk_big_union
    (* SJS: disable pattern not exhasutive warning *)
    [@@ocaml.warning "-8"]
  and mk_hop (((src, src_pt),(dst,dst_pt)), rest) =
    match Node.device src with
    | Node.Middlebox -> assert false (* SJS *)
    | Node.Host -> (* first hop from host to switch *)
      let () = assert (Node.device dst = Node.Switch) in
      let () = assert (List.for_all rest ~f:((=) [])) in
      let dst_sw = Node.id dst in
      match_loc dst_sw dst_pt
    | Node.Switch ->
      let () = assert (List.for_all rest ~f:((<>) [])) in
      begin match Node.device dst with
      | Node.Middlebox -> assert false (* SJS *)
      | Node.Host -> (* last hop from switch to host *)
        let rest = route rest in
        let hop = set_pt src_pt in
        mk_seq rest hop
      | Node.Switch ->
        let src_sw = Node.id src in
        let dst_sw = Node.id dst in
        let hop = mk_seq (set_pt src_pt) (Link (src_sw, src_pt, dst_sw, dst_pt)) in
        let rest = route rest in
        mk_seq rest hop
      end
  in
  let ing =
    List.map paths ~f:snd
    |> List.concat
    |> List.map ~f:List.last_exn
    |> List.map ~f:snd
    |> List.dedup ~compare:lcompare
    |> List.map ~f:(fun (sw, pt) ->
        let sw = Node.id sw in
        let t1 = Test (Switch sw) in
        let t2 = Test (Location (Physical pt)) in
        mk_and t1 t2)
    |> mk_big_or
  in
  paths
  |> List.map ~f:(fun (dst_ip, paths) -> mk_seq (match_dst dst_ip) (route paths))
  |> mk_big_union
  |> mk_seq (mk_filter ing)

let rec unzip3 (lst : ('a * 'b * 'c) list) : 'a list * 'b list * 'c list = match lst with
  | [] -> ([], [], [])
  | (x,y,z) :: rest ->
    let (xs, ys, zs) = unzip3 rest in
    (x :: xs, y :: ys, z :: zs)

let rec unzip4 lst =
  match lst with
  | [] -> ([], [], [], [])
  | (x,y,z,q) :: rest ->
    let (xs, ys, zs, qs) = unzip4 rest in
    (x :: xs, y :: ys, z :: zs, q :: qs)



let edge_to_link topo e : policy =
  let ((src, src_pt), (dst, dst_pt)) = unwrap_edge topo e in
  match Node.device src, Node.device dst with
  | Node.Switch, Node.Switch ->
    let src_sw = Node.id src in
    let dst_sw = Node.id dst in
    Link (src_sw, src_pt, dst_sw, dst_pt)
  | _ -> drop

let host_of_edge topo e =
  let ((src, _), (dst, pt)) = unwrap_edge topo e in
  match Node.device src, Node.device dst with
  | Node.Host, Node.Switch ->
    let h =
      Node.name src
      |> (fun s -> String.sub s 1 (String.length s - 1))
      |> Int64.of_string
    in
    let mac = Node.mac src in
    let sw = Node.id dst in
    Some ((h, mac), (sw, pt))
  | _ -> None

let hosts_of_topo (topo : Topology.t) =
  Topology.fold_edges (fun e hs -> Option.to_list (host_of_edge topo e) @ hs) topo []

let topo_policy (topo : Topology.t) =
  Topology.fold_edges (fun e t -> mk_union t (edge_to_link topo e)) topo drop

let rec join ?(s=",") lst =
  match lst with
  | [] -> ""
  | [x] -> x
  | x::lst' -> x ^ s ^ (join ~s lst')

let switch_of_vertex v =
  match Node.device v with
  | Node.Switch -> Some (Node.id v |> Int64.to_string |> (fun s -> "s" ^ s))
  | _ -> None

let host_of_vertex v =
  match Node.device v with
  | Node.Host ->
    Node.name v |> Option.some
  | _ -> None

let device_of_vertex v =
  match Node.device v with
  | Node.Switch -> switch_of_vertex v |> (fun x -> match x with Some x -> x | None -> assert false)
  | Node.Host -> host_of_vertex v |> (fun x -> match x with Some x -> x | None -> assert false)
  | _ -> assert false

let string_of_edge topo e =
  let ((src, _), (dst, _)) = unwrap_edge topo e in
  Printf.sprintf "%s-%s" (device_of_vertex src) (device_of_vertex dst)


let print_topo filename ((topo, vertexes, _, _) : topo) host_fabric =
  let vertexes =
    Topology.VertexSet.to_list vertexes
    |> List.map ~f:(Topology.vertex_to_label topo)
  in
  let switches = List.filter_map vertexes ~f:switch_of_vertex in
  let hosts = List.filter_map vertexes ~f:host_of_vertex in
  let edges =
    Topology.fold_edges List.cons topo []
    |> List.map ~f:(string_of_edge topo)
  in
  Out_channel.with_file filename ~f:(fun out ->
    Printf.fprintf out "%s\n" (join switches);
    Printf.fprintf out "%s\n" (join hosts);
    Printf.fprintf out "%s\n" (join host_fabric);
    Printf.fprintf out "%s\n" (join edges);
    ())


let big_switch ~topo:((topo, vertexes, switches, _) : topo) =
  let hosts = hosts_of_topo topo in
  let pinout =
    List.map hosts ~f:(fun ((h, mac),(sw,pt)) ->
      mk_and (Test(Switch sw)) (Test(Location(Physical pt))))
    |> mk_big_or
  in
  let vsw = Int64.of_int 1 in
  let (vrel, vingpol, vinout) =
    List.map hosts ~f:(fun ((h, mac),(sw,pt)) ->
      let vloc = mk_and (Test(VSwitch vsw)) (Test(VPort h)) in
      let set_vloc = mk_seq (Mod (VSwitch vsw)) (Mod (VPort h)) in
      let ploc = mk_and (Test(Switch sw)) (Test(Location(Physical pt))) in
      (mk_and vloc ploc, mk_seq (mk_filter ploc) set_vloc, vloc))
    |> unzip3
    |> (fun (vrel, vingp, vinout) ->
        (mk_big_or vrel, mk_big_union vingp, mk_big_or vinout))
  in
  let vpol =
    List.map hosts ~f:(fun ((h,mac), _) ->
      let h = match Int32.of_int64 h with
        | Some h -> h
        | None -> assert false in
      let test = mk_filter (Test (EthDst mac)) in
      let action = Mod (Location (Physical h)) in
      mk_seq test action)
    |> mk_big_union
  in
  let vtopo = drop in
  let ptopo = topo_policy topo in
  (* let host_fabric =
    List.map hosts ~f:(fun ((h,_),(sw, _)) ->
      Printf.sprintf "h%Lu-s%Lu" h sw) in *)
  (vpol, vrel, vtopo, vingpol, vinout, ptopo, pinout)



