open Core.Std
open Frenetic_NetKAT_Portless

let (--) i j =
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

let geo_sum a r n = a * ((1-(Int.pow r n))/(1-r))

let i64 = Int64.of_int_exn

let single host_count =
  let children = 1 -- host_count in
  let topo_down, _ = List.fold children ~init:([], 1)
      ~f:(fun (acc, port) to_host ->
          ((Switch(1L, i64 port), Host(i64 to_host)) :: acc), port + 1) in

  let topo_up = List.map topo_down (fun (x, y) -> y, x) in
  topo_up @ topo_down

let minimal = single 2

let linear switch_count =
  let hosts = List.map (1 -- switch_count) (fun x -> (Switch(i64 x, i64 1), Host(i64 x))) in
  let switches =
    if switch_count > 1 then
      (Switch(2L, 2L), Switch(1L, 2L)) :: List.map (2 -- (switch_count - 1)) (fun x -> (Switch(i64 (x + 1), i64 2), Switch(i64 x, i64 3)))
    else
      [] in

  let topo_down = hosts @ switches in
  let topo_up = List.map topo_down (fun (x, y) -> y, x) in
  topo_up @ topo_down

let tree depth fanout =
  let rec add_hosts switch fanout position =
    let children = (((position - 1) * fanout) + 1) -- (position * fanout) in
    let switches, top_port = List.fold children ~init:([], 1)
        ~f:(fun (acc, port) to_host ->
            ((Switch(i64 switch, i64 port), Host(i64 to_host)) :: acc), port + 1)
    in switches in

  let rec add_switches switch fanout =
    let children = (((switch - 1) * fanout) + 2) -- ((switch * fanout) + 1) in
    let switches, top_port = List.fold children ~init:([], 1)
        ~f:(fun (acc, port) to_sw ->
            ((Switch(i64 switch, i64 port), Switch(i64 to_sw, i64 (fanout + 1))) :: acc), port + 1)
    in switches in

  let switch_numbers    = 1 -- (geo_sum 1 fanout (depth - 1)) in
  let switch_with_hosts = List.zip_exn
      (((geo_sum 1 fanout (depth - 1)) + 1) -- (geo_sum 1 fanout depth))
      (1 -- (Int.pow fanout (depth - 1))) in
  let sw_links = List.fold switch_numbers ~init:[] ~f:(fun acc sw -> add_switches sw fanout @ acc) in
  let topo_down = List.fold switch_with_hosts ~init:sw_links ~f:(fun acc (sw, pos) -> add_hosts sw fanout pos @ acc) in
  let topo_up = List.map topo_down (fun (x, y) -> y, x) in
  topo_up @ topo_down

type topo_name =
  | Tree of int * int
  | Linear of int
  | Single of int
  | Minimal

let topo_from_name name = match name with
  | Tree (x, y) -> tree x y
  | Linear x -> linear x
  | Single x -> single x
  | Minimal -> minimal
