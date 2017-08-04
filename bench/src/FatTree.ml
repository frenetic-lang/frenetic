open Core
open Frenetic_NetKAT
open Frenetic_NetKAT_Optimize

let build_list (n : int) ~(f : int -> 'a) : 'a list =
  let rec loop (i : int): 'a list =
    if i = n then []
    else (f i) :: loop (i + 1) in
  loop 0

type core = { k: int; sw: int }
type aggregation =  { k: int; pod: int; sw: int }
type edge = { k: int; pod: int; sw: int }

type switch
  = Core of core
  | Aggregation of aggregation
  | Edge of edge

module Switch : sig
  val get_id : switch -> switchId
end = struct

  let next_id = ref 0L

  let known_switches = Hashtbl.Poly.create ()

  let get_id sw =
    let default () =
      let id = !next_id in
      next_id := Int64.(id + 1L);
      id in
    Hashtbl.Poly.find_or_add known_switches sw ~default

end

(* Each core switch is connected to pod n on port n *)
let core_switch_policy ~(k : int) (sw : int) : policy =
  (* There are k ports, thus k rules to direct traffic to each pod *)
  (* 10.pod.0.0/16 -> port:=pod (for pod in [0, k-1]) *)
  let pols = build_list k ~f:(fun pod ->
    let pod = Int32.of_int_exn pod in
    let prefix = Int32.(0x0a000000l + shift_left pod 16) in
    (* <:netkat< filter (ethType = 0x800 && ip4Dst = $prefix/16); port := $pod >> *)
    let open Frenetic_NetKAT_Optimize in 
    let open Frenetic_NetKAT in 
    mk_seq
      (mk_filter (mk_big_and [Test(EthType 0x800); Test(IP4Dst(prefix,16l))]))
      (Mod(Location(Physical(pod))))
  ) in
  let pol = mk_big_union pols in
  let sw_id = Switch.get_id (Core { k; sw }) in
  (* <:netkat< filter (switch = $sw_id); $pol >> *)
  let open Frenetic_NetKAT_Optimize in 
  let open Frenetic_NetKAT in 
  mk_seq (mk_filter (Test(Switch sw_id))) pol

let core_policy ~(k : int) : policy =
  build_list ((k / 2) * (k / 2)) ~f:(core_switch_policy ~k) |> mk_big_union

(* Send traffic as follows:

     pod 0, 1 -> port 0
     pod 2, 3 -> port 1
     ...

   For k / 2 ports

 *)
let aggregation_to_core_hashed ~(k : int) ~(pod : int) ~(sw : int) : policy =
  (* Only need k / 2 rules if we use the prefix 10.2i/15 *)
  let pol = build_list (k / 2) ~f:(fun i ->
    let i = Int32.of_int_exn i in
    (* 10.i0.0.0/15 *)
    let prefix = Int32.(shift_left 0x0al 24 + shift_left (2l * i) 16) in
    (* <:netkat<filter (ip4Dst = $prefix/15); port := $i>> *)
    let open Frenetic_NetKAT_Optimize in 
    let open Frenetic_NetKAT in 
    mk_seq (mk_filter (Test(IP4Dst(prefix,15l)))) (Mod(Location(Physical(i)))))
    |> mk_big_union in
  (* 10.pod.0.0/16 *)
  let local = Int32.(shift_left 0x0al 24 +
                     shift_left (of_int_exn pod) 16) in
  (* <:netkat<filter (ethType = 0x800 && !(ip4Dst = $local/16)); $pol>> *)
  let open Frenetic_NetKAT_Optimize in 
  let open Frenetic_NetKAT in 
  mk_seq
    (mk_filter (mk_big_and [Test(EthType 0x800); mk_not (Test(IP4Src(local,16l)))]))
    pol

let aggregation_switch_policy
  (to_core : k:int -> pod:int -> sw:int -> policy)
  ~(k : int) ~(pod : int) ~(sw : int) : policy =
  (* Traffic for 10.pod.sw.* must be sent to the edge switch for pod.sw, which
     is connected to port sw + k / 2 where 0 <= sw < k / 2 *)
  let to_edge = build_list (k / 2) ~f:(fun sw' ->
    let pod = Int32.of_int_exn pod in
    let sw' = Int32.of_int_exn sw' in
    (* 10.pod.sw.0/24 *)
    let prefix = Int32.(0x0a000000l + shift_left pod 16 + shift_left sw' 8) in
    let k = Int32.of_int_exn k in
    let pt = Int32.(sw' + (k / 2l)) in
    (* <:netkat< filter (ethType = 0x800 && ip4Dst = $prefix/24); port := $pt >> *)
    let open Frenetic_NetKAT_Optimize in 
    let open Frenetic_NetKAT in 
    mk_seq 
      (mk_filter (mk_big_and [Test(EthType 0x800); Test(IP4Dst(prefix,24l))]))
      (Mod(Location(Physical(pt)))))
    |> mk_big_union in
  (* We can send traffic to any one of k/2 core switches, by sending traffic
     out of ports 0 .. (k / 2). *)
  let to_core = to_core ~k ~pod ~sw in
  let sw_id = Switch.get_id (Aggregation { k; pod; sw }) in
  (* <:netkat< filter (switch = $sw_id); ($to_edge + $to_core) >> *)
  let open Frenetic_NetKAT_Optimize in 
  let open Frenetic_NetKAT in 
  mk_seq 
    (mk_filter (Test(Switch sw_id)))
    (mk_union to_edge to_core)

let aggregation_policy ~(k : int) : policy =
  build_list k ~f:(fun pod ->
    build_list (k / 2) ~f:(fun sw ->
      aggregation_switch_policy aggregation_to_core_hashed ~k ~pod ~sw) |>
    mk_big_union) |>
  mk_big_union

(* Sends traffic for pod N out of port N / 2, for 0 <= N < K. *)
let edge_to_aggregation_hashed ~(k : int) ~(pod : int) ~(sw : int) : policy =
  (* Only need k / 2 rules if we use the prefix 10.2i/15 *)
  let pol = build_list (k / 2) ~f:(fun i ->
    let i = Int32.of_int_exn i in
    (* 10.i0.0.0/15 *)
    let prefix = Int32.(shift_left 0x0al 24 + shift_left (2l * i) 16) in
    (* <:netkat<filter (ip4Dst = $prefix/15); port := $i>> *)
    let open Frenetic_NetKAT_Optimize in 
    let open Frenetic_NetKAT in 
    mk_seq (mk_filter (Test(IP4Dst(prefix,15l)))) (Mod(Location(Physical(i)))))
    |> mk_big_union in
  (* 10.pod.sw.0/24 *)
  let local = Int32.(shift_left 0x0al 24 +
                     shift_left (of_int_exn pod) 16 +
                     shift_left (of_int_exn sw) 8) in
  (* <:netkat<filter (ethType = 0x800 && !(ip4Dst = $local/24)); $pol>> *)
  let open Frenetic_NetKAT_Optimize in 
  let open Frenetic_NetKAT in 
  mk_seq 
    (mk_filter (mk_big_and [Test(EthType 0x800); mk_not (Test(IP4Src(local,24l)))]))
    pol

let edge_switch_policy
  (edge_to_aggregation : k:int -> pod:int -> sw:int -> policy)
  ~(k : int) ~(pod : int) ~(sw : int) : policy =
  (* Traffic for 10.pod.sw.n must be sent to that host at port n ,
     for 0 <= n < k / 2. *)
  let prefix = Int32.(0x0a000000l +
                      shift_left (of_int_exn pod) 16 +
                      shift_left (of_int_exn sw) 8) in
  let to_hosts = build_list (k / 2) ~f:(fun n ->
    let ip = Int32.(prefix + of_int_exn n) in
    let pt = Int32.of_int_exn n in
    (* <:netkat< filter (ethType = 0x800 && ip4Dst = $ip/32); port := $pt >> *)
    let open Frenetic_NetKAT_Optimize in 
    let open Frenetic_NetKAT in 
    mk_seq 
      (mk_filter (mk_big_and [Test(EthType 0x800); Test(IP4Dst(ip,32l))]))
      (Mod(Location(Physical(pt)))))
    |> mk_big_union in
  (* We can send other traffic to any one of the k/2 aggregation switches
     in our pod by sending out of port [k/2, k). *)
  let to_aggregation = edge_to_aggregation_hashed ~k ~pod ~sw in
  let sw_id = Switch.get_id (Edge { k; pod; sw }) in
  (* <:netkat<filter (switch = $sw_id); ($to_hosts + $to_aggregation)>> *)
  let open Frenetic_NetKAT_Optimize in 
  let open Frenetic_NetKAT in 
  mk_seq (mk_filter (Test(Switch sw_id))) (mk_union to_hosts to_aggregation)

let edge_policy ~(k : int) : policy =
  build_list k ~f:(fun pod ->
    build_list (k / 2) ~f:(fun sw ->
      edge_switch_policy edge_to_aggregation_hashed ~k ~pod ~sw)
    |> mk_big_union)
    |> mk_big_union

let fattree_routing ~(k : int) : policy =
  assert (k >= 2 && k mod 2 = 0);
  let core = core_policy ~k in
  let aggregation = aggregation_policy ~k in
  let edge = edge_policy ~k in
  (* <:netkat<$core + $aggregation + $edge>> *)
  let open Frenetic_NetKAT_Optimize in 
  let open Frenetic_NetKAT in 
  mk_big_union [core; aggregation; edge]
