open Core

let time f x =
  let t1 = Unix.gettimeofday () in
  let r = f x in
  let t2 = Unix.gettimeofday () in
  (t2 -. t1, r)

let timed descr f =
  printf "%s...%!" descr;
  let t, y = time f () in
  printf " %f" t;
  y

let print_time time =
  printf "time: %.4f\n" time

let map_fst xs ~f =
  List.map xs ~f:(fun (x,y) -> (f x, y))

let map_snd xs ~f =
  List.map xs ~f:(fun (x,y) -> (x, f y))

let tap x ~f =
  f x; x
