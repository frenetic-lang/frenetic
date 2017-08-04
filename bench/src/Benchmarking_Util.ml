open Core

let profile f =
  let t1 = Unix.gettimeofday () in
  let r = f () in
  let t2 = Unix.gettimeofday () in
  (t2 -. t1, r)

let float_sum lst = List.fold_left lst ~init:0.0
  ~f:(fun x y -> x +. y)

let int_sum lst = List.fold_left lst ~init:0
  ~f:(fun x y -> x + y)
