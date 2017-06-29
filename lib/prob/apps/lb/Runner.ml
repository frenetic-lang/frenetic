open Core
open ProbNetKAT
open ProbNetKAT_Packet_Repr

module Mc = ProbNetKAT_Mc

let fprintf = Format.fprintf
let fmt = Format.std_formatter

module Dense = Owl.Dense.Matrix.D
module Sparse = Owl.Sparse.Matrix.D

let time f =
  let t1 = Unix.gettimeofday () in
  let r = f () in
  let t2 = Unix.gettimeofday () in
  (t2 -. t1, r)

let print_time time =
  printf "time: %.4f\n" time

let run ?(print=true) ?(lbl=true) ?(transpose=false) p =
  printf "\n========================= EIGEN ==========================\n\n%!";
  fprintf fmt "policy = %a\n\n%!" pp_policy p;
  let dom = domain p in
  let module Repr = ProbNetKAT_Packet_Repr.Make(struct let domain = dom end) in
  let n = Repr.Index0.max.i + 1 in
  let module Mc = ProbNetKAT_Mc.MakeOwl(Repr) in
  if print && not lbl then begin
    fprintf fmt "index packet mapping:\n%!";
    Array.init n ~f:ident
    |> Array.iter ~f:(fun i -> fprintf fmt " %d = %a\n%!" i Repr.Index0.pp' i);
    fprintf fmt "\n%!";
  end;
  let (t, mc) = time (fun () -> Mc.of_pol p) in
  if print then begin (if transpose then Sparse.transpose else ident) mc |> Sparse.to_dense |>
    Format.printf "@[MATRIX:@\n%a@\n@]@."
      (if not lbl then Owl_pretty.pp_fmat else
         Owl_pretty.pp_labeled_fmat
          ~pp_left:(Some (fun fmt -> fprintf fmt "%a|" Repr.Index.pp'))
          ~pp_head:None
          ~pp_foot:None
          ~pp_right:None ())
  end;
  print_time t;
  ()


