open Core
open ProbNetKAT
open ProbNetKAT_Packet_Repr
open ProbNetKAT.Syntax.Dumb

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

let identity = fun x -> x

let rec test_filter test_fn (query:policy) i =
  match query.p with
  | Skip -> true
  | Test (f, n) -> test_fn f n i
  | Neg a -> not (test_filter test_fn a i)
  | Or (a1, a2) ->
    test_filter test_fn a1 i || test_filter test_fn a2 i
  | Seq (a1, a2) ->
    test_filter test_fn a1 i && test_filter test_fn a2 i
  | _ -> failwith "Invalid query"

let run ?(print=true) ?(lbl=true) ?(transpose=false) ?(debug=false)
      ?(row_query=skip) ?(col_query=skip) p =
  printf "\n========================= EIGEN ==========================\n\n%!";
  fprintf fmt "policy = %a\n\n%!" pp_policy p;
  let dom = domain p in
  let module Repr = ProbNetKAT_Packet_Repr.Make(struct let domain = dom end) in
  let n = Repr.Index0.max.i + 1 in
  let module Mc = ProbNetKAT_Mc.MakeOwl(Repr) in
  let (t, mc) = time (fun () -> Mc.of_pol ~debug p) in
  if print then begin
    let dense_mc = (if transpose then Sparse.transpose else ident) mc |>
                   Sparse.to_dense in
    let rows = Dense.filteri_rows (fun i _ -> test_filter Repr.Index.test' row_query i) dense_mc in
    let cols = Dense.filteri_cols (fun i _ -> test_filter Repr.Index.test' col_query i) dense_mc in
    let fd_mc = Dense.rows dense_mc (Array.map ~f:(fun i -> i - 1) rows) in
    let fd_mc = Dense.cols fd_mc (Array.map ~f:(fun i -> i - 1) cols) in
    (* let fd_mc = Dense.cols fd_mc cols in *)
    let print_mat ?(row_map=identity) ?(col_map=identity) =
      Format.printf "@[MATRIX:@\n%a@\n@]@."
        (if not lbl then Owl_pretty.pp_fmat else
           Owl_pretty.pp_labeled_fmat
             ~pp_left:(Some (fun fmt i -> fprintf fmt "%a|" Repr.Index.pp' (row_map i)))
             ~pp_head:(Some (fun fmt i -> fprintf fmt "%a|" Repr.Index.pp' (col_map i)))
             ~pp_foot:None
             ~pp_right:None ()) in
    print_mat ~row_map:(fun x -> Array.nget rows (x-1)) ~col_map:(fun x -> Array.nget cols (x-1)) fd_mc;
    (* print_mat dense_mc; *)
  end;
  print_time t;
  ()


