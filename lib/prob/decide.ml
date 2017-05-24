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

let pp_sparse m =
  Sparse.to_dense m
  |> fprintf fmt "%a\n" Owl_pretty.pp_fmat

let run ?(print=true) p =
  fprintf fmt "\n===========================================================\n\n%!";
  fprintf fmt "policy = %a\n\n%!" pp_policy p;
  let dom = domain p in
  let module Repr = ProbNetKAT_Packet_Repr.Make(struct let domain = dom end) in
  let n = Repr.Index.max.i + 1 in
  let module Mc = ProbNetKAT_Mc.MakeOwl(Repr) in
  if print then fprintf fmt "index packet mapping:\n%!";
  if print then Array.init n ~f:ident
    |> Array.iter ~f:(fun i -> fprintf fmt " %d = %a\n%!" i Repr.Index.pp' i);
  if print then fprintf fmt "\n%!";
  let (t, mc) = time (fun () -> Mc.of_pol p) in
  if print then pp_sparse mc;
  print_time t;
  ()

let () = begin
(*   printf "Hello, World!\n";
  printf "Here is a nice matrix:\n\n%a\n" pp_mat mat;
  let pk = Field.Map.empty in
  printf "codepoint = %d\n" (Codepoint.(of_pk pk) :> int);
  printf "max codepoint = %d\n" (Codepoint.max :> int);
  ignore (Codepoint.to_pk Codepoint.max); *)
  (* let open ProbNetKAT.Syntax in *)
  let open ProbNetKAT.Syntax.Dumb in
  let pwhile n =
    mk_while ??("f", 0) ?@[
      skip       @ (n-1)//n;
      !!("f", 1) @ 1//n
    ]
  in
  (** this while loop is singular: it never terminates  *)
  let pwhile' =
    mk_while ??("f", 0) ?@[
      skip       @ 1//1;
      !!("f", 1) @ 0//1
    ]
  in
  let dependent =
    ?@[ !!("f", 1) @ 1//2; !!("f", 2) @ 1//2] >>
    ite ??("f", 1)  !!("g", 1) !!("g", 2)
  in
  Sparse.(print (eye 3));
  run skip;
  run drop;
  run (??("f", 0));
  run (mk_while ??("f", 0) !!("f", 1));
  run (pwhile 10);
  run (pwhile 100);
  run (pwhile 100_000_000);
  try run pwhile' with e -> printf "%s\n" (Exn.to_string e);
  run dependent;

  let blowup n =
    seqi n ~f:(fun i -> let l = sprintf "l%d" i in
                ?@[ !!(l,0) @ 1//2; !!(l,1) @ 1//2])
  in
  run (blowup 3);
  run (blowup 10) ~print:false;
(*   let uniform =
    seqi 4 ~f:(fun i -> let l = sprintf "l%d" i in 
                ?@[!!(l,0), Q.(1//2);
                   !!(l,1), Q.(1//2)])
    >>
    mk_while (
      seqi 4 ~f:(fun i -> let l = sprintf "l%d" i in
                          let t = sprintf "take%d" i in
                          )) *)

end
