open Core
open ProbNetKAT
open ProbNetKAT_Packet_Repr

module Mc = ProbNetKAT_Mc

let fprintf = Format.fprintf
let fmt = Format.std_formatter

module Dense = Owl.Dense.Matrix.D
module Sparse = Owl.Sparse.Matrix.D

let run p =
  fprintf fmt "\n===========================================================\n\n%!";
  fprintf fmt "policy = %a\n\n%!" pp_policy p;
  let dom = domain p in
  let module Repr = ProbNetKAT_Packet_Repr.Make(struct let domain = dom end) in
  let n = Repr.Index.max.i + 1 in
  let module Mc = ProbNetKAT_Mc.Make(Repr) in
  fprintf fmt "index packet mapping:\n%!";
  Array.init n ~f:ident
  |> Array.iter ~f:(fun i -> fprintf fmt " %d = %a\n%!" i Repr.Index.pp' i);
  fprintf fmt "\n%!";
  Sparse.print (Mc.of_pol p);
  ()

let () = begin
(*   printf "Hello, World!\n";
  printf "Here is a nice matrix:\n\n%a\n" pp_mat mat;
  let pk = Field.Map.empty in
  printf "codepoint = %d\n" (Codepoint.(of_pk pk) :> int);
  printf "max codepoint = %d\n" (Codepoint.max :> int);
  ignore (Codepoint.to_pk Codepoint.max); *)
  let open ProbNetKAT.Syntax in
  let open ProbNetKAT.Convenience in
  let pwhile n =
    While (??("f", 0),
    ?@[
      Skip,       Q.(Int.(n-1)//n);
      !!("f", 1), Q.(1//n)
    ])
  in
  let pwhile' =
    While (??("f", 0),
    ?@[
      Skip,       Q.(1//1);
      !!("f", 1), Q.(0//1)
    ])
  in
  Sparse.(print (eye 3));
  run Skip;
  run Drop;
  run (??("f", 0));
  run (While (??("f", 0), !!("f", 1)));
  run (pwhile 10);
  run (pwhile 100);
  run (pwhile 100_000_000);
  try run pwhile' with e -> printf "%s\n" (Exn.to_string e);
(*   let uniform =
    seqi 4 ~f:(fun i -> let l = sprintf "l%d" i in 
                ?@[!!(l,0), Q.(1//2);
                   !!(l,1), Q.(1//2)])
    >>
    While (
      seqi 4 ~f:(fun i -> let l = sprintf "l%d" i in
                          let t = sprintf "take%d" i in
                          )) *)

end
