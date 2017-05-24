open Core.Std
open ProbNetKAT
open ProbNetKAT_Packet_Repr

module Mc = ProbNetKAT_Mc

let fprintf = Format.(fprintf std_formatter)

module Dense = Owl.Dense.Matrix.D
module Sparse = Owl.Sparse.Matrix.D

let run p = 
  fprintf "%a\n%!" pp_policy p;
  Sparse.(print Mc.(of_pol p))

let () = begin
(*   printf "Hello, World!\n";
  printf "Here is a nice matrix:\n\n%a\n" pp_mat mat;
  let pk = Field.Map.empty in
  printf "codepoint = %d\n" (Codepoint.(of_pk pk) :> int);
  printf "max codepoint = %d\n" (Codepoint.max :> int);
  ignore (Codepoint.to_pk Codepoint.max); *)
  let open ProbNetKAT.Syntax in
  let pwhile =
    While (??("f", 0),
    ?@[
      Skip,       Q.(3//4);
      !!("f", 1), Q.(1//4)
    ])
  in
  Sparse.(print (eye 3));
  run Skip;
  run Drop;
  run (??("f", 0));
  run (While (??("f", 0), !!("f", 1)));
  run pwhile;
end
