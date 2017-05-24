open Core.Std
open ProbNetKAT
open ProbNetKAT_Packet_Repr
open ProbNetKAT_Mc

module Repr = ProbNetKAT_Packet_Repr.Make(struct let domain = domain Skip end)
module Mc = ProbNetKAT_Mc.Make(Repr)

let fprintf = Format.(fprintf std_formatter)



let () = begin
(*   printf "Hello, World!\n";
  printf "Here is a nice matrix:\n\n%a\n" pp_mat mat;
  let pk = Field.Map.empty in
  printf "codepoint = %d\n" (Codepoint.(of_pk pk) :> int);
  printf "max codepoint = %d\n" (Codepoint.max :> int);
  ignore (Codepoint.to_pk Codepoint.max); *)
  let open ProbNetKAT.Syntax in
  let p =
    While (??("f", 0),
    ?@[
      Skip,       Q.(3//4);
      !!("f", 1) >> !!("g", 1), Q.(1//4)
    ])
  in
  fprintf "%a\n%!" pp_policy p;
  (* let m = Mc.of_pol p in *)
  ()
  (* Owl.Sparse.Matrix.D.print m *)
end
