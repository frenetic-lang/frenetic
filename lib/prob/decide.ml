open Core.Std
open Lacaml.D (* double precision real *)
open ProbNetKAT
open ProbNetKAT_NomPacket

let printf = Printf.printf
let mat = Mat.random 100 100

let pp_mat out = Lacaml.Io.pp_lfmat () (Format.formatter_of_out_channel out)

include ProbNetKAT_NomPacket.Make(struct let domain = domain Skip end)

let () = begin
  printf "Hello, World!\n";
  printf "Here is a nice matrix:\n\n%a\n" pp_mat mat;
  let pk = Field.Map.empty in
  printf "codepoint = %d\n" (Codepoint.(of_pk pk) :> int);
  printf "max codepoint = %d\n" (Codepoint.max :> int);
  ignore (Codepoint.to_pk Codepoint.max);
end
