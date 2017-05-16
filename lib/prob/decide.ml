open Core.Std
open Lacaml.D (* double precision real *)

let printf = Printf.printf
let mat = Mat.random 100 100

let pp_mat out = Lacaml.Io.pp_lfmat () (Format.formatter_of_out_channel out)

let () = begin
  printf "Hello, World!\n";
  printf "Here is a nice matrix:\n\n%a" pp_mat mat
end
