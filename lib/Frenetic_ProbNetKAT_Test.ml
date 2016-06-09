open Core.Std
open Frenetic_ProbNetKAT_Interpreter

module PreciseProb = struct
  include Num
  let pp_num f n = Printf.printf "%s" (string_of_num n)
  let num_of_sexp _ = failwith "<>"
  let sexp_of_num n = Sexp.of_string (string_of_num n)
  type t = num [@@deriving sexp, compare, show]
  let one = Int 1
  let (+) a b = a +/ b
  let ( * ) a b = a */ b
  let to_string = string_of_num
end

module FloatProb = struct
  include Float
  let show _ = ""
end

let (/) (a : int) (b : int) : PreciseProb.t =
  Num.(num_of_int a // num_of_int b)

include Interp(Hist)(PreciseProb)

let pk1 = Hist.make ()
let pk2 = Hist.make ~switch:1 ()

let mk_simple_dist alist =
  List.map ~f:(fun (pk, prob) -> (HSet.singleton pk, prob)) alist
  |> Dist.T.of_alist_exn

let d1 = mk_simple_dist [(pk1, 1/2); (pk2, 1/2)]

open Pol
let p1 = ?@[ !!(Switch 0) , 1/2
           ; !!(Switch 1) , 1/2 ]
let p2 = ?@[ !!(Switch 0) & !!(Switch 1) , 1/2
           ; Drop                        , 1/2]

let p3 = p1 >> Star (Dup >> p1)
let p4 = p1 >> Star (??(Switch 0) >> Dup >> p1) >> ??(Switch 1)

let n = 2

let () = begin
  Dist.print d1;
  print_endline "";
  Dist.print (eval n p1 (HSet.singleton pk1));
  print_endline "";
  Dist.print (eval n p2 (HSet.singleton pk1));
  print_endline "";
  Dist.print (eval n p3 (HSet.singleton pk1));
  print_endline "";
  Dist.print (eval (2*n) p4 (HSet.singleton pk1));
end
