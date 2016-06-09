open Core.Std
open Frenetic_ProbNetKAT_Interpreter

module PreciseProb = struct
  include Num
  let pp_num f n = Printf.printf "%s" (string_of_num n)
  let num_of_sexp _ = failwith "aaa" (*function

    | _ -> num_of_string str *)
  let sexp_of_num n = Sexp.of_string (string_of_num n)
  type t = num [@@deriving sexp, compare, show]
  let one = Int 1
  let (+) a b = a +/ b
  let ( * ) a b = a */ b
  let to_string = string_of_num
end

include Interp(Pkt)(PreciseProb)

let pk1 = Pkt.make ()
let pk2 = Pkt.make ~switch:1 ()

let (//) (a : int) (b : int) = Num.(num_of_int a // num_of_int b)

let mk_simple_dist alist =
  List.map ~f:(fun (pk, prob) -> (HSet.singleton pk, prob)) alist
  |> Dist.T.of_alist
  |> function
    | `Duplicate_key _ -> assert false
    | `Ok x -> x

let d1 = mk_simple_dist [(pk1, 1//2); (pk2, 1//2)]

let p1 = Pol.( ?@ [!!(Port 1) @ 1//2 ; !!(Port 2) @ 1//2] )
let p2 = Pol.( ?@ [(!!(Port 1) || !!(Port 2)) @ 1//2 ; Drop @ 1//2] )

let n = 0

let show_dist d =
  Dist.sexp_of_t d
  |> Sexp.to_string
  |> Printf.printf "%s\n"


let () = begin
  show_dist d1;
  show_dist (eval n p1 (HSet.singleton pk1));
  show_dist (eval n p2 (HSet.singleton pk1))
end
