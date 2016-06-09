open Core.Std
open Frenetic_ProbNetKAT_Interpreter

module PreciseProb = struct
  include Num
  let pp_num f n = Printf.printf "%s" (string_of_num n)
  let num_of_sexp _ = failwith "<>"
  let sexp_of_num n = Sexp.of_string (string_of_num n)
  type t = num [@@deriving sexp, compare, show]
  let zero = Int 0
  let one = Int 1
  let (+) a b = a +/ b
  let ( * ) a b = a */ b
  let ( / ) a b = a // b
  let of_int = num_of_int
  let to_string = string_of_num
  let to_dec_string = approx_num_fix 10
end

module FloatProb = struct
  include Float
  let show _ = ""
  let to_dec_string = Printf.sprintf "%.10f"
end



include Interp(Hist)(PreciseProb)

(* Packets *)
let pk1 = Hist.make ()
let pk2 = Hist.make ~switch:1 ()

(* Policies  *)
open Pol
let p1 = ?@[ !!(Switch 0) , 1/2
           ; !!(Switch 1) , 1/2 ]

let p1' = ?@[ !!(Switch 0) , 9/10
           ; !!(Switch 1) , 1/10 ]

let p2 = ?@[ !!(Switch 0) & !!(Switch 1) , 1/2
           ; Drop                        , 1/2]

let p3 = p1 >> Star (Dup >> p1)
let p4 = p1' >> Star (??(Switch 0) >> Dup >> p1') >> ??(Switch 1)

(* Random Variables (i.e, Queries) *)
let q1 (pk,h) = Prob.of_int (List.length h + 1)



let () = begin
  let n = 2 in
  Dist.print (eval n p1);
  Dist.print (eval n p2);
  Dist.print (eval n p3);
  Dist.print (eval (2*n) p4);
  for n=0 to 80 do
    expectation' n p4 ~f:q1
    |> Prob.to_dec_string
    |> Printf.printf "n = %d: %s\n%!" n
  done
end
