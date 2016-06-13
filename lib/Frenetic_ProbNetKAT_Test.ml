open Core.Std
open Frenetic_ProbNetKAT_Interpreter

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
