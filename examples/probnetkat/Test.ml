open Core.Std
open Frenetic_NetKAT
open Frenetic_ProbNetKAT
open Pol

let t1 = Switch 1L
let p1 = mk_fresh_choice (mk_seq dup id) (mk_seq dup (mk_filter t1))

let main : unit = begin
  let a = ProbAuto.of_pol' p1 in
  printf "%s" (ProbAuto.to_string a);
end
