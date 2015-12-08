open Core.Std
open Frenetic_NetKAT
open Frenetic_ProbNetKAT
open Pol

let t1 = Switch 1L
let p1 = id
let p2 = mk_fresh_choice (mk_seq dup id) (mk_seq dup (mk_filter t1))

let main : unit = begin
  printf "%s" (ProbAuto.to_string (ProbAuto.of_pol' p2));
  (* printf "%s" (DetState.to_string (DetState.of_pol p1)); *)
  (* printf "%s" (ProbState.to_string (ProbState.of_pol p1)); *)
end
