open Core.Std
open Frenetic_NetKAT
open Frenetic_ProbNetKAT
open Pol

let show_pol p =
  printf "%s\n" (Pol.to_string p)

let show_prob p =
  show_pol p;
  printf "%s\n" (ProbAuto.to_string (ProbAuto.of_pol' p))

let show_det p =
  show_pol p;
  printf "%s\n" (DetAuto.to_string (DetAuto.of_pol' p))

let mk_hop = mk_seq dup

let t1 = Switch 1L

(* equivalentl to id, but does not syntactically collapse to id *)
let id' = mk_or (mk_filter t1) (mk_filter_out t1)

(* show off determinization, and limits of determinization *)
let p1 = mk_union (mk_hop id) (mk_hop id')
let p1' = mk_fresh_choice (mk_hop id) (mk_hop id')

(* star stuff *)
let p2 = mk_star dup
let p2' = mk_fresh_choice dup id |> mk_star


let main : unit = begin
  show_prob p2;
  show_prob p2';
  show_prob p1;
  show_prob p1';
end
