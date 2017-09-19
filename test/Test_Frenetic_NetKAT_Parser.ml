open Core
open Frenetic_NetKAT
open Frenetic_NetKAT_Parser

let output_port p = Mod (Location(Physical(p)))
let filter_port p = Filter (Test(Location (Physical(p))))
let union3 p1 p2 p3 = Union(Union(p1, p2),p3)
let and3 p1 p2 p3 = And(And(p1, p2), p3)
let if_then_else a p q = Frenetic_NetKAT.(Union(Seq(Filter a, p), Seq(Filter (Neg a), q)))

let%test "pol_of_string returns a simple policy from a policy string in examples/drop.kat" =
  let nk_str = In_channel.read_all "examples/drop.kat" in
  pol_of_string nk_str = Filter False

let%test "pol_of_string returns a policy from a policy string in examples/example1.kat" =
  let nk_str = In_channel.read_all "examples/example1.kat" in
  pol_of_string nk_str = union3 
    (Seq (filter_port(1l), ( Union (output_port(2l), output_port(3l)) ))) 
    (Seq (filter_port(2l), output_port(1l)))
    (Seq (filter_port(3l), output_port(1l)))

let%test "pol_of_string returns a policy from a policy string in examples/fall-through-optimization.kat" =
  let nk_str = In_channel.read_all "examples/fall-through-optimization.kat" in
  pol_of_string nk_str = if_then_else 
    ( and3 (Test(Vlan(1))) (Test(VlanPcp(1))) (Test(TCPSrcPort(1))) )
    (Filter True)
    (Filter False) 

let%test "pred_of_string returns a predicate from a predicate string in examples/virtual/1/ving.kat" =
  let nk_str = In_channel.read_all "examples/virtual/1/ving.kat" in
  pred_of_string nk_str = And ( Test(VSwitch(1L)), Test(VPort(0L)) )

let%test "pred_of_string bombs out when given a policy, not a predicate, as in examples/drop.kat" =
  let nk_str = In_channel.read_all "examples/drop.kat" in
  Exn.does_raise (fun () ->
    pred_of_string nk_str = Test(Location (Physical(1l)))
  )
