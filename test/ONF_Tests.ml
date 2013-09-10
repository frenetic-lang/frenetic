open NetKAT_Types
module SDN = SDN_Types
open VInt

let compile (pol : policy) : policy = ONF.to_netkat (ONF.compile pol)

let test_compile lhs rhs =
  let lhs' = compile lhs in
  if lhs' = rhs then
    true
  else
    (Format.printf "compile @,%a@, produced %a@,,@,expected %a\n%!"
       format_policy lhs format_policy lhs' format_policy rhs;
     false)

let ite (pred : pred) (then_pol : policy) (else_pol : policy) : policy =
  Par (Seq (Filter pred, then_pol), Seq (Filter (Neg pred), else_pol))

TEST "compile drop" =
  compile (Filter Drop) = (Filter Drop)

TEST "compile test" =
  let pr = Test (Header SDN.EthSrc, Int48 100L) in
  test_compile (Filter pr) (ite pr (Filter Id) (Filter Drop))

TEST "compile negation" =
  let pr = Test (Header SDN.EthSrc, Int48 200L) in
  test_compile (Filter (Neg pr)) (ite pr (Filter Drop) (Filter Id))

TEST "compile negation of sum" =
  let pr = And (Test (Header SDN.EthSrc, Int48 0L), Test (Header SDN.EthDst, Int48 0L)) in
  test_compile
    (Filter (Neg pr))
    (* order flipped, a canonical ordering from to_netkat would be great *)
    (ite (And (Test (Header SDN.EthDst, Int48 0L), Test (Header SDN.EthSrc, Int48 0L)))
       (Filter Drop)
       (* trivial optimization *)
       (ite (Test (Header SDN.EthSrc, Int48 0L)) (Filter Id) (Filter Id)))

(* TODO(arjun): Prove that this is true using the axioms of NetKAT. *)
TEST "commute test annihilator" =
  test_compile
    (Seq (Mod (Header SDN.EthSrc, Int48 1L), Filter (Test (Header SDN.EthSrc, Int48 0L))))
    (Filter Drop)

TEST "commute test different fields" =
  test_compile
    (Seq (Mod (Header SDN.EthSrc, Int48 1L), Filter (Test (Header SDN.EthDst, Int48 0L))))
    (ite (Test (Header SDN.EthDst, Int48 0L))
       (Mod (Header SDN.EthSrc, Int48 1L))
       (Filter Drop))

(* trivial optimization possible *)
TEST "commute same field" =
  test_compile
    (Seq (Mod (Header SDN.EthSrc, Int48 1L), Filter (Test (Header SDN.EthSrc, Int48 1L))))
    (ite Id
       (Mod (Header SDN.EthSrc, Int48 1L))
       (Filter Drop))

(* trivial optimization possible *)
TEST "same field, two values = drop" =
  test_compile
    (Filter (And (Test (Header SDN.EthSrc, Int48 1L), Test (Header SDN.EthSrc, Int48 0L))))
    (ite (Test (Header SDN.EthSrc, Int48 1L)) (Filter Drop) (Filter Drop))

