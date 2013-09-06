(*
open NetKAT_Types
open NetCore_Verify
open SDN_Types
open VInt

let test_verify testname input program output bool= 
  check testname input program output (Some bool)
  

TEST "verify modify" = 
  test_verify "verify modify" Id (Mod (Switch, 12)) (Test (Switch, 12)) true

TEST "verify test" =
    let pr = Test (Header SDN.EthSrc, Int48 100L) in
  test_verify pr (ite pr Id Drop)

TEST "verify negation" =
	let pr = Test (Header SDN.EthSrc, Int48 200L) in
  test_verify (Neg pr) (ite pr Drop Id)

TEST "verify negation of sum" =
	    let pr = Seq (Test (Header SDN.EthSrc, Int48 0L), Test (Header SDN.EthDst, Int48 0L)) in
  test_verify
    (Neg pr)
    (* order flipped, a canonical ordering from to_netkat would be great *)
    (ite (Seq (Test (Header SDN.EthDst, Int48 0L), Test (Header SDN.EthSrc, Int48 0L)))
       Drop
       (* trivial optimization *)
       (ite (Test (Header SDN.EthSrc, Int48 0L)) Id Id))

(* TODO(arjun): Prove that this is true using the axioms of NetKAT. *)
TEST "commute test annihilator" =
  test_verify
    (Seq (Mod (Header SDN.EthSrc, Int48 1L), Test (Header SDN.EthSrc, Int48 0L)))
    Drop

TEST "commute test different fields" =
  test_verify
    (Seq (Mod (Header SDN.EthSrc, Int48 1L), Test (Header SDN.EthDst, Int48 0L)))
    (ite (Test (Header SDN.EthDst, Int48 0L))
       (Mod (Header SDN.EthSrc, Int48 1L))
       Drop)

(* trivial optimization possible *)
TEST "commute same field" =
  test_verify
    (Seq (Mod (Header SDN.EthSrc, Int48 1L), Test (Header SDN.EthSrc, Int48 1L)))
    (ite Id
       (Mod (Header SDN.EthSrc, Int48 1L))
       Drop)

(* trivial optimization possible *)
TEST "same field, two values = drop" =
  test_verify
    (Seq (Test (Header SDN.EthSrc, Int48 1L), Test (Header SDN.EthSrc, Int48 0L)))
    (ite (Test (Header SDN.EthSrc, Int48 1L)) Drop Drop)

*)
