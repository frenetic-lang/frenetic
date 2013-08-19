open NetKAT_Types

let compile (pol : pol) : pol = ONF.to_netkat (ONF.compile pol)

let test_compile lhs rhs =
	let lhs' = compile lhs in
	if lhs' = rhs then
	  true
	else
	  (Format.printf "compile @,%a@, produced %a@,,@,expected %a\n%!"
	    format_pol lhs format_pol lhs' format_pol rhs;
	   false)

let ite pred then_pol else_pol =
	Par (Seq (pred, then_pol), Seq (Neg pred, else_pol))

TEST "compile drop" =
  compile Drop = Drop

TEST "compile test" =
  let pr = Test (DlSrc, DlAddr 100L) in
  test_compile pr (ite pr Id Drop)

TEST "compile negation" =
  let pr = Test (DlSrc, DlAddr 200L) in
  test_compile (Neg pr) (ite pr Drop Id)

(* TODO(arjun): Prove that this is true using the axioms of NetKAT. *)
TEST "commute test annihilator" =
  test_compile
    (Seq (Set (DlSrc, DlAddr 1L), Test (DlSrc, DlAddr 0L)))
    Drop

TEST "commute test different fields" =
  test_compile
    (Seq (Set (DlSrc, DlAddr 1L), Test (DlDst, DlAddr 0L)))
    (ite (Test (DlDst, DlAddr 0L))
    	   (Set (DlSrc, DlAddr 1L))
    	 Drop)

(* trivial optimization possible *)
TEST "commute same field" =
  test_compile
    (Seq (Set (DlSrc, DlAddr 1L), Test (DlSrc, DlAddr 1L)))
    (ite Id
    	   (Set (DlSrc, DlAddr 1L))
    	   Drop)

(* trivial optimization possible *)
TEST "same field, two values = drop" =
  test_compile
    (Seq (Test (DlSrc, DlAddr 1L), Test (DlSrc, DlAddr 0L)))
    (ite (Test (DlSrc, DlAddr 1L)) Drop Drop)
