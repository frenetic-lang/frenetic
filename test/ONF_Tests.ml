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
  compile (Filter False) = (Filter False)

TEST "compile test" =
  let pr = Test (Header SDN.EthSrc, Int48 142L) in
  test_compile (Filter pr) (Filter pr)

TEST "compile negation" =
  let pr = Test (Header SDN.EthSrc, Int48 200L) in
  test_compile (Filter (Neg pr)) (Filter (Neg pr))

TEST "compile negation of conjunction" =
  let pr = And (Test (Header SDN.EthSrc, Int48 0L), Test (Header SDN.EthDst, Int48 0L)) in
  test_compile
    (Filter (Neg pr))
    (Par (Filter (And (Neg (Test (Header SDN.EthDst, Int48 0L)), Test (Header SDN.EthSrc, Int48 0L))),
          Filter (Neg (Test (Header SDN.EthSrc, Int48 0L)))))

TEST "commute test annihilator" =
  test_compile
    (Seq (Mod (Header SDN.EthSrc, Int48 1L), Filter (Test (Header SDN.EthSrc, Int48 0L))))
    (Filter False)

TEST "commute test different fields" =
  test_compile
    (Seq (Mod (Header SDN.EthSrc, Int48 1L), Filter (Test (Header SDN.EthDst, Int48 0L))))
    (Seq (Filter (Test (Header SDN.EthDst, Int48 0L)), Mod (Header SDN.EthSrc, Int48 1L)))

(* trivial optimization possible *)
TEST "commute same field" =
  test_compile
    (Seq (Mod (Header SDN.EthSrc, Int48 1L), Filter (Test (Header SDN.EthSrc, Int48 1L))))
    (Mod (Header SDN.EthSrc, Int48 1L))

(* trivial optimization possible *)
TEST "same field, two values = drop" =
  let pr1 = Test (Header SDN.EthSrc, Int48 1L) in
  let pr2 = Test (Header SDN.EthSrc, Int48 0L) in
  test_compile
    (Filter (And (pr1, pr2)))
    (Filter False)

TEST "par1" =
  test_compile
    (Par(Mod (Header SDN.EthSrc, Int48 1L),
	 ite
	   (Test (Header SDN.EthSrc, Int48 1L))
	   (Mod (Header SDN.EthSrc, Int48 2L))
	   (Mod (Header SDN.EthSrc, Int48 3L))))
    (ite
       (Test (Header SDN.EthSrc, Int48 1L))
       (Par (Mod (Header SDN.EthSrc, Int48 2L),
	     Mod (Header SDN.EthSrc, Int48 1L)))
       (Par (Mod (Header SDN.EthSrc, Int48 3L),
	     Mod (Header SDN.EthSrc, Int48 1L))))
       
TEST "star id" =
  test_compile
    (Star (Filter True))
    (Filter True)

TEST "star drop" =
  test_compile
    (Star (Filter False))
    (Filter True)

TEST "star modify1" =
  test_compile
    (Star (Mod (Header SDN.EthSrc, Int48 1L)))
    (Par (Mod (Header SDN.EthSrc, Int48 1L), Filter True))

let testSrc n = Test (Header SDN.EthSrc, Int48 n) 
let modSrc n = Mod (Header SDN.EthSrc, Int48 n) 
let testDst n = Test (Header SDN.EthDst, Int48 n) 
let modDst n = Mod (Header SDN.EthDst, Int48 n) 

TEST "star modify2" = 
  test_compile
    (Star (Par (modSrc 1L, 
	        ite (testSrc 1L) (modSrc 2L) (modSrc 3L))))
    (ite 
       (testSrc 1L)
       (Par (modSrc 2L, Par (modSrc 1L, Filter True)))
       (Par(modSrc 3L, Par(modSrc 2L, Par(modSrc 1L, Filter True)))))
