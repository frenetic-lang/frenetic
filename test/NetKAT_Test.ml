module QCGen = QuickCheck_gen

module TestHeaders = struct
  type header = Switch | Port | Src | Dst
  type value = A | B | C | D
  type payload = unit

  let switch = Switch
  let port = Port

  let header_to_string h = match h with
    | Switch -> "Switch"
    | Port -> "Port"
    | Src -> "Src"
    | Dst -> "Dst"

  let value_to_string v = match v with
    | A -> "A"
    | B -> "B"
    | C -> "C"
    | D -> "D"

  let format_header fmt x = Format.pp_print_string fmt (header_to_string x)
  let format_value fmt x = Format.pp_print_string fmt (value_to_string x)
  let compare_header = Pervasives.compare

  let all_headers = [ Src; Dst ]
  let arbitrary_header = QCGen.elements all_headers
  let arbitrary_headerval = QCGen.elements [ A; B; C; D ]
  let arbitrary_payload = QCGen.ret_gen ()
end

module NetKAT = Semantics.Make (TestHeaders)
module Compiler = LocalCompiler.Make (TestHeaders) (NetKAT)
module NetKATArb = NetKAT_Arbitrary.Make (NetKAT) (TestHeaders)

open NetKAT
open TestHeaders

let test_compile lhs rhs =
  let rhs' = Compiler.Local.to_netkat (Compiler.Local.of_policy lhs) in
  if rhs' = rhs then
    true
  else
    (Format.printf "compile @,%a@, produced %a@,,@,expected %a\n%!"
       format_policy lhs format_policy rhs' format_policy rhs;
     false)

let ite (pred : pred) (then_pol : policy) (else_pol : policy) : policy =
  Par (Seq (Filter pred, then_pol), Seq (Filter (Neg pred), else_pol))

TEST "compile drop" =
  test_compile (Filter False) (Filter False)

TEST "compile test" =
  let pr = Test (Src, A) in
  test_compile (Filter pr) (Filter pr)

TEST "compile negation" =
  let pr = Test (Src, A) in
  test_compile (Filter (Neg pr)) (Filter (Neg pr))

TEST "compile negation of conjunction" =
  let pr = And (Test (Src, A), Test (Dst, A)) in
  test_compile
    (Filter (Neg pr))
    (Par (Filter (And (Neg (Test (Dst, A)), Test (Src, A))),
          Filter (Neg (Test (Src, A)))))

TEST "commute test annihilator" =
  test_compile
    (Seq (Mod (Src, B), Filter (Test (Src, A))))
    (Filter False)

TEST "commute test different fields" =
  test_compile
    (Seq (Mod (Src, B), Filter (Test (Dst, A))))
    (Seq (Filter (Test (Dst, A)), Mod (Src, B)))

(* trivial optimization possible *)
TEST "commute same field" =
  test_compile
    (Seq (Mod (Src, B), Filter (Test (Src, B))))
    (Mod (Src, B))

(* trivial optimization possible *)
TEST "same field, two values = drop" =
  let pr1 = Test (Src, B) in
  let pr2 = Test (Src, A) in
  test_compile
    (Filter (And (pr1, pr2)))
    (Filter False)

TEST "par1" =
  test_compile
    (Par(Mod (Src, B),
	 ite
	   (Test (Src, B))
	   (Mod (Src, C))
	   (Mod (Src, D))))
    (ite
       (Test (Src, B))
       (Par (Mod (Src, B),
	     Mod (Src, C)))
       (Par (Mod (Src, B),
	     Mod (Src, D))))
       
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
    (Star (Mod (Src, B)))
    (Par (Filter True, Mod (Src, B)))

let testSrc n = Test (Src, n)
let modSrc n = Mod (Src, n)
let testDst n = Test (Dst, n)
let modDst n = Mod (Dst, n)

TEST "star modify2" =
  test_compile
    (Star (Par (modSrc A,
	        ite (testSrc A) (modSrc B) (modSrc C))))
    (ite
       (testSrc A)
       (Par (Par (Par (Filter True, modSrc A), modSrc B), modSrc C))
       (Par (Par (Par (Filter True, modSrc A), modSrc B), modSrc C)))

(*
TEST "policy that caused stack overflow on 10/16/2013" =
  test_compile
    (Par (Seq (Filter (Or (Test (Dst, B), And (Test (Dst, B), Test (Src, A)))),
            Par (Mod (Dst, A), Filter (And (Or (Test (Src, C), Test (Dst, B)),
                                          Test (Dst, A))))),
         Seq (drop, Mod (Src, B))))
    id *)

(*  Src -> A ; (filter Src = C + Dst -> C) *)
TEST "quickcheck failure on 10/16/2013" =
  test_compile
    (Seq (Mod (Src, A), Par (Filter (Test (Src, C)), Mod (Dst, C))))
    (Seq (Mod (Src, A), Mod (Dst, C)))

TEST "choice1" = 
  test_compile
    (Choice (Mod (Src, A), Mod(Src, B)))
    (Choice (Mod (Src, A), Mod(Src, B)))

TEST "choice2" = 
  test_compile
    (Seq (Filter (Test(Src,C)), Choice (Mod (Src, A), Mod(Src, B))))
    (Seq (Filter (Test(Src,C)), Choice (Mod (Src, A), Mod(Src, B))))

TEST "choice3" = 
  test_compile
    (Par (Seq (Filter (Test(Src,C)), Choice (Mod (Src, A), Mod(Src, B))),
          Par (Seq (Filter (Test(Src,A)), Mod (Src,C)),
	       Seq (Filter (Test(Src,B)), Mod (Src,C)))))
    (Par (Seq (Filter (Test(Src,A)), Mod (Src, C)),
     Par (Seq (Filter (Test(Src,B)), Mod (Src, C)),
	 (Seq (Filter (Test(Src,C)), Choice(Mod(Src,A), 
				     Choice(Par(Mod(Src,A), Mod(Src,B)),
					    Mod(Src,B))))))))

(* TEST "quickcheck local compiler" = *)
(*   let testable_pol_pkt_to_bool = *)
(*     let open QuickCheck in *)
(*     let open QCGen in *)
(*     testable_fun *)
(*       (resize 4 *)
(*        (NetKATArb.arbitrary_policy >>= fun pol -> *)
(*           NetKATArb.arbitrary_packet >>= fun pkt -> *)
(*             Format.eprintf "Policy: %s\n%!" (NetKAT.string_of_policy pol); *)
(*             ret_gen (pol, pkt))) *)
(*       (fun (pol,pkt) -> NetKAT.string_of_policy pol) *)
(*       testable_bool in *)
(*   let prop_compile_ok (pol, pkt) = *)
(*     let open NetKAT in *)
(*     NetKAT.PacketSetSet.compare *)
(*       (NetKAT.eval pkt pol) *)
(*       (NetKAT.eval pkt (Compiler.Local.to_netkat (Compiler.Local.of_policy pol))) = 0 in *)
(*   let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in *)
(*   match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with *)
(*     | QuickCheck.Success -> true *)
(*     | _ -> failwith "quickchecking failed" *)
