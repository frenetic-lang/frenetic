open Core

open Frenetic_Vlr

(* There's very little guidance on how to test Functors in isolation.  The Pa_ounit documentation says
   to place the tests inside the functor itself.  Then the tests are executed when you test concrete modules,
   but I don't really care for that.  It blends tests into production code, bloating the executable, etc.

   I decided to make easy types that fit the signatures, and test the module I get from passing those
   to the functor *)

module PlainLabel = struct
  type t = Var1 | Var2 | Var3  [@@deriving sexp]
  let to_string = function | Var1 -> "Var1"| Var2 -> "Var2"| Var3 -> "Var3"
  let compare = Pervasives.compare
  let hash = function | Var1 -> 1 | Var2 -> 2 | Var3 -> 3
end

module IntUnderDivision = struct
  type t = int [@@deriving sexp]

  let to_string = Int.to_string
  let compare = Pervasives.compare
  let hash (x:t) = x

  let rec gcd u v =
    if v <> 0 then (gcd v (u mod v))
    else (abs u)

  let subset_eq a b =
    (* I don't think this is correct, but it seems to work as long as numbers are relatively prime, *)
    (gcd a b) > 1

  let meet ?(tight=false) a b =
    Some (gcd a b)

  let join ?(tight=false) m n =
    (* least common multiple of a and b *)
    match m, n with
    | 0, _ | _, 0 -> Some 0
    | m, n -> Some (abs (m * n) / (gcd m n))
end

module IntUnderAddAndMult = struct
  type t = int  [@@deriving sexp]

  let to_string = Int.to_string
  let compare = Pervasives.compare
  let hash (x:t) = x

  let sum = ( + )
  let prod = ( * )
  let one = 1
  let zero = 0
end

module TestableVLR = Make(PlainLabel)(IntUnderDivision)(IntUnderAddAndMult)

let%test_module _ = (module struct
  open TestableVLR

  let init = clear_cache Int.Set.empty

(* Note: making d in VLR a private type has renedered most of these tests inoperable *)

(*
  let%test "S.get places a diagram in the current cache, and unget retrieves it" =
    init;
    unget (get (Leaf 3)) = Leaf 3

  let%test "S.unget throws an exception if you try to get a diagram not in the cache" =
    try
      init;
      unget (3) = Leaf 3
    with Not_found -> true | _ -> false
*)
  let%test "S.equal returns true if two subdiagrams are equal" =
    (* S.equal really depends on the diagram-creating procedures doing their job correctly *)
    let one_idx = const 60 in
    let two_idx = const 60 in
    equal one_idx two_idx
(*
  let%test "S.cond is a primitive to create an if-then-else node for the diagram" =
    let () = init in
    let ten_idx = const 10 in
    let twenty_idx = const 20 in
    unget (cond (Var1,30) ten_idx twenty_idx) = Branch ((Var1,30), ten_idx, twenty_idx)

  let%test "S.cond does a quick optimization if the true and false branches are equal" =
    let () = init in
    let ten_idx = const 10 in
    unget (cond (Var1,30) ten_idx ten_idx) = Leaf 10

  let%test "S.const is a primitive to create a leaf node for the diagram" =
    init;
    unget (const 60) = Leaf 60
*)
  let%test "S.const doesn't make the same leaf node twice" =
    let () = init in
    let one_idx = const 60 in
    let two_idx = const 60 in
    one_idx = two_idx

  let%test "S.drop returns leaf with zero element" =
    drop = (const 0)

  let%test "S.id returns leaf with one element" =
    id = (const 1)
(*
  let%test "S.const makes a decision diagram with a constant" =
    init;
    unget (const 3) = Leaf 3

  let%test "S.atom makes a single branch with constant leaves" =
    let () = init in
    let ten_idx = const 10 in
    let twenty_idx = const 20 in
    unget (atom (Var2,3) 10 20) = Branch ((Var2,3), ten_idx, twenty_idx)
*)
  let two_level_tree =
    let () = init in
    (* Note 17 and 23 below are relatively prime.  If they weren't, the tree wouldn't work *)
    cond (Var1, 17) (atom (Var2,10) 300 600) (atom (Var1, 23) 100 200)
(*
  let%test "S.restrict returns a smaller decision tree where the given pattern is true" =
    let fdd = two_level_tree in
    let restricted_fdd = restrict [ (Var1,17) ] fdd in
    unget restricted_fdd = Branch ((Var2, 10), (const 300), (const 600))
*)
  let%test "S.restrict returns the entire tree if the variable is not referenced" =
    let fdd = two_level_tree in
    let restricted_fdd = restrict [ (Var3,4000) ] fdd in
    restricted_fdd = fdd

  let%test "S.fold does the normal fold operation over a diagram" =
    let fdd = two_level_tree in
    (* This is nonsensical, but easy to test.  It just doubles the leaf values, then adds them together with the test pattern values *)
    let add_across (v,l) m n : int = l + m + n in
    let double x : int = x * 2 in
    fold ~f:double ~g:add_across fdd = 2450

  let%test "S.map_r does the normal map operation, applying function to all leaf nodes and ignoring branch nodes" =
    let fdd = two_level_tree in
    let double x = x * 2 in
    let expected_fdd = cond (Var1, 17) (atom (Var2,10) 600 1200) (atom (Var1, 23) 200 400) in
    map_r double fdd = expected_fdd

  (* TODO: TEST "sum" *)

  (* TODO: TEST "prod" *)

  let%test "S.clear_cache doesn't remove drop or id diagrams under any circumstances" =
    let _ = two_level_tree in
    let zero_idx = drop in
    let one_idx = id in
    clear_cache Int.Set.empty;
    zero_idx = drop && one_idx = id

  let%test "S.compressed_size returns number of leaves and branches, not counting duplicates" =
    let () = init in
    let fdd = cond (Var1, 17) (atom (Var2,10) 100 200) (atom (Var1, 23) 100 200) in
    (* The two leaves 100 and 200 are counted only once apiece *)
    compressed_size fdd = 5

  let%test "S.uncompressed_size returns number of leaves and branches, counting duplicates" =
    let () = init in
    let fdd = cond (Var1, 17) (atom (Var2,10) 100 200) (atom (Var1, 23) 100 200) in
    uncompressed_size fdd = 7
end)
  (* TODO: TEST "to_dot" *)
(*
  let%test "S.refs returns set of diagrams" =
    let () = init in
    let fdd = atom (Var1, 23) 100 200 in
    let first_child = const 100 in
    let second_child = const 200 in
    Int.Set.equal (refs fdd) (Int.Set.of_list [ 100; second_child; fdd ])
end
*)
