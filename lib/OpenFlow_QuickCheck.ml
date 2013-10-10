(* Contains useful QuickCheck functions. All of these should eventually be sent
 * upstream.
 *)

module Gen = QuickCheck_gen

(* arbitrary instance for usigned integers. Still uses the `int` type. *)
let arbitrary_uint = Gen.sized (fun n -> Gen.choose_int (0, n))
