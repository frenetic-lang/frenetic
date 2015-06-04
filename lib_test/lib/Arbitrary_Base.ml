open QuickCheck
module Gen = QuickCheck_gen

type int128 = int64 * int64

(* arbitrary instance for usigned integers, using `int` type. *)
let arbitrary_uint = Gen.sized (fun n -> Gen.choose_int (0, n))

(* arbitrary instance for unsigned int4, using the `int` type. *)
let arbitrary_uint4 = Gen.choose_int (0, 0xf)

(* arbitrary instance for unsigned int8, using the `int` type. *)
let arbitrary_uint8 = Gen.choose_int (0, 0xff)

(* arbitrary instance for unsigned int12, using the `int` type. *)
let arbitrary_uint12 = Gen.choose_int (0, 0xfff)

(* arbitrary instance for unsigned int16, using the `int` type. *)
let arbitrary_uint16 = Gen.choose_int (0, 0xffff)
  
(* arbitrary instance for unsigned int32, using the `int32` type. *)
let arbitrary_uint32 =
  let open Gen in
  arbitrary_uint16 >>= fun a ->
  arbitrary_uint16 >>= fun b ->
    let open Int32 in
    let hi = shift_left (of_int a) 16 in
    let lo = of_int b in
    ret_gen (logor hi lo)

(* arbitrary first `b` bits set in an Int32 *)
let arbitrary_uint32_bits b =
  Gen.choose_int32 (Int32.zero, Int32.of_int ((0x1 lsl b) - 1) )

(* arbitrary instance for unsigned int48, using the `int64` type. *)
let arbitrary_uint48 =
  let open Gen in
  arbitrary_uint16 >>= fun a ->
  arbitrary_uint16 >>= fun b ->
  arbitrary_uint16 >>= fun c ->
    let open Int64 in
    let hi = shift_left (of_int a) 32 in
    let mid = shift_left (of_int b) 16 in
    let lo = of_int c in
    ret_gen Int64.(logor (logor hi mid) lo)

(* arbitrary instance for unsigned int68, using the `int64` type. *)
let arbitrary_uint64 =
  let open Gen in
  arbitrary_uint16 >>= fun a ->
  arbitrary_uint16 >>= fun b ->
  arbitrary_uint16 >>= fun c ->
  arbitrary_uint16 >>= fun d ->
    let open Int64 in
    let hi = shift_left (of_int a) 48 in
    let mid1 = shift_left (of_int b) 32 in
    let mid2 = shift_left (of_int c) 16 in
    let lo = of_int d in
    ret_gen Int64.(logor (logor hi (logor mid1 mid2)) lo)

(* arbitrary instance for unsigned int128, using the `int64` type. *)
let arbitrary_uint128 =
  let open Gen in
  arbitrary_uint64 >>= fun a ->
  arbitrary_uint64 >>= fun b ->
    ret_gen (a,b)


(* arbitrary instance for option type, favoring `Some` rather than `None` *)
let arbitrary_option arb =
  let open Gen in
  frequency [
      (1, ret_gen None);
      (3, arb >>= fun e -> ret_gen (Some e)) ]
