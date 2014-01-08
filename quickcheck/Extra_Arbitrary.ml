open QuickCheck
module Gen = QuickCheck_gen

(* arbitrary instance for usigned integers, using `int` type. *)
let arbitrary_uint = Gen.sized (fun n -> Gen.choose_int (0, n))

(* arbitrary instance for unsigned int8, using the `int` type. *)
let arbitrary_uint8 = Gen.choose_int (0, 255)

(* arbitrary instance for unsigned int16, using the `int` type. *)
let arbitrary_uint16 =
  let open Gen in
  arbitrary_uint8 >>= fun a ->
  arbitrary_uint8 >>= fun b ->
    ret_gen ((a lsl 8) lor b)

(* arbitrary instance for unsigned int32, using the `int32` type. *)
let arbitrary_uint32 =
  let open Gen in
  arbitrary_uint16 >>= fun a ->
  arbitrary_uint16 >>= fun b ->
    let open Int32 in
    let hi = shift_left (of_int a) 16 in
    let lo = of_int b in
    ret_gen (logor hi lo)

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

(* arbitrary instance for option type, favoring `Some` rather than `None` *)
let arbitrary_option arb =
  let open Gen in
  frequency [
      (1, ret_gen None);
      (3, arb >>= fun e -> ret_gen (Some e)) ]
