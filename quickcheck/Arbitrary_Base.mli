open QuickCheck

(* arbitrary instance for usigned integers. Still uses the `int` type. *)
val arbitrary_uint : int arbitrary

(* arbitrary instance for unsigned int8, using the `int` type. *)
val arbitrary_uint8 : int arbitrary

(* arbitrary instance for unsigned int16, using the `int` type. *)
val arbitrary_uint16 : int arbitrary

(* arbitrary instance for unsigned int32, using the `int32` type. *)
val arbitrary_uint32 : int32 arbitrary

(* arbitrary instance for unsigned int48, using the `int64` type. *)
val arbitrary_uint48 : int64 arbitrary

(* arbitrary instance for unsigned int64, using the `int64` type. *)
val arbitrary_uint64 : int64 arbitrary

(* arbitrary instance for option type, favoring `Some` rather than `None` *)
val arbitrary_option : 'a arbitrary -> 'a option arbitrary
