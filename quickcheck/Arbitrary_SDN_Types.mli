open QuickCheck

val arbitrary_bytes : int -> Cstruct.t arbitrary

val arbitrary_bufferId : SDN_Types.bufferId arbitrary

val arbitrary_payload : SDN_Types.payload arbitrary

val arbitrary_pattern : SDN_Types.Pattern.t arbitrary
