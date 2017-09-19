open QuickCheck

val arbitrary_bytes : int -> Cstruct.t arbitrary

val arbitrary_bufferId : Frenetic_OpenFlow.bufferId arbitrary

val arbitrary_payload : Frenetic_OpenFlow.payload arbitrary

val arbitrary_ip_mask : Frenetic_OpenFlow.Pattern.Ip.t arbitrary

val arbitrary_pattern : Frenetic_OpenFlow.Pattern.t arbitrary
