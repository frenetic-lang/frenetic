(** An extension of [Semantics.HEADERS] for testing. *)
module type ARBITRARY_HEADERS = sig
  include Semantics.HEADERS
  val all_headers : header list
  val arbitrary_header : header QuickCheck.arbitrary
  val arbitrary_headerval : value QuickCheck.arbitrary
  val arbitrary_payload : payload QuickCheck.arbitrary  
end

module type S = sig
  type policy
  type packet
  val arbitrary_link : policy QuickCheck.arbitrary
  val arbitrary_lf_policy : policy QuickCheck.arbitrary
  val arbitrary_policy : policy QuickCheck.arbitrary
  val arbitrary_packet : packet QuickCheck.arbitrary
end

module Make
  (Syntax : Semantics.S)
  (Headers : ARBITRARY_HEADERS
     with type header = Syntax.header
      and type value = Syntax.header_val
      and type payload = Syntax.payload) :
    S with type policy = Syntax.policy
       and type packet = Syntax.packet

val arbitrary_link : NetKAT_Types.policy QuickCheck.arbitrary
val arbitrary_lf_pol : NetKAT_Types.policy QuickCheck.arbitrary
val arbitrary_pol : NetKAT_Types.policy QuickCheck.arbitrary
