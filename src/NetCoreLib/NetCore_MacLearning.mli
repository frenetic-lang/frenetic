open NetCore_Types.Internal

module Learning : sig
  val init : pol
  val policy : pol Lwt_stream.t
end 

module Routing : sig
  val policy : pol Lwt_stream.t
end


