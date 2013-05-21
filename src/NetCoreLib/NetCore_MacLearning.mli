module Learning : sig
  val init : NetCore_Types.External.policy
  val policy : NetCore_Types.External.policy Lwt_stream.t
end 

module Routing : sig
  val policy : NetCore_Types.External.policy Lwt_stream.t
end


