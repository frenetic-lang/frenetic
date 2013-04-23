open Packet
open OpenFlowTypes
open WordInterface

open Platform0x04
open NetCoreFT

module Learning : sig
  
  val learned_hosts : (switchId * Word48.t, portId) Hashtbl.t

  val policy : policy Lwt_stream.t

end 

module Routing : sig

  val policy : (policy*policy*policy) Lwt_stream.t
    
end

module Make : functor (Platform : PLATFORM) -> sig
    
  val start : unit -> unit Lwt.t
    
end

