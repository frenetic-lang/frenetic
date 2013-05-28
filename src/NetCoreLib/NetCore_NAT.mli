open NetCore_Types.Internal

(** [new_nat public_ip] returns two related policy streams. The first
   is for outgoing traffic and the second is for incoming traffic. *)
val make : Packet.nwAddr ->
  unit Lwt.t * pol NetCore_Stream.t * pol NetCore_Stream.t

