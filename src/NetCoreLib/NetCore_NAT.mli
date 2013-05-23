open NetCore_Types.External

(** [new_nat public_ip] returns two related policy streams. The first
   is for outgoing traffic and the second is for incoming traffic. *)
val make : Packet.nwAddr -> (policy * policy) NetCore_Stream.t

