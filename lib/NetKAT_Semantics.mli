open NetKAT_Types
    
(** {2 Semantics}
 
  [eval pkt pol] raises [Not_found] if it tests or updates a header that  [pkt]
  does not have. This behavior is different from OpenFlow, which fails  silently
  in both cases. *)
  
val eval : packet -> policy -> PacketSet.t
val eval_pipes :  packet
               -> policy
               -> (string * NetKAT_Types.packet) list * NetKAT_Types.packet list

val size: policy -> int
