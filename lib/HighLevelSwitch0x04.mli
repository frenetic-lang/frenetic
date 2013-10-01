(** High-level interface to an OpenFlow 1.3 switch. *)
include SDN_Types.SWITCH

val from_handle : OpenFlow0x04_Switch.t -> t
