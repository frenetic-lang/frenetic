(** High-level interface to an OpenFlow 1.0 switch. *)
include SDN_Types.SWITCH

val from_handle : OpenFlow0x01_Switch.t -> t
