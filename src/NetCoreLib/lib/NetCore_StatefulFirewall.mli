open NetCore_Types
open OpenFlow0x01

val make : switchId -> portId -> portId -> (unit Lwt.t * pol NetCore_Stream.t)
