open NetCore_Types

val make : switchId -> portId -> portId -> (unit Lwt.t * pol NetCore_Stream.t)
