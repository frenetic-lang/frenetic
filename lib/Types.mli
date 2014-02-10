open Packet

type switchId = int64
type portId = int64

type node = Switch | Host | Middlebox

type attributes = {
  node_type : node
  ; name : string
  ; ip : nwAddr
  ; mac : dlAddr
  ; dev_id : int64
}

val default : attributes
