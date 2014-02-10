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

let default = {
  node_type = Host
  ; name = ""
  ; ip = 0l
  ; mac = 0L
  ; dev_id = 0L
}
