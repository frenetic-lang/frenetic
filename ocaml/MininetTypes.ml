type switchId = Int64.t
type hostAddr = Int64.t
type portId = int

type node =
  | Host of hostAddr
  | Switch of switchId
