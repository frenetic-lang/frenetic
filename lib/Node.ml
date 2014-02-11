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

type node_record = {
  mutable hash : int option ;
  mutable visited : bool ;
  id : int
}

type t = node_record

type label = t


let create i = { hash = Some (Hashtbl.hash i) ; visited = false; id = i}

let compare = Pervasives.compare

let hash (n:t) : int = match n with
  | { hash = Some h; _ } -> h
  | { hash = None; visited = _; id = i } ->
    let h = Hashtbl.hash i in
    n.hash <- Some h;
    h

let equal (n1:t) (n2:t) = n1.id = n2.id

module NodeHash = Hashtbl.Make(struct
  type t = node_record
  let hash = hash
  let equal = equal
end)

type attr_tbl = attributes NodeHash.t

let to_dot n idtbl = let attr = NodeHash.find idtbl n in attr.name

let to_string = to_dot

let id_of_switch n idtbl = let attr = NodeHash.find idtbl n in attr.dev_id

let visited n = n.visited

let visit n = n.visited <- true

let leave n = n.visited <- false
