type v = Node.t
type t = {
  srcport : Node.portId;
  dstport : Node.portId;
  cost : int64;
  capacity : int64;
}
type e = v * t * v
let compare = Pervasives.compare
let default = {
  srcport = 0L;
  dstport = 0L;
  cost = 1L;
  capacity = Int64.max_int
}

  (* Constructors and mutators *)
let mk_edge s d l = (s,l,d)

let mk_link s sp d dp cap cost =
  ( s,
    { srcport = sp;
      dstport = dp;
      cost = cost;
      capacity = cap;},
    d)

let reverse (s,d,l) =
  ( d, s,
    { srcport = l.dstport;
	  dstport = l.srcport;
      cost = l.cost;
	  capacity = l.capacity }
  )

  (* Accessors *)
let src (s,l,d) = s
let dst (s,l,d) = d
let label (s,l,d) = l

let capacity (s,l,d) = l.capacity
let cost (s,l,d)     = l.cost
let srcport (s,l,d)  = l.srcport
let dstport (s,l,d)  = l.dstport

let reverse (s,l,d) =
  ( d,
    { srcport = l.dstport; dstport = l.srcport;
      cost = l.cost; capacity = l.capacity },
    s
  )

let name (s,_,d) tbl =
  Printf.sprintf "%s_%s" (Node.to_string s tbl) (Node.to_string d tbl)

let string_of_label (s,l,d) =
  Printf.sprintf "{srcport = %s; dstport = %s; cost = %s; capacity = %s;}"
    (Int64.to_string l.srcport)
    (Int64.to_string l.dstport)
    (Int64.to_string l.cost)
    (Int64.to_string l.capacity)

let to_dot (s,l,d) tbl =
  let s = Node.to_dot s tbl in
  let d = Node.to_dot d tbl in
  Printf.sprintf "%s -> %s [label=\"%s\"]" s d (string_of_label (s,l,d))

let to_string = to_dot
