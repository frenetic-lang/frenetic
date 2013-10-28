(** NetKAT Syntax *)

(** {2 Basics} *)

type header = 
  | Header of SDN_Types.field
  | Switch

type header_val = VInt.t

type payload = SDN_Types.payload

(** {2 Policies} *)
  
type pred = 
  | True
  | False
  | Test of header * header_val
  | And of pred * pred
  | Or of pred * pred
  | Neg of pred
      
type policy =
  | Filter of pred
  | Mod of header * header_val
  | Par of policy * policy
  | Choice of policy * policy
  | Seq of policy * policy
  | Star of policy
  | Link of header_val * header_val * header_val * header_val
      
let id = Filter True
  
let drop = Filter False

(** {2 Packets} 

  If we only defined the semantics and were not building a system, a
  packet would only be a record of headers. However, the runtime needs to
  apply [eval] to packets contained in [PACKET_IN] mesages. For the runtime,
  packets also carry a payload that is unmodified by [eval]. *)

(** A map keyed by header names. *)
module HeaderMap = Map.Make (struct
  type t = header
  let compare = Pervasives.compare
end)
  
type header_val_map = header_val HeaderMap.t
    
(** Evaluating the policy [Test (h, v)] looks for [h] in these headers. If
    they are not found, we signal an error. An OpenFlow switch will never
    look for a header that does not exist. So, it is safe to assume that
    unused headers are set to zero or some other default value. *)
type packet = {
  headers : header_val_map;
  payload : payload
}

module PacketSet = Set.Make (struct
  type t = packet
      
  (* First compare by headers, then payload. The payload comparison is a
     little questionable. However, this is safe to use in eval, since
     all output packets have the same payload as the input packet. *)
  let compare x y =
    let cmp = HeaderMap.compare Pervasives.compare x.headers y.headers in
    if cmp <> 0 then
      cmp
    else
      Pervasives.compare x.payload y.payload
end)
  
module PacketSetSet = Set.Make(PacketSet)

