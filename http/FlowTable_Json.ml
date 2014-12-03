open Async.Std
open SDN_Types

let pattern_to_json (p:Pattern.t) : Yojson.Safe.json = 
  let open Pattern in 
  let i = function 
    | None -> 
       `Null 
    | Some (a,m) -> 
       `Tuple [`Intlit(Packet.string_of_ip a);
               `Intlit(Int32.to_string m)] in 
  let o f = function None -> `Null | Some x -> `Intlit(f x) in 
  `Assoc [
     ("dlSrc", o Packet.string_of_mac p.dlSrc);
     ("dlDst", o Packet.string_of_mac p.dlDst);
     ("dlTyp", o string_of_int p.dlTyp);
     ("dlVlan", o string_of_int p.dlVlan);
     ("dlVlanPcp", o string_of_int p.dlVlanPcp);
     ("nwSrc", i p.nwSrc);
     ("nwDst", i p.nwDst);
     ("nwProto", o string_of_int p.nwProto);
     ("tpSrc", o string_of_int p.tpSrc);
     ("tpDst", o string_of_int p.tpDst);
     ("inPort", o Int32.to_string p.inPort) ]

let pseudoport_to_json (p:pseudoport) : Yojson.Safe.json = 
  match p with
  | Physical p -> 
     `List[`String "Physical"; `Intlit (Int32.to_string p)]
  | InPort -> 
     `String "InPort"
  | Table -> 
     `String "Table"
  | Normal -> 
     `String "Normal"
  | Flood -> 
     `String "Flood"
  | All -> 
     `String "All"
  | Controller(n) -> 
     `List[`String "Controller"; `Intlit (string_of_int n)]
  | Local -> 
     `String "Local"

let modify_to_json (m:modify) : Yojson.Safe.json = 
  match m with 
  | SetEthSrc m -> 
     `List [`String "SetDlSrc"; `Intlit (Packet.string_of_mac m)]
  | SetEthDst m ->
     `List [`String "SetDlDst"; `Intlit (Packet.string_of_mac m)]
  | SetVlan o -> 
     `List [`String "SetVlan"; `Intlit (match o with None -> "0xffff" | Some n -> string_of_int n)]
  | SetVlanPcp n -> 
     `List [`String "SetVlanPcp"; `Intlit (string_of_int n)]
  | SetEthTyp n -> 
     `List [`String "SetDlTyp"; `Intlit (string_of_int n)]
  | SetIPProto n -> 
     `List [`String "SetNwProto"; `Intlit (string_of_int n)]
  | SetIP4Src n -> 
     `List [`String "SetNwSrc"; `Intlit (Packet.string_of_ip n)]
  | SetIP4Dst n -> 
     `List [`String "SetNwDst"; `Intlit (Packet.string_of_ip n)]
  | SetTCPSrcPort n -> 
     `List [`String "SetTpSrc"; `Intlit (string_of_int n)]
  | SetTCPDstPort n -> 
     `List [`String "SetTpDst"; `Intlit (string_of_int n)]

let action_to_json (a:action) : Yojson.Safe.json = 
  match a with 
  | Output p -> 
     `List [`String "Output"; pseudoport_to_json p]
  | Enqueue (p,q) -> 
     `List [`String "Enqueue"; `Intlit (Int32.to_string p); `Intlit (Int32.to_string q)]
  | Modify m -> 
     `List [`String "Modify"; modify_to_json m]

let seq_to_json (s:seq) : Yojson.Safe.json = 
  `List (List.map action_to_json s)

let par_to_json (p:par) : Yojson.Safe.json = 
  `List (List.map seq_to_json p)

let action_to_json (g:group) : Yojson.Safe.json = 
  match g with 
  | [p] -> par_to_json p

let timeout_to_json (t:timeout) : Yojson.Safe.json = 
  match t with
  | Permanent -> 
     `String "Permanent"
  | ExpiresAfter n -> 
     `List [`String "ExpiresAfter"; `Intlit (string_of_int n)]

let flow_to_json (n:int) (f:flow) : Yojson.Safe.json = 
  `Assoc [
     ("priority", `Intlit (string_of_int n));
     ("pattern", pattern_to_json f.pattern);
     ("action", action_to_json f.action);
     ("cookie", `Intlit (Int64.to_string f.cookie));
     ("idle_timeout", timeout_to_json f.idle_timeout);
     ("hard_timeout", timeout_to_json f.hard_timeout)
   ]

let to_json (t:flowTable) : Yojson.Safe.json = 
  let l,_ = 
    List.fold_right
      (fun f (acc,i) -> 
         (flow_to_json i f::acc, i - 1))
      t
      ([],65535) in 
  `List l
      
