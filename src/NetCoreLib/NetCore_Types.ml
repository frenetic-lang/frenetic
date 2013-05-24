open List
open Packet
open Format

module Internal = struct
  type pred =
  | PrHdr of NetCore_Pattern.t
  | PrOnSwitch of OpenFlow0x01.switchId
  | PrOr of pred * pred
  | PrAnd of pred * pred
  | PrNot of pred
  | PrAll
  | PrNone

  type pol =
  | PoAction of NetCore_Action.Output.t
  | PoFilter of pred
  | PoUnion of pol * pol
  | PoSeq of pol * pol
  | PoITE of pred * pol * pol

  type payload = 
  | Buf of OpenFlow0x01.bufferId
  | Data of bytes 

  type value =
  | Pkt of OpenFlow0x01.switchId * NetCore_Pattern.port * packet * payload

  let rec format_pred fmt pred = match pred with 
    | PrHdr pat -> 
      fprintf fmt "@[PrHdr@;<1 2>@[%a@]@]" NetCore_Pattern.to_format pat
    | PrOnSwitch sw ->
      fprintf fmt "@[PrOnSwitch %Lx@]" sw
    | PrOr (p1,p2) ->
      fprintf fmt "@[PrOr@;<1 2>@[(@[%a@],@ @[%a@])@]@]"
        format_pred p1 format_pred p2
    | PrAnd (p1,p2) -> 
      fprintf fmt "@[PrAnd@;<1 2>@[(@[%a@],@ @[%a@])@]@]" 
        format_pred p1 format_pred p2
    | PrNot p -> 
      fprintf fmt "@[PrNot@;<1 2>(@[%a@])@]" format_pred p
    | PrAll -> 
      pp_print_string fmt "PrAll"
    | PrNone -> 
      pp_print_string fmt "PrNone"

  let rec pred_to_string pred = 
    let buf = Buffer.create 100 in
    let fmt = formatter_of_buffer buf in
    pp_set_margin fmt 80;
    format_pred fmt pred;
    fprintf fmt "@?";
    Buffer.contents buf

  let rec format_pol fmt pol = match pol with
    | PoAction a -> 
      fprintf fmt "@[PoAction@;<1 2>@[%s@]@]" (NetCore_Action.Output.to_string a)
    | PoFilter pr -> 
      fprintf fmt "@[PoFilter@;<1 2>(@[%a@])@]" format_pred pr
    | PoUnion (p1,p2) -> 
      fprintf fmt "@[PoUnion@;<1 2>@[(@[%a@],@ @[%a@])@]@]" format_pol p1
        format_pol p2
    | PoSeq (p1,p2) -> 
      fprintf fmt "@[PoSeq@;<1 2>@[(@[%a@],@ @[%a@])@]@]" format_pol p1
        format_pol p2
    | PoITE (pred, then_pol, else_pol) ->
      fprintf fmt "@[PoITE@;<1 2>@[(@[%a@],@ @[%a@],@ @[%a@])@]@]"
        format_pred pred format_pol then_pol format_pol else_pol

  let rec pol_to_string pred = 
    let buf = Buffer.create 100 in
    let fmt = formatter_of_buffer buf in
    pp_set_margin fmt 80;
    format_pol fmt pred;
    fprintf fmt "@?";
    Buffer.contents buf

  let value_to_string = function 
    | Pkt (sid, port, pkt, pay) ->
      Printf.sprintf "(%Ld, %s, %s, _)" 
        sid (NetCore_Pattern.string_of_port port) (packet_to_string pkt)
end

module External = struct
  type get_packet_handler = OpenFlow0x01.switchId -> portId -> packet -> unit

  type predicate =
  | And of predicate * predicate
  | Or of predicate * predicate
  | Not of predicate
  | All
  | NoPackets
  | Switch of OpenFlow0x01.switchId
  | InPort of portId
  | DlSrc of Int64.t
  | DlDst of Int64.t
  | DlVlan of int option (** 12-bits *)
  | SrcIP of Int32.t
  | DstIP of Int32.t
  | TcpSrcPort of int (** 16-bits, implicitly IP *)
  | TcpDstPort of int (** 16-bits, implicitly IP *)

  type action =
  | Pass
  | Drop
  | To of int
  | ToAll
  | UpdateDlSrc of Int64.t * Int64.t
  | UpdateDlDst of Int64.t * Int64.t
  | UpdateDlVlan of int option * int option (** 12-bits *)
  | UpdateSrcIP of Int32.t * Int32.t
  | UpdateDstIP of Int32.t * Int32.t
  | UpdateSrcPort of int * int
  | UpdateDstPort of int * int
  | GetPacket of get_packet_handler
      
  type policy =
  | Empty
  | Act of action
  | Par of policy * policy (** parallel composition *)
  | Seq of policy * policy
  | Filter of predicate
  | Slice of predicate * policy * predicate
  | ITE of predicate * policy * policy

  let par (pols : policy list) : policy = match pols with 
    | x :: xs -> 
      List.fold_right (fun x y -> Par (x, y)) xs x
    | [] -> 
      Empty
      
  let rec predicate_to_string pred = match pred with
    | And (p1,p2) -> 
      Printf.sprintf "(And %s %s)" (predicate_to_string p1) (predicate_to_string p2)
    | Or (p1,p2) -> 
      Printf.sprintf "(Or %s %s)" (predicate_to_string p1) (predicate_to_string p2)
    | Not p1 -> 
      Printf.sprintf "(Not %s)" (predicate_to_string p1)
    | NoPackets -> 
      Printf.sprintf "None"
    | Switch sw -> 
      Printf.sprintf "(Switch %Ld)" sw
    | InPort pt -> 
      Printf.sprintf "(InPort %d)" pt
    | DlSrc add -> 
      Printf.sprintf "(DlSrc %s)" (string_of_mac add)
    | DlDst add -> 
      Printf.sprintf "(DlDst %s)" (string_of_mac add)
    | DlVlan no -> 
      Printf.sprintf "(DlVlan %s)" (match no with None -> "None" | Some n -> "Some " ^ string_of_int n)
    | All -> "All"
    | TcpSrcPort n ->
      Printf.sprintf "(TcpSrcPort %d)" n
    | TcpDstPort n ->
      Printf.sprintf "(TcpDstPort %d)" n
    | SrcIP n ->
      Printf.sprintf "(SrcIP %ld)" n
    | DstIP n ->
      Printf.sprintf "(DstIP %ld)" n
        
  let action_to_string act = match act with
    | Pass -> "Pass"
    | Drop -> "Drop"
    | To pt -> 
      Printf.sprintf "To %d" pt
    | ToAll -> "ToAll"
    | UpdateDlSrc(old,new0) -> 
      Printf.sprintf "UpdateDlSrc(%Ld,%Ld)" old new0
    | UpdateDlDst(old,new0) -> 
      Printf.sprintf "UpdateDlSrc(%Ld,%Ld)" old new0
    | UpdateDlVlan(old,new0) -> 
      Printf.sprintf "UpdateDlSrc (%s,%s)"
        (Packet.dlVlan_to_string old) (Packet.dlVlan_to_string new0)
   | UpdateSrcIP (old, new_) ->
     Printf.sprintf "UpdateSrcIP (%s, %s)"
       (Int32.to_string old) (Int32.to_string new_)
   | UpdateDstIP (old, new_) ->
     Printf.sprintf "UpdateDstIP (%s, %s)"
       (Int32.to_string old) (Int32.to_string new_)
   | UpdateSrcPort (old, new_) ->
     Printf.sprintf "UpdateSrcPort (%s, %s)"
       (string_of_int old) (string_of_int new_)
   | UpdateDstPort (old, new_) ->
     Printf.sprintf "UpdateDstPort (%s, %s)"
       (string_of_int old) (string_of_int new_)
    | GetPacket _ -> 
      Printf.sprintf "GetPacket <fun>"
        
  let rec policy_to_string pol = match pol with 
    | Empty -> 
      "Empty"
    | Act act -> 
      Printf.sprintf "%s" (action_to_string act)
    | Par (p1,p2) -> 
      Printf.sprintf "(%s) U (%s)" 
        (policy_to_string p1) 
        (policy_to_string p2)
    | Seq (p1,p2) -> 
      Printf.sprintf "(%s) >> (%s)" 
        (policy_to_string p1) 
        (policy_to_string p2)
    | Filter pr -> 
      predicate_to_string pr
    | ITE (pred, then_pol, else_pol) ->
      Printf.sprintf "ITE (%s, %s, %s)"
        (predicate_to_string pred)
        (policy_to_string then_pol)
        (policy_to_string else_pol)
    | Slice (ingress,pol',egress) -> 
      Printf.sprintf "{%s} %s {%s}" 
        (predicate_to_string ingress) 
        (policy_to_string pol') 
        (predicate_to_string egress)

  (* JNF: better comment *)
  (* Fail if a policy contains slices and also matches or sets VLANs. *)
  let check_policy_vlans (pol : policy) : unit = 
    let check_act (act : action) = 
      match act with
      | To _ -> 
        (false, false)
      | ToAll -> 
        (false, false)
      | UpdateDlSrc _ -> 
        (false,false)
      | UpdateDlDst _ -> 
        (false,false)
      | UpdateDlVlan _ -> 
        (false,true)
      | GetPacket _ -> 
        (false, false) in 
    let rec check_pred (pred : predicate) =
      match pred with
      | And (pr1, pr2) -> check_pred pr1 || check_pred pr2
      | Or (pr1, pr2) -> check_pred pr1 || check_pred pr2
      | Not pr -> check_pred pr
      | All -> false
      | NoPackets -> false
      | Switch _ -> false
      | InPort _ -> false
      | DlSrc _ -> false
      | DlDst _ -> false
      | DlVlan _ -> true
      | SrcIP _ -> false
      | DstIP _ -> false
      | TcpSrcPort _ -> false
      | TcpDstPort _ -> false in 
    let rec check_pol (pol : policy) = 
      match pol with
      | Act act -> check_act act
      | Par (p1, p2) -> 
        let sliceB, vlanB = check_pol p1 in
        let sliceB', vlanB' = check_pol p2 in
        (sliceB || sliceB', vlanB || vlanB')
      | Seq (p1, p2) ->
        let sliceB, vlanB = check_pol p1 in
        let sliceB', vlanB' = check_pol p2 in
        (sliceB || sliceB', vlanB || vlanB')
      | Filter pred -> (false, check_pred pred)
      | Empty -> (false, false)
      | Slice (ingress, pol', egress) ->
        let vlanB = check_pred ingress in
        let _, vlanB' = check_pol pol' in
        let vlanB'' = check_pred egress in
        (true, vlanB || vlanB' || vlanB'')
    in
    let sliceB, vlanB = check_pol pol in
    if sliceB && vlanB 
    then failwith ("Error: policy contains slices and also matches on or " ^
                      "modifies VLANs.")
    else ()
end
