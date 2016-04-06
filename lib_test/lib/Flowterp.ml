open Core.Std

module HVSet = Set.Make(struct
  open Frenetic_NetKAT_Semantics

  type t = HeadersValues.t sexp_opaque [@@deriving sexp]
  let compare = HeadersValues.compare
end)

module Headers = struct
  open Frenetic_NetKAT
  open Frenetic_NetKAT_Semantics
  open Frenetic_OpenFlow

  let eval_pattern (hdrs : HeadersValues.t) (pat : Pattern.t) : bool =
    let matches p f =
      match p with
        | None -> true
        | Some(p_v) -> Field.get f hdrs = p_v
    in
    let matches_mask (p:Pattern.Ip.t option) f =
      match p with
        | None -> true
        | Some (x,m) ->
          Pattern.Ip.less_eq (Field.get f hdrs, 32l) (x, m)
    in
    let open Pattern in
    HeadersValues.Fields.for_all
      ~location:(fun f ->
        match Field.get f hdrs, pat.inPort with
         | _, None -> true
         | Physical p1, Some p2 -> p1 = p2
         | _          , Some _  -> false)
      ~ethSrc:(matches pat.dlSrc)
      ~ethDst:(matches pat.dlDst)
      ~vlan:(matches pat.dlVlan)
      ~vlanPcp:(matches pat.dlVlanPcp)
      ~ethType:(matches pat.dlTyp)
      ~ipProto:(matches pat.nwProto)
      ~ipSrc:(matches_mask pat.nwSrc)
      ~ipDst:(matches_mask pat.nwDst)
      ~tcpSrcPort:(matches pat.tpSrc)
      ~tcpDstPort:(matches pat.tpDst)

  let eval_action port (hdrs : HeadersValues.t) (action : action) =
    let open HeadersValues in
    match action with
      | Output (Physical p) ->  { hdrs with location = Physical p }
      | Output (Controller _) -> { hdrs with location = Pipe "controller" }
      | Output InPort -> { hdrs with location = Physical(port) }
      | Output _ -> assert false
      | Enqueue _ -> assert false
      | FastFail _ -> assert false
      | Modify m ->
        begin match m with
          | SetEthSrc dlSrc -> { hdrs with ethSrc = dlSrc }
          | SetEthDst dlDst -> { hdrs with ethDst = dlDst }
          | SetVlan  dlVlan ->
            begin match dlVlan with
             | None -> assert false
             | Some(vlan_id) -> { hdrs with vlan = vlan_id }
            end
          | SetVlanPcp dlVlanPcp -> { hdrs with vlanPcp = dlVlanPcp }
          | SetEthTyp dlTyp -> { hdrs with ethType = dlTyp }
          | SetIPProto nwProto -> { hdrs with ipProto = nwProto }
          | SetIP4Src nwSrc -> { hdrs with ipSrc = nwSrc }
          | SetIP4Dst nwDst -> { hdrs with ipDst = nwDst }
          | SetTCPSrcPort tpSrc -> { hdrs with tcpSrcPort = tpSrc }
          | SetTCPDstPort tpDst -> { hdrs with tcpDstPort = tpDst }
        end

  let eval_par port (hdrs : HeadersValues.t) (par : par) : HVSet.t =
    let open Core.Std in
    List.fold par ~init:HVSet.empty ~f:(fun acc actions ->
      HVSet.add acc (List.fold actions ~init:hdrs ~f:(eval_action port)))

  let eval_flow port (hdrs : HeadersValues.t) (flow : flow) : HVSet.t option =
    if eval_pattern hdrs flow.pattern then
      let par = match flow.action with
        | []    -> []
        | [par] -> par
        | _     -> assert false in (* Do not handle groups *)
      Some(eval_par port hdrs par)
    else
      None

  let eval (hdrs : HeadersValues.t) (table : flowTable) : HVSet.t =
    let port = match hdrs.HeadersValues.location with
      | Physical(p) -> p
      | _ -> failwith "not at physical port" in
    let rec loop t =
      match t with
      | []     -> HVSet.empty
      | (f::t') ->
        begin match eval_flow port hdrs f with
          | None          -> loop t'
          | Some(pkt_set) -> pkt_set
        end in
    loop table
end

module Packet = struct
  module PacketSet = Frenetic_NetKAT_Semantics.PacketSet

  let of_hv_set pkt hv_set : PacketSet.t =
    HVSet.fold hv_set ~init:PacketSet.empty ~f:(fun acc hdrs ->
      PacketSet.add acc { pkt with Frenetic_NetKAT_Semantics.headers = hdrs })

  let eval_flow
      (port : Frenetic_NetKAT.portId)
      (pkt : Frenetic_NetKAT_Semantics.packet)
      (flow : Frenetic_OpenFlow.flow)
    : PacketSet.t option =
    let open Core.Std in
    Option.map (Headers.eval_flow port pkt.Frenetic_NetKAT_Semantics.headers flow) (of_hv_set pkt)

  let eval
      (pkt : Frenetic_NetKAT_Semantics.packet)
      (table : Frenetic_OpenFlow.flowTable)
    : PacketSet.t =
    of_hv_set pkt (Headers.eval pkt.Frenetic_NetKAT_Semantics.headers table)
end
