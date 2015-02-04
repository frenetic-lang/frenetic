open Core.Std
open Async.Std


module Log = Async_OpenFlow.Log
let tags = [("netkat", "learning")]

module SwitchMap = Map.Make(Int64)
module MacMap = Map.Make(Int64)

let create () =
  let open Async_NetKAT in
  let open NetKAT_Types in
  let state = ref SwitchMap.empty in

  let learn switch_id port_id packet =
    let ethSrc = packet.Packet.dlSrc in
    let mac_map = SwitchMap.find_exn !state switch_id in
    if MacMap.mem mac_map ethSrc then
      false
    else begin
      Log.info ~tags "[learning] switch %Lu: learn %s => %Lu@%lu"
        switch_id (Packet.string_of_mac ethSrc) switch_id port_id;
      state := SwitchMap.add !state switch_id (MacMap.add mac_map ethSrc port_id);
      true
    end in

  let open Optimize in

  let default = Mod(Location(Pipe "learn")) in
  let drop = Filter False in

  let all_ports ps = Net.Topology.PortSet.fold ps ~init:drop ~f:(fun acc p ->
    mk_union (Mod(Location(Physical p))) acc)
  in

  let gen_pol nib =
    SwitchMap.fold !state ~init:drop ~f:(fun ~key:switch_id ~data:mac_map acc ->
      let known_pred, forward, unknown_pred = ref False, ref drop, ref True in
      MacMap.iter mac_map ~f:(fun ~key:mac ~data:port ->
        known_pred := mk_or (Test(EthDst mac)) !known_pred;
        unknown_pred := mk_and (Neg(Test(EthSrc mac))) !unknown_pred;
        forward := mk_union (Seq(Filter(Test(EthDst mac)),
                                Mod(Location(Physical port))))
                            !forward
      );
      let broadcast =
        let open Net.Topology in
        let v  = vertex_of_label nib (Async_NetKAT.Switch switch_id) in
        let ps = vertex_to_ports nib v in
        let ports = PortSet.fold ps ~init:drop ~f:(fun acc p ->
            Union(Seq(Filter(Test(Location(Physical p))),
                      all_ports (PortSet.remove ps p)),
                  acc))
        in
        Seq(Filter(Or(Test(EthDst 0xffffffffffffL), Neg(!known_pred))), ports)
      in
      Union(Seq(Filter(Test(Switch switch_id)),
                Union(!forward,
                Union(broadcast,
                      Seq(Filter(!unknown_pred), default)))),
            acc))
  in

  let handler t w () e = match e with
    | SwitchUp(switch_id) ->
      state := SwitchMap.add !state switch_id MacMap.empty;
      return (Some(gen_pol !t))
    | SwitchDown(switch_id) ->
      state := SwitchMap.remove !state switch_id;
      return (Some(gen_pol !t))
    | PortUp(switch_id, port_id)
    | PortDown(switch_id, port_id) ->
      return (Some(gen_pol !t))
    | PacketIn(_, switch_id, port_id, payload, _) ->
      let packet = Packet.parse (SDN_Types.payload_bytes payload) in
      let pol = if learn switch_id port_id packet then
         Some(gen_pol !t)
      else
         None in
      return pol
    | _ -> return None in
      
  Policy.create ~pipes:(PipeSet.singleton "learn") default handler
