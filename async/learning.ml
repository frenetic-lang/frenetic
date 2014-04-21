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

  let forward switch_id packet : action =
    let ethDst = packet.Packet.dlDst in
    let mac_map = SwitchMap.find_exn !state switch_id in
    let open SDN_Types in
    match MacMap.find mac_map ethDst with
      | None ->
        Log.of_lazy ~tags ~level:`Info (lazy (Printf.sprintf
          "[learning] switch %Lu: flood %s"
              switch_id (Packet.to_string packet)));
        Output(All)
      | Some(p) ->
        Log.of_lazy ~tags ~level:`Info (lazy (Printf.sprintf
          "[learning] switch %Lu: port %lu %s"
              switch_id p (Packet.to_string packet)));
        Output(Physical p) in

  let default = Mod(Location(Pipe "learn")) in

  let gen_pol () =
    let drop = Filter False in
    SwitchMap.fold !state ~init:drop ~f:(fun ~key:switch_id ~data:mac_map acc ->
      let known, unknown_pred = MacMap.fold mac_map ~init:(drop, True)
        ~f:(fun ~key:mac ~data:port (k, u) ->
          let k' = Union(Seq(Filter(Test(EthDst mac)),
                             Mod(Location(Physical port))),
                         k) in
          let u' = And(Neg(Test(EthDst mac)), u) in
          (k', u')) in
      Union(Seq(Filter(Test(Switch switch_id)),
                Union(known, Seq(Filter(unknown_pred), default))),
            acc)) in

  let handler t w () e = match e with
    | SwitchUp(switch_id) ->
      state := SwitchMap.add !state switch_id MacMap.empty;
      return (Some(gen_pol ()))
    | SwitchDown(switch_id) ->
      state := SwitchMap.remove !state switch_id;
      return (Some(gen_pol ()))
    | PacketIn(_, switch_id, port_id, bytes, _, buf) ->
      let packet = Packet.parse bytes in
      let pol = if learn switch_id port_id packet then
         Some(gen_pol ())
      else 
         None in
      let action = forward switch_id packet in
      Pipe.write w (switch_id, bytes, buf, Some(port_id), [action]) >>= fun _ ->
      return pol 
    | _ -> return None in
      
  create ~pipes:(PipeSet.singleton "learn") default handler
