module Make :
  functor (Platform : Platform0x04.PLATFORM) ->
    sig
      module Controller :
        sig
          val start_controller :
            (NetCoreFT.policy * NetCoreFT.group_htbl) Lwt_stream.t ->
            unit Lwt.t
        end
      val start : unit -> unit Lwt.t
    end
