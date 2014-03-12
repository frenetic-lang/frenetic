open Core.Std
open Async.Std

module OFAsync = Async_OpenFlow
module Log = OFAsync.Log

module OFTcp = OFAsync.ClientServer.Make (OFAsync.Chunk.Message)

type chunk = OFAsync.Chunk.Message.t

let _ = Log.set_level `Debug

let _ = Log.set_output 
  [Log.make_filtered_output [("openflow", "socket");
                                     ("openflow", "serialization")]]

let openflow_parser = OFAsync.OpenFlow0x01.chunk_conv

let virtualize_switch (controller_addr : 'a Tcp.where_to_connect)
  (from_switch : chunk Pipe.Reader.t)
  (to_switch : chunk Pipe.Writer.t) : unit Deferred.t =
  Log.info "switch connected";
  try_with ~name:"internal" (fun () ->
    OFTcp.connect controller_addr (fun sock from_ctrl to_ctrl ->
      let (openflow_from_switch, openflow_to_switch) = openflow_parser (from_switch, to_switch) in
      let (openflow_from_ctrl, openflow_to_ctrl) = openflow_parser (from_ctrl, to_ctrl) in
      let closed1 = Pipe.transfer_id openflow_from_switch openflow_to_ctrl in
      let closed2 = Pipe.transfer_id openflow_from_ctrl openflow_to_switch in
      let cleanup () = 
        Pipe.close to_switch; Pipe.close to_ctrl in
      Deferred.any [closed1; closed2 ]
      >>= fun () ->
      cleanup ();
      return ()))
  >>| function
  | Ok () ->  
    Pipe.close to_switch
  | Error exn -> 
    Log.error "exception %s" (Exn.to_string exn);
    Pipe.close to_switch; ()

let hypervisor (controller : 'a Tcp.where_to_connect)
               (listen_on : ('b, 'c) Tcp.Where_to_listen.t) =
    OFTcp.listen listen_on (virtualize_switch controller)
  
let _ = match Array.to_list Sys.argv with
  | [ _; "local"; actual_controller_port ] ->
    let _ = 
      try_with ~name:"external" (fun () ->
        hypervisor 
        (Tcp.to_host_and_port "localhost" (int_of_string actual_controller_port))
        (Tcp.on_port 6633))
      >>= function
      | Ok x -> Tcp.Server.close_finished x 
                >>= fun () ->
                Log.error "stopped listening";
                return ()
      | Error exn ->
        (Log.error "bad exception %s" (Exn.to_string exn); raise exn) in
    never_returns (Scheduler.go ())
  | _ -> 
    printf "Invalid command line arguments.\n%!"
