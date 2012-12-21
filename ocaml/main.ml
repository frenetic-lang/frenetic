open Printf
open Word
open Openflow1_0
open Platform 
open Unix

module ServerFd : Platform.FD = struct
  let fd = socket PF_INET SOCK_STREAM 0
    
  let _ = 
    bind fd (ADDR_INET (inet_addr_any, 6633));
    listen fd 10;
    at_exit (fun () -> close fd)
end


module Test1 = struct
  module Platform = TestPlatform
  module Controller = DummyController.Make (Platform)

  open Platform

  let test_script () = 
    connect_switch (Word64.from_int64 100L);
    connect_switch (Word64.from_int64 200L)
    

  let t1 = Thread.create test_script ()
  let t2 = Thread.create Controller.start ()

  let _ = 
    Thread.join t1;
    Thread.join t2


end

    


