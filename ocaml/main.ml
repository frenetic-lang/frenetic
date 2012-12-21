open Printf
open Word
open Openflow1_0

open Unix

let switch_thread (uid : int) (t : float) : unit =
  let rec loop () = 
    printf "%d unpaused ...\n%!" uid;
    Thread.delay t;
    loop ()
  in loop ()


let main () = 
  Thread.create (switch_thread 10) 1.0;
  let x =  Thread.create (switch_thread 20) 2.0 in
  Thread.join x;
  printf "Hello, world"

let _ = main ()
