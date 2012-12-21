open Printf
open Word
open Openflow1_0

open Unix

let switch_thread (uid : int) (t : float) : unit =
  let rec loop () = 
    printf "%d unpaused ...\n" uid;
    Thread.delay t;
    loop ()
  in loop ()


let main () = 
  t1 <- Thread.create 
  printf "Hello, world"

let _ = main ()
