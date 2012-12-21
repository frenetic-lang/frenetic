(* CONCURRENT ML WTF *)
open Event
open Printf

let chan : string channel = new_channel ()

let consumer () : unit = 
  printf "Got: %s\n%!" (sync (receive chan));
  printf "Got: %s\n%!" (sync (receive chan))

let main () : unit = 
  let t = Thread.create consumer () in
  let evt1 = wrap (send chan "first") (fun () -> "first sent") in
  let evt2 = wrap (send chan "second") (fun () -> "second sent") in
  (* WTF IS THIS. *)
  printf "main: %s\n%!" (sync evt2);
  printf "main: %s\n%!" (sync evt1);
  Thread.join t;;

main ()
  
  

