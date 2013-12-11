module Log =
  struct
    let log = ref stderr

    let get_log_chan () : out_channel = !log

    let set_log_file (replace : bool) (filename : string) : unit =
      let chan = 
	match replace with
	  | true -> open_out filename 
	  | false -> 
            open_out_gen [Open_wronly; Open_text; Open_creat; Open_excl] 
              0o600 filename in
      log := chan;
      at_exit (fun () -> close_out chan)

    let printf (fmt : ('a, out_channel, unit) format) : 'a =
      Printf.printf ".%!";
      Printf.fprintf !log fmt
  end

module Lwt_channel =
  struct
    type 'a t = {
      stream : 'a Lwt_stream.t;
      push : 'a option -> unit
    }

    let of_pushed_stream stream push = { stream; push }

    let create () = 
      let (stream, push) = Lwt_stream.create () in
      of_pushed_stream stream push

    let send (v : 'a) (chan : 'a t) = Lwt.return (chan.push (Some v))

    let recv (chan : 'a t) = Lwt_stream.next chan.stream

    let to_stream (chan : 'a t) = chan.stream

  end

let test_bit n x = 
  Int32.logand (Int32.shift_right_logical x n) Int32.one = Int32.one
let clear_bit n x =
  Int32.logand x (Int32.lognot (Int32.shift_left Int32.one n))
let set_bit n x = 
  Int32.logor x (Int32.shift_left Int32.one n)
let bit (x : int32) (n : int) (v : bool) : int32 = 
  if v then set_bit n x else clear_bit n x

module type SAFESOCKET = sig
  type t = Lwt_unix.file_descr
  val create : Lwt_unix.file_descr -> t
  val recv : t -> string -> int -> int -> bool Lwt.t
end

module SafeSocket : SAFESOCKET = struct
  open Lwt
  open Lwt_unix

  type t = Lwt_unix.file_descr

  let create fd = fd

  let rec recv fd buf off len = 
    if len = 0 then 
      return true
    else 
      lwt n = Lwt_unix.recv fd buf off len [] in  
      if n = 0 then 
	return false
      else if n = len then 
	return true
      else
	recv fd buf (off + n) (len - n)
end
