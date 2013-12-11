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
