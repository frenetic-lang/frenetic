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
