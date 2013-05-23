type 'a t = {
  now : unit -> 'a;
  (* attach_listener adds a callback that reads new values, and returns
     a thunk that removes the callback. *)
  attach_listener : ('a -> unit) -> (unit -> unit)
}

module Listeners : sig
  type 'a t
  val empty : unit -> 'a t
  val attach : 'a t -> ('a -> unit) -> (unit -> unit)
  val invoke_all : 'a t -> 'a -> unit
end = struct

  module Gensym = NetCore_Gensym

  type 'a t = (Gensym.t * ('a -> unit)) list ref

  let empty () = ref []

  let attach lst listener =
    let key = Gensym.gensym () in
    lst := (key, listener) :: !lst;
    (fun () -> lst := List.remove_assq key !lst)

  let invoke_all lst v = 
    List.iter (fun (_, listener) -> listener v) !lst
end

let map (f : 'a -> 'b) (src : 'a t) : 'b t =
  let now = ref (f (src.now ())) in
  let listeners = Listeners.empty () in
  let updater a =
    now := f a;
    Listeners.invoke_all listeners !now in
  let _ = src.attach_listener updater in
  { 
    now = (fun () -> !now);
    attach_listener = Listeners.attach listeners
  }

let map2 (f : 'a -> 'b -> 'c) (a_src : 'a t) (b_src : 'b t) : 'c t =
  let now = ref (f (a_src.now ()) (b_src.now ())) in
  let listeners = Listeners.empty () in
  let a_updater a =
    now := f a (b_src.now ());
    Listeners.invoke_all listeners !now in
  let b_updater b =
    now := f (a_src.now ()) b;
    Listeners.invoke_all listeners !now in
  let _ = a_src.attach_listener a_updater in
  let _ = b_src.attach_listener b_updater in
  {
    now = (fun () -> !now);
    attach_listener = Listeners.attach listeners
  }

let constant (x : 'a) = {
  now = (fun () -> x);
  attach_listener = (fun _ -> fun () -> ())
}


let from_stream (init : 'a) (stream : 'a Lwt_stream.t) : 'a t =
  let now = ref init in
  let listeners = Listeners.empty () in
  Lwt.async
    (fun () -> 
      Lwt_stream.iter
        (fun a ->
          now := a;
          Listeners.invoke_all listeners !now)
        stream);
  {
    now = (fun () -> !now); 
    attach_listener = Listeners.attach listeners
  }

let to_stream (x : 'a t) : 'a Lwt_stream.t =
  let (stream, push) = Lwt_stream.create () in
  let _ = x.attach_listener (fun a -> push (Some a)) in
  push (Some (x.now ()));
  stream
