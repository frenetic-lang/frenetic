open Core.Std
open Async.Std

type ('t, 'a, 'b) stage = 't -> 'a -> 'b list Deferred.t

let compose (f : ('t, 'b, 'c) stage) (g : ('t, 'a, 'b) stage) : ('t, 'a, 'c) stage =
  fun t e -> g t e >>= function es ->
    Deferred.List.map ~f:(f t) es >>| List.concat

let (>=>) f g = compose g f
let (<=<) f g = compose f g

let combine (f : ('t, 'a, 'b) stage) (g : ('t, 'a, 'b) stage) : ('t, 'a, 'b) stage =
  fun t e ->
    f t e >>= fun es1 ->
    g t e >>= fun es2 ->
      return (es1 @ es2)

let (<|>) f g = combine f g

let local (l : 't1 -> 't2) (f : ('t2, 'a, 'b) stage) : ('t1, 'a, 'b) stage =
  fun t e -> f (l t) e

let run (f : ('t, 'a, 'b) stage) (t : 't) (r : 'a Pipe.Reader.t) : 'b Pipe.Reader.t =
  Pipe.init (fun w ->
    Pipe.iter r ~f:(fun e ->
      f t e >>| Queue.of_list
            >>= Pipe.write' w))
