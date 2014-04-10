open Core.Std
open Async.Std

type ('r, 'a, 'b) t = 'r -> 'a -> 'b list Deferred.t

let compose (f : ('r, 'b, 'c) t) (g : ('r, 'a, 'b) t) : ('r, 'a, 'c) t =
  fun t e -> g t e >>= function es ->
    Deferred.List.map ~f:(f t) es >>| List.concat

let (>=>) f g = compose g f
let (<=<) f g = compose f g

let combine (f : ('r, 'a, 'b) t) (g : ('r, 'a, 'b) t) : ('r, 'a, 'b) t =
  fun t e ->
    f t e >>= fun es1 ->
    g t e >>= fun es2 ->
      return (es1 @ es2)

let (<|>) f g = combine f g

let local (l : 'r1 -> 'r2) (f : ('r2, 'a, 'b) t) : ('r1, 'a, 'b) t =
  fun t e -> f (l t) e

let run (f : ('r, 'a, 'b) t) (t : 'r) (r : 'a Pipe.Reader.t) : 'b Pipe.Reader.t =
  Pipe.init (fun w ->
    Pipe.iter r ~f:(fun e ->
      f t e >>| Queue.of_list
            >>= Pipe.write' w))
