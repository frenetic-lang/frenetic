open Core
open Frenetic_netkat


(* module type Queue = sig
  type 'a queue
  val empty : 'a queue
  val is_empty : 'a queue -> bool
  val push : 'a -> 'a queue -> 'a queue
  val peek : 'a queue -> 'a option
  val pop : 'a queue -> 'a queue option
end *)

(* module type FDD : sig
  type t
  val equiv : t -> t -> t
  val union : t -> t -> t
  val fold : ('a -> 'a -> unit) -> t -> t -> unit
  val succs: t -> t -> (state * state) list
end *)


module type Params = sig
  type state [@@deriving eq]
  type obs
  type trans

  val obs : state -> obs
  val trans : state -> trans
  val obs_equiv : obs -> obs -> bool
  val succs : trans -> trans -> (state * state) list
end


module Decision(Params : Params) = struct
  open Params

  let equiv (x : state) (y : state) : bool =
    (*
     * Basic idea: keep a worklist of all paris of states i.e. Q1 U Q2 X Q1 U Q2
     * (1) Put (s1, s2) into the worklist and initialize emtpy relation R
     * (2) LOOP: while worklist nonemtpy
     *     (3) Pop (s,t)
     *     (4) if [s is final state] =! [t is final state] then false
     *     (5) otherwise if (s,t) are already in R then GOTO 2
     *     (6) do some merge of states to make (s,t) in R (want a\equiv b)
     *     (7) for all letters a push (d(s,a),d(t,a)) onto worklist
     *)
    let rel = [] in
    let todo = Queue.singleton (x, y) in
    let rec loop (rel : (state * state) list) : bool =
      let x, y = Queue.dequeue_exn todo in
      if List.mem rel (x, y) ~equal:[%eq: state * state] then
        loop rel
      else if not (obs_equiv (obs x) (obs y)) then
        false
      else begin
        List.iter (succs (trans x) (trans y)) ~f:(Queue.enqueue todo);
        loop rel
      end
    in
    loop rel


(*===========================================================================*)
(* Worry about this later                                                    *)
(*===========================================================================*)

  (* TODO *)
  module Packet = struct
    (* maps fields to values (partial?) *)
    type t
  end

  type result = [`True | `False of Packet.t list ]




end




