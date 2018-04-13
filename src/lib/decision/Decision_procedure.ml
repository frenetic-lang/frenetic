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


module rec State : sig
  type t
  val obs : t -> Obs.t
  val trans : t -> Trans.t
end = State
and Obs : sig
  type t
  val equiv : t -> t -> bool
end = Obs
and Trans : sig
  type t
  val succs : t -> t -> (State.t * State.t) list
end = Trans;;


(* type state = { epsilon : obs;
               delta : trans;
             }
and trans =  *)

module type AUTOMATON = sig
  module Fdd : FDD
  (* type state [@@deriving compare, sexp] *)
  type state : State
  type t = {
    start : state;
    states : state list;
    (* epsilon : state -> Fdd.t;
    delta : state -> Fdd.t; *)
  }
end


module Decision(Automaton : AUTOMATON) = struct

  let equiv (x : Automaton.t) (y : Automaton.t) : bool =
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
    let R = [] in 
    let todo = Queue.singleton (x.start y.start) in
    let rec loop (R' : (Automaton.state * Automaton.state) list) : bool = 
      let (s, t) = Queue.dequeue_exn todo in
      if List.mem (s, t) R' then loop R'
      else if not FDD.equiv (x.epsilon s) (y.epsilon t) then false
      else
        FDD.fold (fun i1 i2 -> Queue.enqueue todo (i1, i2))
                 (x.delta s)
                 (y.delta t)
        loop ((s, t)::R')
    in
    loop R


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




