open Core
open Frenetic_netkat



module type Params = sig
  type state [@@deriving hash, compare, sexp]
  type obs
  type trans

  val obs : state -> obs
  val trans : state -> trans
  val obs_equiv : obs -> obs -> bool
  val succs : trans -> trans -> (state * state) list
end

module DFA : Params = struct
  type state = int [@@deriving hash, compare, sexp]
  type obs = bool
  type alphabet = bool
  let alphabet = [true; false]
  type trans = alphabet -> state

  let obs q = failwith "todo"
  let trans q s = failwith "todo"
  let obs_equiv = Bool.equal
  let succs t1 t2 = List.map alphabet ~f:(fun s -> (t1 s, t2 s))
end

(* module NkAutomaton : Params = struct
  module FDD : sig
    type t
    val equiv : t -> t -> bool
  end

  (** "Symbolic" packet, i.e. a set of packets represented as a predicate *)
  type packet = FDD.t
  type stateId = int64

  type state = stateId * packet (* state id plus current packet, represented as boolean predicate *)


  type obs = FDD.t
  let obs (id,_ : state) : obs = fst (get_state id)

  let obs_equiv = FDD.equiv

  type trans = FDD.t * packet
  let trans (id,pk : state) : trans = (snd (get_state id), pk)

  let succs (id1, pk1) (id2, pk2) =
    let e1,d1 = get_state id1 in
    let e2,ds =
    FDD.fold2 t1 t2
      ~f:(fun l1 l2 -> )

end *)


module Decision(Params : Params) = struct
  open Params

  module State_rel = Hash_set.Make(struct
    type t = state * state [@@deriving hash, compare, sexp]
  end)

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
    let rel = State_rel.create () in

    let rec loop todo =
      match todo with
      | [] -> true
      | (x,y)::todo when Hash_set.mem rel (x, y) -> loop todo
      | (x,y)::_ when not (obs_equiv (obs x) (obs y)) -> false
      | (x,y)::todo -> loop (todo @ succs (trans x) (trans y))
    in
    loop [(x,y)]

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



