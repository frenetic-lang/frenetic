open Core
open Frenetic_netkat

module type AUTOMATON = sig

  module Fdd : sig
    type t
    val equiv : t -> t -> t
  end

  type state [@@deriving compare, sexp]

  type t = {
    start : state;
    epsilon : state -> Fdd.t;
    delta : state -> Fdd.t;
  }
end


module Decision(Automaton : AUTOMATON) = struct

  let equiv (x : Automaton.t) (y : Automaton.t) : bool =
    failwith "todo"



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




