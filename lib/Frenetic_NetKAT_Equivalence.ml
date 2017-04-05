open Core.Std

module A = Frenetic_NetKAT_Compiler.Automaton
module FDD = struct
  include Frenetic_NetKAT_Compiler.FDD
  include (struct include Frenetic_NetKAT_Compiler end : sig
    val seq : t -> t -> t
  end)

  let rec strict_equal (x : t) (y : t) =
    match unget x, unget y with
    | Branch((vx, lx), tx, fx), Branch((vy, ly), ty, fy) ->
      begin match Frenetic_Fdd.Field.compare vx vy with
      |  0 ->
        begin match Frenetic_Fdd.Value.compare lx ly with
        |  0 -> strict_equal tx ty && strict_equal fx fy
        | -1 -> failwith "todo"
        |  1 -> failwith "todo"
        |  _ -> assert false
        end
      | -1 -> failwith "todo"
      |  1 -> failwith "todo"
      |  _ -> assert false
      end
end


module Naive = struct

  type state = int
  type mask = FDD.t

  let equiv ?(mask=FDD.id) (a1 : A.t) (a2 : A.t) =
    let cache = Hash_set.Poly.create () in
    
    let rec eq_states (s1 as a, x : mask * state) (s2 as b, y : mask * state) =
      Hash_set.mem cache (s1,s2) || begin
        Hash_set.add cache (s1,s2);
        eq_states' (a, Hashtbl.find_exn a1.states x)
                   (b, Hashtbl.find_exn a2.states y)
      end

    and eq_states' (a, (e1,d1) : mask * (FDD.t * FDD.t))
                   (b, (e2,d2) : mask * (FDD.t * FDD.t)) =
      FDD.strict_equal (FDD.seq a e1) (FDD.seq b e2) && eq_ds (a, d1) (b,d2)

    and eq_ds (a, d1) (b, d2) =
      true

    in
    eq_states (mask, a1.source) (mask, a2.source)

end
