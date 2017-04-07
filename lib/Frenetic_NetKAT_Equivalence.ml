open Core.Std

module Automaton = Frenetic_NetKAT_Compiler.Automaton
module FDD = Frenetic_NetKAT_Compiler.FDD


(*===========================================================================*)
(* UPTO                                                                      *)
(*===========================================================================*)

type state = FDD.t * FDD.t

module type UPTO = sig
  val add_equiv : state -> state -> unit
  val equiv : state -> state -> bool
end

(* upto reflexivity & symmetry *)
module Upto_Sym () : UPTO = struct
  (* FIXME: avoid polymorphic hash/max/min/equality *)
  let cache = Hash_set.Poly.create ()
  let equiv s1 s2 = (s1 = s2) || Hash_set.mem cache (min s1 s2, max s1 s2)
  let add_equiv s1 s2 = Hash_set.add cache (min s1 s2, max s1 s2)
end

(* upto reflexivity & symmetry & transitivity (Hopcroft-Karp) *)
module Upto_Trans () : UPTO = struct
  (* FIXME: avoid polymorphic hash/max/min/equality *)
  let cache = Hashtbl.Poly.create ()
  let find = Hashtbl.find_or_add cache ~default:Union_find.create
  let equiv s1 s2 = (s1 = s2) || Union_find.same_class (find s1) (find s2)
  let add_equiv s1 s2 = Union_find.union (find s1) (find s2)
end



(*===========================================================================*)
(* Transition Trees                                                          *)
(*===========================================================================*)

module Trans_Tree = struct
  include Frenetic_Vlr.Make(Frenetic_Fdd.Field)(Frenetic_Fdd.Value)(struct
    type t = Int.Set.t * Int.Set.t [@@deriving sexp, compare]
    let hash = Hashtbl.hash
    let to_string t = sexp_of_t t |> Sexp.to_string

    let zero = (Int.Set.empty, Int.Set.empty)
    let sum (l1, r1) (l2, r2) = (Set.union l1 l2, Set.union r1 r2)

    (* SJS: no one or product *)
    let one = zero
    let prod _ _ = failwith "no product"
  end)

  let of_seq inj seq =
    let leaf =
      Frenetic_Fdd.Action.(Seq.find_exn seq K)
      |> Frenetic_Fdd.Value.to_int_exn
      |> inj
      |> const
    in
    Frenetic_Fdd.Action.Seq.to_hvs seq
    |> List.fold ~init:leaf ~f:(fun tree test -> cond test tree drop)

  let of_par inj par =
    Frenetic_Fdd.Action.Par.fold par
      ~init:drop 
      ~f:(fun acc seq -> sum acc (of_seq inj seq))

  let of_left_par = of_par (fun i -> (Int.Set.singleton i, Int.Set.empty))
  let of_right_par = of_par (fun i -> (Int.Set.empty, Int.Set.singleton i))
end



(*===========================================================================*)
(* Decision Procedure                                                        *)
(*===========================================================================*)

module Make_Naive(Upto : UPTO) = struct

  module SymPkt = struct
    module T = Map.Make(Frenetic_Fdd.Field)
    include T

    let all = empty

    module Set = Set.Make(struct
      type t = Frenetic_Fdd.Value.t T.t [@@deriving sexp]
      let compare = compare Frenetic_Fdd.Value.compare
    end)

    let apply_seq pk seq =
      Frenetic_Fdd.Action.Seq.to_hvs seq
      |> List.fold ~init:pk ~f:(fun pk (key,data) -> add pk ~key ~data)

    let apply_par pk par : Set.t =
      Frenetic_Fdd.Action.Par.fold par ~init:Set.empty ~f:(fun pks seq ->
        Set.add pks (apply_seq pk seq))
  end


  (** decides equivalence of symbolic NetKAT NFAs *)
  let equiv ?(pk=SymPkt.all) (a1 : Automaton.t) (a2 : Automaton.t) =

    let rec eq_state_ids pk (s1 : int) (s2 : int) =
      eq_states pk (Hashtbl.find_exn a1.states s1) (Hashtbl.find_exn a2.states s2)

    and eq_state_id_sets pk (s1s : Int.Set.t) (s2s : Int.Set.t) =
      let merge (a : Automaton.t) states =
        Int.Set.fold states ~init:(FDD.drop, FDD.drop) ~f:(fun (e,d) s ->
          let (e',d') = Hashtbl.find_exn a.states s in
          FDD.(union e e', union d d'))
      in
      eq_states pk (merge a1 s1s) (merge a2 s2s)
    
    and eq_states pk ((e1, d1) : FDD.t * FDD.t) ((e2, d2) : FDD.t * FDD.t) =
      let mask = SymPkt.to_alist pk in
      let ((e1, d1) as s1) = FDD.(restrict mask e1, restrict mask d1) in
      let ((e2, d2) as s2) = FDD.(restrict mask e2, restrict mask d2) in
      Upto.equiv s1 s2 || begin
        Upto.add_equiv s1 s2;
        eq_es pk e1 e2 && eq_ds pk d1 d2
      end

    and eq_es pk = eq_fdd pk ~leaf_eq:(fun pk par1 par2 ->
      SymPkt.Set.equal (SymPkt.apply_par pk par1) (SymPkt.apply_par pk par2))

    and eq_ds pk = eq_fdd pk ~leaf_eq:(fun pk par1 par2 ->
      eq_trans_tree pk (Trans_Tree.(sum (of_left_par par1) (of_right_par par2))))

    and eq_trans_tree pk tree =
      match Trans_Tree.unget tree with
      | Leaf (ksl, ksr) ->
        eq_state_id_sets pk ksl ksr
      | Branch ((f,n), tru, fls) ->
        eq_trans_tree (SymPkt.add pk f n) tru && eq_trans_tree pk fls

    and eq_fdd ~leaf_eq pk x y =
      let check_with pk f n x y =
        match SymPkt.find pk f with
        | None ->
          eq_fdd ~leaf_eq (SymPkt.add pk ~key:f ~data:n) x y
        | Some m -> 
          m <> n || eq_fdd ~leaf_eq pk x y
      in
      match FDD.(unget x, unget y) with
      | Leaf r1, Leaf r2 ->
        leaf_eq pk r1 r2
      | Branch ((f,n), xt, xf), Leaf _ ->
        check_with pk f n xt y && eq_fdd ~leaf_eq pk xf y
      | Leaf _, Branch ((g,m), yt, yf) ->
        check_with pk g m x yt && eq_fdd ~leaf_eq pk x yf
      | Branch((f, n), xt, xf), Branch((g, m), yt, yf) ->
        begin match Frenetic_Fdd.(Field.compare f g, Value.compare m n) with
        |  0,  0 -> check_with pk f n xt yt && eq_fdd ~leaf_eq pk xf yf 
        | -1,  _
        |  0, -1 -> check_with pk f n xt y && eq_fdd ~leaf_eq pk xf y
        |  1,  _
        |  0,  1 -> check_with pk g m x yt && eq_fdd ~leaf_eq pk x yf
        |  _     -> assert false
        end

    in
    eq_state_ids pk a1.source a2.source

end


(*===========================================================================*)
(* Instantiations                                                            *)
(*===========================================================================*)

module Hopcroft = Make_Naive(Upto_Trans ())
module Simple = Make_Naive(Upto_Sym ())
