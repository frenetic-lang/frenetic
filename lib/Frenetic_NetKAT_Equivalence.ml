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
  val clear : unit -> unit
end

(* upto nothing; reflexivity and symmetry DO NOT hold *)
module Upto_Nada () : UPTO = struct
  (* FIXME: avoid polymorphic hash/max/min/equality *)
  let cache = Hash_set.Poly.create ()
  let equiv s1 s2 = Hash_set.mem cache (s1, s2)
  let add_equiv s1 s2 = Hash_set.add cache (s1, s2)
  let clear () = Hash_set.clear cache
end

(* upto transitivity (Hopcroft-Karp) *)
module Upto_Trans () : UPTO = struct
  (* FIXME: avoid polymorphic hash/max/min/equality *)
  let cache_left = FDD.BinTbl.create ()
  let cache_right = FDD.BinTbl.create ()
  let find_left = FDD.BinTbl.find_or_add cache_left ~default:Union_find.create
  let find_right = FDD.BinTbl.find_or_add cache_right ~default:Union_find.create
  let equiv s1 s2 = Union_find.same_class (find_left s1) (find_right s2)
  let add_equiv s1 s2 = Union_find.union (find_left s1) (find_right s2)
  let clear () = FDD.BinTbl.clear cache_left; FDD.BinTbl.clear cache_right
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

module Boolean_Tree = struct
  include Frenetic_Vlr.Make(Frenetic_Fdd.Field)(Frenetic_Fdd.Value)(struct
    include Bool
    let zero = false
    let one = true
    let sum = (||)
    let prod = (&&)
  end)

  let one = id
  let zero = drop

end


(* IDEA: build product trees *)

module Prod_FDD = struct
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

module type GUARD = sig
  type t
  val one : t
  val zero : t
  val set : t -> Frenetic_Fdd.Field.t -> Frenetic_Fdd.Value.t -> t
  val cases : t -> Frenetic_Fdd.Field.t -> Frenetic_Fdd.Value.t -> t
  val restrict_fdd : t -> FDD.t -> FDD.t

  val apply_seq : t -> Frenetic_Fdd.Value.t Frenetic_Fdd.Action.Seq.t -> t
  val apply_par : t -> Frenetic_Fdd.Action.Par.t -> t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
  

end

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

  let to_string pk =
    sprintf "[%s]" (List.to_string (to_alist pk) ~f:(fun (f,v) ->
      sprintf "%s=%s" (Frenetic_Fdd.Field.to_string f) (Frenetic_Fdd.Value.to_string v)
    ))
end

module Make_Naive (Upto : UPTO) (Guard : SYMPKT) = struct

  (** decides equivalence of symbolic NetKAT NFAs *)
  let equiv ?(guard=Guard.one) (a1 : Automaton.t) (a2 : Automaton.t) =

    let rec eq_state_ids guard (s1 : int) (s2 : int) =
      eq_states guard (Hashtbl.find_exn a1.states s1) (Hashtbl.find_exn a2.states s2)

    and eq_state_id_sets guard (s1s : Int.Set.t) (s2s : Int.Set.t) =
      let merge (a : Automaton.t) states =
        Int.Set.fold states ~init:(FDD.drop, FDD.drop) ~f:(fun (e,d) s ->
          let (e',d') = Hashtbl.find_exn a.states s in
          FDD.(union e e', union d d'))
      in
      eq_states guard (merge a1 s1s) (merge a2 s2s)
    
    and eq_states guard ((e1, d1) : FDD.t * FDD.t) ((e2, d2) : FDD.t * FDD.t) =
      let ((e1, d1) as s1) = Guard.(restrict_fdd guard e1, restrict_fdd guard d1) in
      let ((e2, d2) as s2) = Guard.(restrict_fdd guard e2, restrict_fdd guard d2) in
      Upto.equiv s1 s2 || begin
        Upto.add_equiv s1 s2;
        eq_es guard e1 e2 && eq_ds guard d1 d2
      end

    and eq_es guard e1 e2 =
      FDD.equal e1 e2 || eq_fdd guard ~leaf_eq:(fun guard par1 par2 ->
        Guard.Set.equal (Guard.apply_par guard par1) (Guard.apply_par guard par2))

    and eq_ds guard = eq_fdd guard ~leaf_eq:(fun guard par1 par2 ->
      eq_trans_tree guard (Trans_Tree.(sum (of_left_par par1) (of_right_par par2))))

    and eq_trans_tree guard tree =
      printf "\n[eq_trans_tree] ----------------------------------\n";
      printf "guard = %s\n" (Guard.to_string guard);
      printf "%s" (Trans_Tree.to_string tree);
      printf "\n";
      match Trans_Tree.unget tree with
      | Leaf (ksl, ksr) ->
        eq_state_id_sets guard ksl ksr
      | Branch ((f,n), tru, fls) ->
        eq_trans_tree (Guard.set guard f n) tru && eq_trans_tree guard fls

    and eq_fdd ~leaf_eq guard x y =
      match FDD.(unget x, unget y) with
      | Leaf r1, Leaf r2 ->
        leaf_eq guard r1 r2
      | Branch ((f,n), xt, xf), Leaf _ ->
        let (gtrue, gfalse) = Guard.cases guard f n in
        eq_fdd ~leaf_eq gtrue xt y && eq_fdd ~leaf_eq gfalse xf y
      | Leaf _, Branch ((g,m), yt, yf) ->
        let (gtrue, gfalse) = Guard.cases guard g m in
        eq_fdd ~leaf_eq gtrue x yt && eq_fdd ~leaf_eq gfalse x yf
      | Branch((f, n), xt, xf), Branch((g, m), yt, yf) ->
        begin match Frenetic_Fdd.(Field.compare f g, Value.compare m n) with
        |  0,  0 ->
          let (gtrue, gfalse) = Guard.cases guard f n in
          eq_fdd ~leaf_eq gtrue xt yt && eq_fdd ~leaf_eq gfalse xf yf
        | -1,  _ |  0, -1 ->
          let (gtrue, gfalse) = Guard.cases guard f n in
          eq_fdd ~leaf_eq gtrue xt y && eq_fdd ~leaf_eq gfalse xf y
        |  1,  _ |  0,  1 ->
          let (gtrue, gfalse) = Guard.cases guard g m in
          eq_fdd ~leaf_eq gtrue x yt && eq_fdd ~leaf_eq gfalse x yf
        |  _     -> 
          assert false
        end

    in
    Upto.clear ();
    eq_state_ids guard a1.source a2.source

end


(*===========================================================================*)
(* Instantiations                                                            *)
(*===========================================================================*)

module Hopcroft = Make_Naive(Upto_Trans ())(Guard)
module Simple = Make_Naive(Upto_Nada ())(Guard)
