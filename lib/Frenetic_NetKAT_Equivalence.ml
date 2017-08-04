open Core

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


(*===========================================================================*)
(* Symbolic Packets                                                          *)
(*===========================================================================*)

module SymPkt = struct

  module Field = Frenetic_Fdd.Field

  module Value = struct
    include Frenetic_Fdd.Value
    module Set = Set.Make(Frenetic_Fdd.Value)
  end

  module T = Map.Make(Field)
  include T

  let all = empty

  type constr =
    | Eq of Value.t
    | Neq of Value.Set.t
    [@@deriving sexp, compare]

  module Set = Set.Make(struct
    type t = constr T.t [@@deriving sexp]
    let compare = T.compare compare_constr
  end)

  let apply_seq pk seq =
    Frenetic_Fdd.Action.Seq.fold seq ~init:pk ~f:(fun ~key ~data pk ->
      match key with
      | K -> pk
      | F f -> T.add pk ~key:f ~data:(Eq data))

  let apply_par pk par : Set.t =
    Frenetic_Fdd.Action.Par.fold par ~init:Set.empty ~f:(fun pks seq ->
      Set.add pks (apply_seq pk seq))

  let rec restrict_fdd pk fdd =
    match FDD.unget fdd with
    | Leaf _ -> fdd
    | Branch ((f,v), left, right) ->
      begin match T.find pk f with
      | Some (Eq v') when v = v'              -> restrict_fdd pk left
      | Some (Eq v')                          -> restrict_fdd pk right
      | Some (Neq vs) when Value.Set.mem vs v -> restrict_fdd pk right
      | Some (Neq _) | None                   ->
        FDD.unchecked_cond (f,v) (restrict_fdd pk left) (restrict_fdd pk right)
      end

  let set_eq pk f n =
    T.add pk f (Eq n)

  let branch pk f v =
    match T.find pk f with
    | Some (Eq v') when v' = v              -> `Left pk
    | Some (Eq v')                          -> `Right pk
    | Some (Neq vs) when Value.Set.mem vs v -> `Right pk
    | Some (Neq vs) ->
      `Both (T.add pk f (Eq v), T.add pk f (Neq Value.Set.(add vs v)))
    | None ->
      `Both (T.add pk f (Eq v), T.add pk f (Neq Value.Set.(singleton v)))

  let to_string pk =
    List.to_string (to_alist pk) ~f:(function
      | (f, Eq v) ->
        sprintf "%s=%s" (Field.to_string f) (Value.to_string v)
      | (f, Neq vs) ->
        Value.Set.to_list vs
        |> List.to_string ~f:Value.to_string
        |> sprintf "%s!=%s" (Field.to_string f))
    |>  sprintf "[%s]"

end


(*===========================================================================*)
(* Decision Procedure                                                        *)
(*===========================================================================*)

module Make_Naive (Upto : UPTO) = struct

  (** decides equivalence of symbolic NetKAT NFAs *)
  let equiv ?(pk=SymPkt.all) (a1 : Automaton.t) (a2 : Automaton.t) =

    let rec eq_state_ids pk (s1 : int) (s2 : int) =
      eq_states pk (Hashtbl.find_exn a1.states s1) (Hashtbl.find_exn a2.states s2)

    and eq_state_id_sets pk (s1s : Int.Set.t) (s2s : Int.Set.t) =
      let to_s set = List.to_string ~f:Int.to_string (Set.to_list set) in 
      (* printf "\n[eq_state_id_sets] ----------------------------------\n"; *)
      (* printf "pk = %s\n" (SymPkt.to_string pk); *)
      (* printf "%s = %s?\n" (to_s s1s) (to_s s2s); *)
      let merge (a : Automaton.t) states =
        Int.Set.fold states ~init:(FDD.drop, FDD.drop) ~f:(fun (e,d) s ->
          let (e',d') = Hashtbl.find_exn a.states s in
          FDD.(union e e', union d d'))
      in
      let eq = eq_states pk (merge a1 s1s) (merge a2 s2s) in
      (* printf "-> %s\n" (Bool.to_string eq); *) eq
    
    and eq_states pk ((e1, d1) : FDD.t * FDD.t) ((e2, d2) : FDD.t * FDD.t) =
      (* printf "\n[eq_states ----------------------------------\n"; *)
      (* printf "pk = %s\n" (SymPkt.to_string pk); *)
      (* printf "%s = %s\nand\n%s = %s\n?\n" (FDD.to_string e1) (FDD.to_string e2) *)
        (* (FDD.to_string d1) (FDD.to_string d2); *)
      let ((e1, d1) as s1) = SymPkt.(restrict_fdd pk e1, restrict_fdd pk d1) in
      let ((e2, d2) as s2) = SymPkt.(restrict_fdd pk e2, restrict_fdd pk d2) in
      (* printf "suffices:\n%s = %s\nand\n%s = %s\n?\n" *)
        (* (FDD.to_string e1) (FDD.to_string e2) *)
        (* (FDD.to_string d1) (FDD.to_string d2); *)
      let eq = Upto.equiv s1 s2 || begin
        Upto.add_equiv s1 s2;
        eq_es pk e1 e2 && eq_ds pk d1 d2
      end in
      (* printf "-> %s\n" (Bool.to_string eq); *)
      eq

    and eq_es pk = eq_fdd pk ~leaf_eq:(fun pk par1 par2 ->
      SymPkt.Set.equal (SymPkt.apply_par pk par1) (SymPkt.apply_par pk par2))

    and eq_ds pk = eq_fdd pk ~leaf_eq:(fun pk par1 par2 ->
      eq_trans_tree pk (Trans_Tree.(sum (of_left_par par1) (of_right_par par2))))

    and eq_trans_tree pk tree =
      (* printf "\n[eq_trans_tree] ----------------------------------\n"; *)
      (* printf "pk = %s\n" (SymPkt.to_string pk); *)
      (* printf "%s" (Trans_Tree.to_string tree); *)
      (* printf "\n"; *)
      match Trans_Tree.unget tree with
      | Leaf (ksl, ksr) ->
        let eq = eq_state_id_sets pk ksl ksr in
        (* printf "-> %s\n" (Bool.to_string eq); *) eq
      | Branch ((f,n), tru, fls) ->
        let eq = eq_trans_tree (SymPkt.set_eq pk f n) tru && eq_trans_tree pk fls in
        (* printf "-> %s\n" (Bool.to_string eq); *) eq

    and eq_fdd ~leaf_eq pk x y =
      (* printf "\n[eq_fdd ----------------------------------\n"; *)
      (* printf "pk = %s\n" (SymPkt.to_string pk); *)
      (* printf "%s = %s\n?\n" (FDD.to_string x) (FDD.to_string y); *)
      let check_branches f n (lx, ly) (rx, ry) =
        match SymPkt.branch pk f n with
        | `Left pk -> eq_fdd ~leaf_eq pk lx ly
        | `Right pk -> eq_fdd ~leaf_eq pk rx ry
        | `Both (lpk, rpk) -> eq_fdd ~leaf_eq lpk lx ly && eq_fdd ~leaf_eq rpk rx ry
      in
      let eq =
        match FDD.(unget x, unget y) with
        | Leaf r1, Leaf r2 ->
          leaf_eq pk r1 r2
        | Branch ((f,n), lx, rx), Leaf _ ->
          check_branches f n (lx, y) (rx, y)
        | Leaf _, Branch ((g,m), ly, ry) ->
          check_branches g m (x, ly) (x, ry)
        | Branch((fx, vx), lx, rx), Branch((fy, vy), ly, ry) ->
          begin match Frenetic_Fdd.(Field.compare fx fy, Value.compare vx vy) with
          |  0,  0 -> check_branches fx vx (lx, ly) (rx, ry)
          | -1,  _
          |  0, -1 -> check_branches fx vx (lx, y) (rx, y)
          |  1,  _
          |  0,  1 -> check_branches fy vy (x, ly) (x, ry)
          |  _     -> assert false
          end
        in
          (* printf "-> %s\n" (Bool.to_string eq); *)
          eq

    in
    Upto.clear ();
    eq_state_ids pk a1.source a2.source

end


(*===========================================================================*)
(* Instantiations                                                            *)
(*===========================================================================*)

module Hopcroft = Make_Naive(Upto_Trans ())
module Simple = Make_Naive(Upto_Nada ())
