open Core

module Field = struct
  module T = struct
    type t
      =
      | F0
      | F1
      | F2
      | F3
      | F4
      | F5
      | F6
      | F7
      | F8
      | F9
      | F10
      | F11
      | F12
      | F13
      | F14
      | F15
      | F16
      | F17
      | F18
      | F19
      | Meta0
      | Meta1
      | Meta2
      | Meta3
      | Meta4
      | Meta5
      | Meta6
      | Meta7
      | Meta8
      | Meta9
      | Meta10
      | Meta11
      | Meta12
      | Meta13
      | Meta14
      | Meta15
      | Meta16
      | Meta17
      | Meta18
      | Meta19
      [@@deriving sexp, enumerate, enum, eq, hash, bin_io]

    let num_fields = max + 1

    let order = Array.init num_fields ~f:ident

    (* compare depends on current order! *)
    let compare (x : t) (y : t) : int =
      (* using Obj.magic instead of to_enum for bettter performance *)
      Int.compare order.(Obj.magic x) order.(Obj.magic y)
  end

  include T
  module Map = Map.Make(T)
  module Set = Set.Make(T)

  type field = t

  let of_string s =
    t_of_sexp (Sexp.of_string s)

  (** Hack: this way we always know what the abstract fields refer to *)
  let field_to_str_map : (t, string) Hashtbl.t = Hashtbl.Poly.create ()

  (** SJS: of_string o to_string may fail *)
  let to_string t =
    match Hashtbl.find field_to_str_map t with
    | None -> sprintf "<%s>" @@ Sexp.to_string (sexp_of_t t)
    | Some s -> s

  let pp fmt t =
    Format.fprintf fmt "%s" (to_string t)

  let is_valid_order (lst : t list) : bool =
    Set.(equal (of_list lst) (of_list all))

  let set_order (lst : t list) : unit =
    assert (is_valid_order lst);
    List.iteri lst ~f:(fun i fld -> order.(to_enum fld) <- i)

  (* Not a clean way to invert a permutation, but fast *)
  let invert arr =
    let inverted = Array.init num_fields ~f:ident in
    Array.iteri arr ~f:(fun i elt -> inverted.(elt) <- i );
    inverted

  let get_order () =
    Array.to_list (invert order)
    |> List.filter_map ~f:of_enum

  module type ENV = sig
    type t
    val empty : t
    exception Full
    val add : t -> string -> field Syntax.meta_init -> bool -> t (* may raise Full *)
    val lookup : t -> string -> field * (field Syntax.meta_init * bool) (* may raise Not_found *)
  end

  module Env : ENV = struct

    type t = {
      alist : (string * (field * (field Syntax.meta_init * bool))) list;
      depth : int
    }

    let empty = { alist = []; depth = 0 }

    exception Full

    let add env name init mut =
      let field =
        match env.depth with
        | 0 -> Meta0
        | 1 -> Meta1
        | 2 -> Meta2
        | 3 -> Meta3
        | 4 -> Meta4
        | 5 -> Meta5
        | 6 -> Meta6
        | 7 -> Meta7
        | 8 -> Meta8
        | 9 -> Meta9
        | 10 -> Meta10
        | 11 -> Meta11
        | 12 -> Meta12
        | 13 -> Meta13
        | 14 -> Meta14
        | 15 -> Meta15
        | 16 -> Meta16
        | 17 -> Meta17
        | 18 -> Meta18
        | 19 -> Meta19
        | _ -> raise Full
      in
      { alist = List.Assoc.add ~equal:(=) env.alist name (field, (init, mut));
        depth = env.depth + 1}

    let lookup env name =
      List.Assoc.find_exn ~equal:(=) env.alist name
  end

  (* Heuristic to pick a variable order that operates by scoring the fields
     in a policy. A field receives a high score if, when a test field=X
     is false, the policy can be shrunk substantially.

     NOTE(arjun): This could be done better, but it seems to work quite well
     on FatTrees and the SDX benchmarks. Some ideas for improvement:

     - Easy: also account for setting tests field=X suceeded
     - Harder, but possibly much better: properly calculate the size of the
       pol for different field assignments. Don't traverse the policy
       repeatedly. Instead, write a size function that returns map from
       field assignments to sizes. *)
  let auto_order (pol : t Syntax.policy) : unit =
    let open Syntax in
    (* Construct array of scores, where score starts at 0 for every field *)
    let count_arr = Array.init num_fields ~f:(fun _ -> 0) in
    let rec f_pred size (pred : t Syntax.pred) =
      match pred with
      | True | False ->
        ()
      | Test (f,_) ->
        let f = to_enum f in
        count_arr.(f) <- count_arr.(f) + size
      | Or (a, b) | And (a, b) ->
        f_pred size a; f_pred size b
      | Neg a ->
        f_pred size a
    in
    let rec f_seq' (pol : t Syntax.policy) lst k =
      match pol with
      | Modify _ ->
        k (1, lst)
      | Filter a ->
        k (1, a :: lst)
      | Seq (p, q) ->
        f_seq' p lst (fun (m, lst) ->
          f_seq' q lst (fun (n, lst) ->
            k (m * n, lst)
          )
        )
      | Ite (a,p,q) ->
        k (f_union a p q, lst)
      | Branch {branches} ->
        List.fold branches ~init:PNK.drop ~f:(fun p (a,q) ->
          PNK.(ite a q p)
        )
        |> (fun p -> f_seq' p lst k)
      | Choice ps ->
        k (f_choice ps, lst)
      | Let { id; init; mut; body=p } ->
        f_seq' p lst k
      | While (a,p) ->
        k (f_union a p PNK.skip, lst)
      | ObserveUpon (p,a) ->
        f_seq' (Seq (p, Filter a)) lst k
    and f_seq pol : int =
      let (size, preds) = f_seq' pol [] (fun x -> x) in
      List.iter preds ~f:(f_pred size);
      size
    and f_union' pol lst k = match pol with
      | Modify _ ->
        k (1, lst)
      | Filter a ->
        k (1, a :: lst)
      | Ite (a, p, q) ->
        f_union' p lst (fun (m, lst) ->
          f_union' q lst (fun (n, lst) ->
            k (m + n, a::lst)))
      | Branch {branches} ->
        List.fold branches ~init:PNK.drop ~f:(fun p (a,q) ->
          PNK.(ite a q p)
        )
        |> (fun p -> f_union' p lst k)
      | Choice ps ->
        List.map ps ~f:fst
        |> List.map ~f:(fun p -> f_union' p [] ident)
        |> List.fold ~init:(0,[]) ~f:(fun (n,l) (n',l') -> (n+n', l@l'))
        |> k
      | Seq _ ->
        k (f_seq pol, lst)
      | Let { id; init; mut; body=p } ->
        k (f_seq p, lst)
      | While (a,p) ->
        f_union' p lst (fun (m, lst) ->
          k (m, a::lst)
        )
      | ObserveUpon (p,a) ->
        f_union' (Seq (p, Filter a)) lst k
    and f_union a p q : int =
      let (p_size, p_preds) = f_union' p [] (fun x -> x) in
      let (q_size, q_preds) = f_union' q [] (fun x -> x) in
      let size = p_size + q_size in
      List.iter p_preds ~f:(f_pred p_size);
      List.iter q_preds ~f:(f_pred q_size);
      f_pred size a;
      size
    and f_choice ps : int =
      List.map ps ~f:fst
      |> List.map ~f:(fun p -> f_union' p [] ident)
      |> Util.tap ~f:(List.iter ~f:(fun (size, preds) -> List.iter preds ~f:(f_pred size)))
      |> List.map ~f:fst
      |> List.fold ~init:0 ~f:(+)
    in
    let _ = f_seq pol in
    Array.foldi count_arr ~init:[] ~f:(fun i acc n -> ((Obj.magic i, n) :: acc))
    |> List.stable_sort ~compare:(fun (_, x) (_, y) -> Int.compare x y)
    |> List.rev (* SJS: do NOT remove & reverse order! Want stable sort *)
    |> List.map ~f:fst
    |> set_order

end

module Value = struct
  include Int
  let subset_eq = equal
end



module PreAction = struct
  module T = Field.Map
  open T

  type t = Value.t Field.Map.t [@@deriving sexp, eq]

  let compare = compare_direct Value.compare
  let one = empty
  let is_one = is_empty
  let hash_fold_t = Map.hash_fold_direct Field.hash_fold_t Value.hash_fold_t

  let prod x y =
    (* Favor modifications to the right *)
    merge x y ~f:(fun ~key m ->
      match m with | `Both(_, v) | `Left v | `Right v -> Some(v))

  let to_hvs = to_alist ~key_order:`Increasing

  let to_string (t : t) : string =
    if T.is_empty t then "skip" else
    to_alist t
    |> List.map ~f:(fun (f,v) ->
        sprintf "%s := %s" (Field.to_string f) (Value.to_string v))
    |> String.concat ~sep:", "
    |> sprintf "[%s]"
end

module Action = struct
  type t =
  | Drop
  | Action of PreAction.t
  [@@deriving sexp, compare, hash, eq]

  let zero = Drop
  let one = Action PreAction.one

  let is_one = function
    | Drop -> false
    | Action a -> PreAction.is_one a

  let prod x y =
    match x,y with
    | Drop, _
    | _, Drop -> Drop
    | Action a1, Action a2 -> Action (PreAction.prod a1 a2)

  let sum x y = failwith "multicast not implemented!"

  let to_hvs t = match t with
    | Drop -> None
    | Action a -> Some (PreAction.to_hvs a)

  let pp fmt (t : t) : unit =
    match t with
    | Drop -> Format.fprintf fmt "drop"
    | Action a -> Format.fprintf fmt "%s"(PreAction.to_string a)


  let to_string t =
    Format.asprintf "%a" pp t
end



module ActionDist : sig
  type t [@@deriving sexp, hash, compare, eq]
  val zero : t
  val one : t
  val is_zero : t -> bool
  val is_one : t -> bool
  val prod : t -> t -> t
  val sum : t -> t -> t
  val negate : t -> t
  val convex_sum : t -> Prob.t -> t -> t

  val prob_of : t -> Action.t -> Prob.t

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val to_alist : t -> (Action.t * Prob.t) list
  val of_alist_exn : (Action.t * Prob.t) list -> t

  val support : t -> Action.t list
  val dirac : Action.t -> t
  val pushforward : t -> f:(Action.t -> Action.t) -> t

  val empty : t
  val unsafe_add : t -> Prob.t -> Action.t -> t
  val unsafe_normalize : t -> t

  val observe_not_drop : t -> t

  (**
    The three events
      A = "f<-n",
      B = "f<-m for m!=n", and
      C = "no assignment to f"
    are a parition of the probability space. Split the input distribution
    into the three conditional distributions
      dist|A, dist|B, dist|C
    together with their weights dist(A), dist(B), dist(C).
  *)
  val split_into_conditionals :
    t -> Field.t * Value.t -> ((t * Prob.t) * (t * Prob.t) * (t * Prob.t))
end = struct

  module T = Dist.Make(Action)
  include T

  let zero = dirac Action.zero
  let is_zero = T.equal zero

  let one = T.dirac Action.one
  let is_one = T.equal one

  let prod x y =
    if is_zero x || is_zero y then
      zero
    else if is_one x then
      y
    else if is_one y then
      x
    else
      T.prod_with ~f:Action.prod x y

  let negate t : t =
    (* This implements negation for the [zero] and [one] actions. Any
       non-[zero] action will be mapped to [zero] by this function. *)
    if is_zero t then one else zero

  let pp fmt t =
    to_alist t
    |> List.map ~f:(fun (act, prob) ->
        Format.asprintf "@[%s @@ %s@]" (Action.to_string act) (Prob.to_string prob)
      )
    |> String.concat ~sep:"; "
    |> Format.fprintf fmt "{ %s }"

  let to_string t =
    Format.asprintf "%a" pp t

  (* sum = ampersand (and ampersand only!). It should ever only be used to
     implement disjunction. Thus, we must have x,y \in {0,1}  *)
  let sum x y =
    if is_zero x then y else
    if is_zero y then x else
    if is_one x && is_one y then x else
      failwith (sprintf "multicast not implemented! cannot add (in the sense of &) %s and %s"
        (to_string x) (to_string y)
      )

  let split_into_conditionals dist (f,n) =
    let (yes, no, mb) =
      List.partition3_map (to_alist dist) ~f:(fun ((act, p) as outcome) ->
        match act with
        | Action.Drop ->
          `Trd outcome
        | Action.Action act ->
          begin match PreAction.T.find act f with
          | None ->
            `Trd outcome
          | Some n' ->
            if n = n' then
              `Fst outcome
            else
              `Snd outcome
          end
      )
    in
    let finish_conditional dist =
      if List.is_empty dist then (zero, Prob.zero) else
      let mass =
        List.map dist ~f:snd
        |> List.fold ~init:Prob.zero ~f:Prob.(+)
      in
      let dist =
        Util.map_snd dist ~f:(fun p -> Prob.(p/mass))
        |> of_alist_exn
      in
      (dist, mass)
    in
    (finish_conditional yes, finish_conditional no, finish_conditional mb)

  let observe_not_drop t =
    T.observe t ~bot:Action.Drop ~f:(function Action.Drop -> false | _ -> true)

end

(* hacky *)
module FactorizedActionDist : sig
  type t [@@deriving sexp, hash, compare, eq]
  val zero : t
  val one : t
  val is_zero : t -> bool
  val is_one : t -> bool
  val prod : t -> t -> t
  val sum : t -> t -> t
  val negate : t -> t
  val convex_sum : t -> Prob.t -> t -> t

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val to_alist : t -> (Action.t * Prob.t) list

  val prob_of_drop : t -> Prob.t

  (* SJS: do not expose, since it will screw up factorization *)
  (* val of_alist_exn : (Action.t * Prob.t) list -> t *)

  val support : t -> Action.t list
  val dirac : Action.t -> t
  val pushforward : t -> f:(Action.t -> Action.t) -> t

  val empty : t

  val factorize : ActionDist.t -> t
  val to_joined : t -> ActionDist.t

  (** The three events
        A = "f<-n",
        B = "f<-m for m!=n", and
        C = "no assignment to f"
      are a parition of the probability space. Split the input distribution
      into the three conditional distributions
        dist|A, dist|B, dist|C
      together with their weights dist(A), dist(B), dist(C).
  *)
  val split_into_conditionals :
    t -> Field.t * Value.t -> ((t * Prob.t) * (t * Prob.t) * (t * Prob.t))

  val observe_not_drop : t -> t

end = struct
  type t = ActionDist.t list
  [@@deriving sexp, hash, compare, eq]

  (** avoid having lots of skips *)
  let canonicalize t =
    List.filter t ~f:(fun d -> not (ActionDist.is_one d))

  (* invariant? *)
  let zero = [ActionDist.zero]
  let one = []
  let empty = [ActionDist.empty]
  let dirac x = canonicalize [ActionDist.dirac x]

  let is_zero t =
    match t with
    | [d] -> ActionDist.is_zero d
    | _ -> false

  let is_one t =
    match t with
    | [] -> true
    | _ -> false

  (** this is not correct in general, but works for the uses of `pushforward`
      in this module *)
  let pushforward t ~f =
    List.map t ~f:(ActionDist.pushforward ~f)
    |> canonicalize

  let to_joined t =
    List.fold t ~init:ActionDist.one ~f:ActionDist.prod

  (** SJS: very poor factorization... *)
  let factorize dist =
    canonicalize [dist]

  (* try to factorize joint distribution *)
  let of_joined = factorize


  let to_alist = Fn.compose ActionDist.to_alist to_joined
  let of_alist_exn = Fn.compose of_joined ActionDist.of_alist_exn

  let support t =
    ActionDist.support (to_joined t)
(*     List.map t ~f:ActionDist.support
    |> List.fold ~init:[Action.one] ~f:(fun supp_left supp_right ->
      List.concat_map supp_left ~f:(fun left -> List.map supp_right ~f:(Action.prod left))
    ) *)

  let to_string t =
    List.map t ~f:ActionDist.to_string
    |> String.concat ~sep:" x "

  let pp fmt t =
    Format.fprintf fmt "%s" (to_string t)

  (* sum = ampersand (and ampersand only!). It should ever only be used to
   implement disjunction. Thus, we must have x,y \in {0,1}  *)
  let sum x y =
    if is_zero x then y else
    if is_zero y then x else
    if is_one x && is_one y then x else
      failwith (sprintf "multicast not implemented! cannot add (in the sense of &) %s and %s"
        (to_string x) (to_string y)
      )

  let negate t : t =
    (* This implements negation for the [zero] and [one] actions. Any
       non-[zero] action will be mapped to [zero] by this function. *)
    if is_zero t then one else zero


  let prod t1 t2 =
    if is_zero t1 || is_one t2 then t1 else
    if is_zero t2 || is_one t1 then t2 else
    let rec loop t1 t2 = match t2 with
      | [] ->
        t1
      | d::t2 ->
        let supp_d = ActionDist.support d in
        let has_drop_d = List.mem supp_d Drop ~equal:Action.equal in
        let has_field_d =
          List.concat_map supp_d ~f:(function
            | Drop -> []
            | Action act -> PreAction.T.keys act
          )
          |> Field.Set.of_list
          |> Field.Set.mem
        in
        let is_conflicted d' =
          ActionDist.support d'
          |> List.exists ~f:(function
            | Action.Drop ->
              has_drop_d
            | Action.Action act ->
              List.exists (PreAction.T.keys act) ~f:has_field_d
          )
        in
        let (dependend, independend) = List.partition_tf t1 ~f:is_conflicted in
        let d = List.fold_right dependend ~init:d ~f:ActionDist.prod in
        loop (d::independend) t2
      in
      loop t1 t2

  (** SJS: we ought to be able to do better...but it does the trick for now  *)
  let convex_sum t1 p t2 =
    if Prob.(equal zero p) then t2 else
    if Prob.(equal one p) then t1 else
    (* let drop p = ActionDist.(convex_sum zero p one) in *)
    (** SJS: this is a hideous hack, but seems to work.
        The proper way would be to handle Skip differently somehow.
        Maybe we should use factored subproability distributions?
     *)
(*     if is_zero t1 then
      drop p :: t2
    else if is_zero t2 then
      drop Prob.(one - p) :: t1
    else *)
      factorize ActionDist.(convex_sum (to_joined t1) p (to_joined t2))

    (* SJS: the implementation below tried to be clever, but that turns out to
       be prohibitively expensive
    *)
    (** intuitively, we pull out the common factors and only take the convex
        combination of the rest *)
(*     let (common, t1) = List.partition_tf t1 ~f:(List.mem t2 ~equal:ActionDist.equal) in
    let t2 = List.filter t2 ~f:(fun d -> not (List.mem common d ~equal:ActionDist.equal)) in
    ActionDist.convex_sum (to_joined t1) p (to_joined t2) :: common *)

  let split_into_conditionals t (f, n) =
    let (f_dependent_factors, f_independent_factors) =
      List.partition_tf t ~f:(fun d ->
        ActionDist.support d
        |> List.exists ~f:(function
          | Action.Drop ->
            false
          | Action.Action act ->
            List.mem (PreAction.T.keys act) f ~equal:Field.equal
        )
      )
    in
    match f_dependent_factors with
    | [] ->
      ((zero, Prob.zero), (zero, Prob.zero), (t, Prob.one))
    | _::_::_ ->
      failwith "bug: must have at most a single f dependent factor"
    | [d] ->
      let yes, no, mb = ActionDist.split_into_conditionals d (f,n) in
      let finish_conditional (dist, mass) =
        if ActionDist.is_zero dist then
          (zero, mass)
        else
          (canonicalize @@ dist::f_independent_factors, mass)
      in
      finish_conditional yes, finish_conditional no, finish_conditional mb

  let observe_not_drop t =
    List.map t ~f:ActionDist.observe_not_drop

  let prob_of_drop t =
    List.fold t ~init:Prob.(one, zero) ~f:(fun (no_drop_yet, drop) d ->
      let p_drop = ActionDist.prob_of d Action.Drop in
      Prob.(no_drop_yet * (one - p_drop), drop + p_drop)
    )
    |> snd

end


(** symbolic packets *)
module PrePacket = struct
  type nomval =
    | Const of Value.t
    | Atom (** An atom in the sense of nominal sets. Some fixed value that is different
               from all constants. To a first approximation, a sort of wildcard, but it ranges
               only over values that do not appear as constants.
            *)
    [@@deriving compare, eq, sexp, hash]

  type t = nomval Field.Map.t [@@deriving compare, eq, sexp]
  (** Symbolic packet. Represents a set of concrete packets { π }.

      f |-> Const v  means π.f = v
      f |-> Atom     means π.f \in { values not appearing as f-values }
      f |-> ⊥        means π.f can have any value

      In particular, the empty map represents the set of all packets, and a map
      that associates a constant with every field represents a singleton set.
  *)

  let hash_fold_t = Map.hash_fold_direct Field.hash_fold_t hash_fold_nomval

  let empty = Field.Map.empty

  let modify (pk : t) (f : Field.t) (v : nomval) : t =
    Map.set pk ~key:f ~data:v

  let test (pk : t) (f : Field.t) (v : Value.t) : bool =
    match Map.find pk f with
    | Some (Const v') when v' = v -> true
    | _ -> false

  let test_with (pk : t) (field : Field.t) (v : Value.t) ~f : bool =
    match Map.find pk field with
    | Some (Const v') when f v' v -> true
    | _ -> false

  let apply (pk : t) (action : PreAction.t) : t =
    Field.Map.merge pk action ~f:(fun ~key:_ -> function
      | `Left v -> Some v
      | `Right v | `Both (_,v) -> Some (Const v))

  let to_preaction pk =
    Map.filter_map pk ~f:(function
      | Const v -> Some v
      | Atom -> None)

  let of_preaction a =
    Map.map a ~f:(fun v -> Const v)

  let pp fmt (pk:t) : unit =
    Format.fprintf fmt "@[";
    if Map.is_empty pk then Format.fprintf fmt "*@ " else
    Map.iteri pk ~f:(fun ~key ~data -> Format.fprintf fmt "@[%s=%s@]@ "
      (Field.to_string key)
      begin match data with
      | Atom -> "*"
      | Const v -> Int.to_string v
      end);
    Format.fprintf fmt "@]"

  let to_string pk : string =
    Format.asprintf "%a" pp pk
end

(** "packet" is a bad name and used just for historicaly reasons. It's a symbolic
    encoding of a packet set (possibly the empty set).
*)
module Packet = struct
  module T = struct
    type t =
      | Emptyset
      | Pk of PrePacket.t
      [@@deriving compare, eq, sexp, hash]

    let pp fmt (pk:t) : unit =
      match pk with
      | Emptyset -> Format.fprintf fmt "@[∅@]"
      | Pk pk -> PrePacket.pp fmt pk

    let to_string pk : string =
      Format.asprintf "%a" pp pk
  end
  include T

  let modify (pk : t) (f : Field.t) (v : PrePacket.nomval) : t =
    match pk with
    | Emptyset -> Emptyset
    | Pk pk -> Pk (PrePacket.modify pk f v)

  let test (pk : t) (f : Field.t) (v : Value.t) : bool =
    match pk with
    | Emptyset -> false
    | Pk pk -> PrePacket.test pk f v

  let test_with (pk : t) (field : Field.t) (v : Value.t) ~f : bool =
    match pk with
    | Emptyset -> false
    | Pk pk -> PrePacket.test_with pk field v ~f

  let apply (pk : t) (act : Action.t) : t =
    match pk, act with
    | Emptyset, _
    | _, Drop ->
      Emptyset
    | Pk pk, Action act ->
      Pk (PrePacket.apply pk act)

  let to_action pk =
    match pk with
    | Emptyset -> Action.Drop
    | Pk pk -> Action.Action (PrePacket.to_preaction pk)

  let of_action a =
    match a with
    | Action.Drop -> Emptyset
    | Action.Action a -> Pk (PrePacket.of_preaction a)

  let empty = Pk PrePacket.empty

  module Dist = struct
    include Dist.Make(T)

    let of_action_dist d =
      ActionDist.to_alist d
      |> Util.map_fst ~f:of_action
      |> of_alist_exn
  end

end


module Fdd00 = Vlr.Make(Field)(Value)(FactorizedActionDist)



(** domain of an Fdd *)
module Domain = struct
  module Valset = Set.Make(struct type t = PrePacket.nomval [@@deriving sexp, compare] end)
  type t = Valset.t Field.Map.t

  let merge : t -> t -> t =
    Map.merge_skewed ~combine:(fun ~key -> Set.union)

  let of_fdd (fdd : Fdd00.t) : t =
    let rec for_fdd dom fdd =
      match Fdd00.unget fdd with
      | Leaf r ->
        for_leaf dom r
      | Branch {test=(field,_)} ->
        let (vs, residuals, all_false) = for_field field fdd [] [] in
        let vs =
          List.map vs ~f:(fun v -> PrePacket.Const v)
          |> Valset.of_list
        in
        let dom = Map.update dom field ~f:(function
          | None -> vs
          | Some vs' -> Set.union vs vs')
        in
        List.fold residuals ~init:dom ~f:for_fdd

    (** returns list of values appearing in tests with field [f] in [fdd], and
        residual trees below f-tests, and the all-false branch with respect to
        field f. *)
    and for_field f fdd vs residual =
      match Fdd00.unget fdd with
      | Branch {test=(f',v); tru; fls} when f' = f ->
        for_field f fls (v::vs) (tru::residual)
      | Branch _ | Leaf _ ->
        (vs, fdd::residual, fdd)

    and for_leaf dom dist =
      FactorizedActionDist.support dist
      |> List.fold ~init:dom ~f:for_action

    and for_action dom action =
      match action with
      | Drop -> dom
      | Action action -> for_preaction dom action

    and for_preaction dom action =
      PreAction.T.to_alist action
      |> Util.map_snd ~f:(fun v -> Valset.singleton (Const v))
      |> Field.Map.of_alist_exn
      |> merge dom

    in
    for_fdd Field.Map.empty fdd
    (* SJS: this is a sound overapproximation, but a very expensive one!! *)
    |> Field.Map.map ~f:(fun vs -> Set.add vs PrePacket.Atom)

  let size (dom : t) : int =
    (* SJS: +1 for the empty set! *)
    Map.fold dom ~init:1 ~f:(fun ~key ~data:vs acc -> acc * (Valset.length vs))
    + 1

  let pre_variants (dom : t) : PrePacket.t list =
    Map.fold dom ~init:[PrePacket.empty] ~f:(fun ~key:f ~data:vs pks ->
      Valset.to_list vs
      |> List.concat_map ~f:(fun v -> List.map pks ~f:(fun pk ->
        PrePacket.modify pk f v))
    )

  let variants (dom : t) : Packet.t list =
    pre_variants dom
    |> List.map ~f:(fun pk -> Packet.Pk pk)
    |> List.cons (Packet.Emptyset)

  let pp fmt (dom:t) : unit =
    Format.fprintf fmt "@[";
    if Map.is_empty dom then Format.fprintf fmt "*@ " else
    Map.iteri dom ~f:(fun ~key ~data -> Format.fprintf fmt "@[%s=%s@]@ "
      (Field.to_string key)
      begin
        Set.to_list data
        |> List.to_string ~f:(function
          | PrePacket.Atom -> "*"
          | PrePacket.Const v -> Int.to_string v)
      end
    );
    Format.fprintf fmt "@]"

  let to_string dom : string =
    Format.asprintf "%a" pp dom

end





(** packet coding *)
type 'domain_witness hyperpoint = int list
type 'domain_witness codepoint = int
type 'domain_witness index = { i : int }  [@@unboxed]
type 'domain_witness index0 = { i : int } [@@unboxed]

module type DOM = sig
  val domain : Domain.t
end

module type CODING = sig
  type domain_witness
  val dom : Domain.t
  val print : unit -> unit

  (** Encoding of packet in n dimensional space.
      More precisely, a packet is encoded as a point in a hypercube, with the
      coordinates being of type int.
      If [dimension] = {k1, ..., kn}, then the hypercube is given by
        {0, ..., k1} x ... x {0, ..., kn}.
      The points within this cube are represented as lists, rather than tuples,
      because n is not known at compile time.
  *)
  module rec Hyperpoint : sig
    type t = domain_witness hyperpoint
    val dimension : int list
    val to_codepoint : t -> Codepoint.t
    (* codepoints can represent the empty set, but hyperpoints cannot *)
    val of_codepoint : Codepoint.t -> t option
    val to_pre_pk : t -> PrePacket.t
    val of_pre_pk : PrePacket.t -> t
  end

  (** Encoding of packets as integers >= 0, i.e. points in single dimensional space. *)
  and Codepoint : sig
    type t = domain_witness codepoint
    val max : t
    val emptyset : t
    (* option, because the empty set is not a hyperpoint *)
    val to_hyperpoint : t -> Hyperpoint.t option
    val of_hyperpoint : Hyperpoint.t -> t
    val to_pk : t -> Packet.t
    val of_pk : Packet.t -> t
    val to_index : t -> Index.t
    val of_index : Index.t -> t
    val to_index0 : t -> Index0.t
    val of_index0 : Index0.t -> t
  end

  (** Encoding of packets as strictly positive integers, i.e. 1-based matrix indices. *)
  and Index : sig
    type t = domain_witness index
    val max : t
    val emptyset : t
    val of_pk : Packet.t -> t
    val to_pk : t -> Packet.t
    (* val test : Field.t -> PrePacket.nomval -> t -> bool *)
    val modify : Field.t -> PrePacket.nomval -> t -> t
    (* val test' : Field.t -> PrePacket.nomval -> int -> bool *)
    val modify' : Field.t -> PrePacket.nomval -> int -> int
    val pp : Format.formatter -> t -> unit
    val pp' : Format.formatter -> int -> unit
  end

  (** Encoding of packets as positive integers (including 0), i.e. 0-based matrix indices. *)
  and Index0 : sig
    type t = domain_witness index0
    val max : t
    val emptyset : t
    val of_pk : Packet.t -> t
    val to_pk : t -> Packet.t
    (* val test : Field.t -> PrePacket.nomval -> t -> bool *)
    val modify : Field.t -> PrePacket.nomval -> t -> t
    (* val test' : Field.t -> PrePacket.nomval -> int -> bool *)
    val modify' : Field.t -> PrePacket.nomval -> int -> int
    val pp : Format.formatter -> t -> unit
    val pp' : Format.formatter -> int -> unit
  end
end

module Coding(D : DOM)() : CODING = struct

  let dom : Domain.t = D.domain
  let domain : (Field.t * PrePacket.nomval list) list =
    Map.to_alist (Map.map D.domain ~f:Set.to_list)

  type domain_witness

  module Hyperpoint = struct
    type t = domain_witness hyperpoint

    let dimension =
      List.map domain ~f:(fun (_,vs) -> List.length vs)

    let injection : (Field.t * (PrePacket.nomval -> int)) list =
      List.Assoc.map domain ~f:(fun vs ->
        List.mapi vs ~f:(fun i v -> (v, i))
        |> Map.Poly.of_alist_exn
        |> Map.Poly.find_exn)

    let ejection : (Field.t * (int -> PrePacket.nomval)) list =
      List.Assoc.map domain ~f:List.to_array
      |> List.Assoc.map ~f:(fun inj v -> inj.(v))


    let to_codepoint t =
      (* SJS: +1 for empty set *)
      1 + List.fold2_exn t dimension ~init:0 ~f:(fun cp v n -> v + n * cp)

    let of_codepoint cp =
      (* SJS: 0 = empty set *)
      if cp = 0 then None else
      List.fold_right dimension ~init:(cp-1,[]) ~f:(fun n (cp, hp) ->
        let (cp, v) = Int.(cp /% n, cp % n) in
        (cp, v::hp))
      |> snd
      |> Option.some

    let to_pre_pk t =
      List.fold2_exn t ejection ~init:Field.Map.empty ~f:(fun pk v (f, veject) ->
        Field.Map.set pk ~key:f ~data:(veject v))

    let of_pre_pk pk =
      List.map injection ~f:(fun (f, vinj) -> vinj (Field.Map.find_exn pk f))

  end

  module Codepoint = struct
    type t = domain_witness codepoint
    let emptyset = 0
    let to_hyperpoint = Hyperpoint.of_codepoint
    let of_hyperpoint = Hyperpoint.to_codepoint
    let to_pre_pk cp = Option.(to_hyperpoint cp >>| Hyperpoint.to_pre_pk)
    let of_pre_pk = Fn.compose of_hyperpoint Hyperpoint.of_pre_pk
    let to_pk cp = if cp = emptyset then Packet.Emptyset else
      Packet.Pk (Option.value_exn (to_pre_pk cp))
    let of_pk pk = match pk with
      | Packet.Emptyset -> emptyset
      | Packet.Pk pk -> of_pre_pk pk
    let max = List.fold ~init:1 ~f:( * ) Hyperpoint.dimension
    let to_index cp : domain_witness index = { i = cp + 1  }
    let of_index (idx : domain_witness index) = idx.i - 1
    let to_index0 cp : domain_witness index0 = { i = cp }
    let of_index0 (idx : domain_witness index0) = idx.i
  end

  module Index = struct
    type t = domain_witness index
    let of_pk = Fn.compose Codepoint.to_index Codepoint.of_pk
    let to_pk = Fn.compose Codepoint.to_pk Codepoint.of_index
    let max = Codepoint.(to_index max)
    let emptyset = Codepoint.(to_index emptyset)
    (* let test f n t = Packet.test f n (to_pk t) *)
    let modify f n t = of_pk (Packet.modify (to_pk t) f n)
    (* let test' f n i = test f n { i = i } *)
    let modify' f n i = (modify f n { i = i }).i
    let pp fmt t = Packet.pp fmt (to_pk t)
    let pp' fmt i = Packet.pp fmt (to_pk { i = i })
  end

  module Index0 = struct
    type t = domain_witness index0
    let of_pk = Fn.compose Codepoint.to_index0 Codepoint.of_pk
    let to_pk = Fn.compose Codepoint.to_pk Codepoint.of_index0
    let max = Codepoint.(to_index0 max)
    let emptyset = Codepoint.(to_index0 emptyset)
    (* let test f n t = Packet.test f n (to_pk t) *)
    let modify f n t = of_pk (Packet.modify (to_pk t) f n)
    (* let test' f n i = test f n { i = i } *)
    let modify' f n i = (modify f n { i = i }).i
    let pp fmt t = Packet.pp fmt (to_pk t)
    let pp' fmt i = Packet.pp fmt (to_pk { i = i })
  end

  let print () = begin
    let fmt = Format.std_formatter in
    let fprintf = Format.fprintf in
    let n = Domain.size dom in
    fprintf fmt "domain size = %d\n" n;
    if n < 50 then begin
      fprintf fmt "index packet mapping:\n%!";
      Array.init n ~f:ident
      |> Array.iter ~f:(fun i -> fprintf fmt " %d = %a\n%!" i Index0.pp' i);
      fprintf fmt "\n%!";
    end
  end

end




module Fdd0 = struct
  include Fdd00

  let iter_maplets fdd ~dom ~(f : (Domain.t * Action.t * Prob.t) -> unit) : unit =
    let rec of_node fdd dom =
      match unget fdd with
      | Leaf r ->
        of_leaf r dom
      | Branch {test=(f,v); tru; fls} ->
        let v = PrePacket.Const v in
        let tru_dom = Map.set dom ~key:f ~data:Domain.Valset.(singleton v) in
        let fls_dom = Map.update dom f ~f:(function
          | None -> assert false
          | Some vs -> Set.remove vs v)
        in
        of_node tru tru_dom;
        of_node fls fls_dom
    and of_leaf dist dom =
      FactorizedActionDist.to_alist dist
      |> List.iter ~f:(fun (act, prob) -> f (dom, act, prob))
    in
    of_node fdd dom
end




(** matrix representation of Fdd0 *)
module Matrix = struct
  type t = {
    matrix : Sparse.mat;
    coding : (module CODING);
    dom : Domain.t;
  }

  let pre_packet_variants (pk : PrePacket.t) (dom : Domain.t) : PrePacket.t list =
    Field.Map.fold2 pk dom ~init:[pk] ~f:(fun ~key:f ~data pks ->
      match data with
      | `Both _ -> pks
      | `Left _ -> assert false
      | `Right vs -> List.concat_map pks ~f:(fun pk ->
          Set.to_list vs
          |> List.map ~f:(fun v -> Field.Map.set pk ~key:f ~data:v)
        )
    )
  (* like pre_packet_variants, but only returns a singel variant *)
  let pre_packet_variant (pk : PrePacket.t) (dom : Domain.t) : PrePacket.t =
    Field.Map.fold2 pk dom ~init:pk ~f:(fun ~key:f ~data pk ->
      match data with
      | `Both _ -> pk
      | `Right vs -> Field.Map.set pk ~key:f ~data:(Set.choose_exn vs)
      | `Left _ -> assert false
    )

  let packet_variants (pk : Packet.t) (dom : Domain.t) : Packet.t list =
    match pk with
    | Emptyset -> [pk]
    | Packet.Pk pk ->
      pre_packet_variants pk dom
      |> List.map ~f:(fun pk -> Packet.Pk pk)

  let maplet_to_matrix_entries (coding : (module CODING)) (dom, act, prob)
    : (int * int * Prob.t) list =
    let module Conv = (val coding : CODING) in
    Domain.pre_variants dom
    |> List.map ~f:(fun pk ->
      let pk = Packet.Pk pk in
      let pk' = Packet.apply pk act in
      ((Conv.Index0.of_pk pk).i, (Conv.Index0.of_pk pk').i, prob)
    )

  let iter_fdd_entries fdd (coding : (module CODING)) ~f =
    let module Coding = (val coding : CODING) in
    let dom = Coding.dom in
    Fdd0.iter_maplets fdd ~dom ~f:(fun maplet ->
      maplet_to_matrix_entries coding maplet
      |> List.iter ~f
    )


  let get_pk_action t (pk : PrePacket.t) : ActionDist.t =
    let module Coding = (val t.coding : CODING) in
    let row_to_action row : ActionDist.t =
      Sparse.foldi_nz row ~init:ActionDist.empty ~f:(fun _i j dist prob ->
        Coding.Index0.to_pk { i = j }
        |> Packet.to_action
        |> ActionDist.unsafe_add dist (Prob.of_float prob)
      )
      |> ActionDist.unsafe_normalize
    in
    let total_pk_action pk : ActionDist.t =
      let i = (Coding.Index0.of_pk (Packet.Pk pk)).i in
      let rowi = Sparse.row t.matrix i in
      row_to_action rowi
    in
    (* Using faster, but less safe code. We're assuming here that all
       pre_pact_variants give the same final result. But we're not checking that
       this is true. *)
    pre_packet_variant pk t.dom
    |> total_pk_action
(*     (* FIXME: inefficient solution for now for safety! *)
    pre_packet_variants pk t.dom
    |> List.map ~f:total_pk_action
    |> List.group ~break:(fun x y -> not (ActionDist.equal x y))
    |> function
      | [act::_] -> act
      | _ -> assert false *)

end



module Fdd = struct

  include Fdd0
  open Syntax


  let to_dotfile t filename =
    Out_channel.with_file ~append:false ~fail_if_exists:false filename ~f:(fun chan ->
      Out_channel.output_string chan (to_dot t)
    )

  let render ?(format="pdf") ?(title="FDD") t =
    Util.show_dot ~format ~title (to_dot t)

  (** SJS: keep global string |-> Field.t map so that we can compare policies
      that are compiled to Fdds one after another
  *)
  let field_allocation_tbl : (string, Field.t) Hashtbl.t = String.Table.create ()
  let next_field = ref 0

  let abstract_field name =
    Hashtbl.find_exn field_allocation_tbl name



  (* sound, but could be better *)
  let clear_cache ~(preserve:Int.Set.t) : unit =
    clear_cache ~preserve;
    if Set.is_empty preserve then (
      Hashtbl.clear field_allocation_tbl;
      next_field := 0;
      Hashtbl.clear Field.field_to_str_map;
    );
    ()

  let allocate_field env (f : string) : Field.t =
    match Field.Env.lookup env f with
    | (field, _) -> field
    | exception Caml.Not_found ->
      String.Table.find_or_add field_allocation_tbl f ~default:(fun () ->
        let open Field in
        let field = match !next_field with
          | 0 -> F0
          | 1 -> F1
          | 2 -> F2
          | 3 -> F3
          | 4 -> F4
          | 5 -> F5
          | 6 -> F6
          | 7 -> F7
          | 8 -> F8
          | 9 -> F9
          | 10 -> F10
          | 11 -> F11
          | 12 -> F12
          | 13 -> F13
          | 14 -> F14
          | 15 -> F15
          | 16 -> F16
          | 17 -> F17
          | 18 -> F18
          | 19 -> F19
          | _ -> failwith "too many fields! (may need to clear the cache?)"
        in
        incr next_field;
        Hashtbl.add_exn Field.field_to_str_map field f;
        field
      )

  let allocate_global_field = allocate_field Field.Env.empty

  let allocate_fields (pol : string policy) : Field.t policy =
    let rec do_pol env (p : string policy) : Field.t policy =
      match p with
      | Filter pred ->
        Filter (do_pred env pred)
      | Modify (f,v) ->
        Modify (allocate_field env f, v)
      | Seq (p, q) ->
        Seq (do_pol env p, do_pol env q)
      | Ite (a, p, q) ->
        Ite (do_pred env a, do_pol env p, do_pol env q)
      | Branch {branches; parallelize} ->
        let branches =
          List.map branches ~f:(fun (a,p) -> (do_pred env a, do_pol env p)) in
        Branch { branches; parallelize }
      | While (a, p) ->
        While (do_pred env a, do_pol env p)
      | ObserveUpon (p, a) ->
        ObserveUpon (do_pol env p, do_pred env a)
      | Choice dist ->
        Choice (Util.map_fst dist ~f:(do_pol env))
      | Let { id; init; mut; body; } ->
        let init = match init with
          | Alias f -> Alias (allocate_field env f)
          | Const v -> Const v
        in
        let env = Field.Env.add env id init mut in
        let (id,_) = Field.Env.lookup env id in
        let body = do_pol env body in
        Let { id; init; mut; body; }
(*       | Repeat (n, p) ->
        Repeat (n, do_pol env p) *)
    and do_pred env (p : string pred) : Field.t pred =
      match p with
      | True -> True
      | False -> False
      | Test (f, v) -> Test (allocate_field env f, v)
      | And (p, q) -> And (do_pred env p, do_pred env q)
      | Or (p, q) -> Or (do_pred env p, do_pred env q)
      | Neg p -> Neg (do_pred env p)
    in
    let pol = do_pol Field.Env.empty pol in
    pol

  let deallocate_fields_pred (pred : Field.t pred) : string pred =
    let do_field : Field.t -> string = Field.to_string in
    let rec do_pred a =
      match a with
      | True -> True
      | False -> False
      | Test (f, v) -> Test (do_field f, v)
      | And (p, q) -> And (do_pred p, do_pred q)
      | Or (p, q) -> Or (do_pred p, do_pred q)
      | Neg p -> Neg (do_pred p)
    in
    do_pred pred

  let deallocate_fields (pol : Field.t policy) : string policy =
    let do_field : Field.t -> string = Field.to_string in
    let do_pred = deallocate_fields_pred in
    let rec do_pol p =
      match p with
      | Filter pred ->
        Filter (do_pred pred)
      | Modify (f,v) ->
        Modify (do_field f, v)
      | Seq (p, q) ->
        Seq (do_pol p, do_pol q)
      | Ite (a, p, q) ->
        Ite (do_pred a, do_pol p, do_pol q)
      | Branch {branches; parallelize} ->
        let branches =
          List.map branches ~f:(fun (a,p) -> (do_pred a, do_pol p)) in
        Branch { branches; parallelize }
      | While (a, p) ->
        While (do_pred a, do_pol p)
      | ObserveUpon (p, a) ->
        ObserveUpon (do_pol p, do_pred a)
      | Choice dist ->
        Choice (Util.map_fst dist ~f:(do_pol))
      | Let { id; init; mut; body; } ->
        let init = match init with
          | Alias f -> Alias (do_field f)
          | Const v -> Const v
        in
        let id = "<N/A>" in
        let body = do_pol body in
        Let { id; init; mut; body; }
    in
    do_pol pol


(*===========================================================================*)
(* EQUIVALENCE                                                               *)
(*===========================================================================*)

  type weighted_pk = Packet.t * Prob.t [@@deriving compare, eq]

  (** removes all modifications of the provied list of fields  *)
  let modulo t fields : t =
    (* translate strings to Field.t's *)
    let fields =
      List.filter_map fields ~f:(Hashtbl.find field_allocation_tbl)
      |> Field.Set.of_list
    in
    let open Action in
    dp_map_r t ~f:(fun dist ->
      FactorizedActionDist.pushforward dist ~f:(function
      | Drop ->
        Drop
      | Action act ->
        Action (Field.Map.filteri act ~f:(fun ~key:f ~data:_->
          not (Set.mem fields f)
        ))
      )
    )

  type ternary = TTrue | TFalse | TMaybe

  let less_than ?(modulo=[]) t1 t2 =
    let (&&) t1 t2 = match t1, t2 with
      | TMaybe, TMaybe -> TMaybe
      | TFalse, _  | _, TFalse -> TFalse
      | _, TTrue | TTrue, _ -> TTrue in
    let modulo =
      List.filter_map modulo ~f:(Hashtbl.find field_allocation_tbl)
      |> Field.Set.of_list
    in
    let rec do_nodes t1 t2 pk =
      match unget t1, unget t2 with
      | Branch {test=(f1,v1); tru=l1; fls=r1; all_fls=all_fls_1},
        Branch {test=(f2,v2); tru=l2; fls=r2; all_fls=all_fls_2} ->
        begin match Field.compare f1 f2 with
        | -1 ->
          do_nodes l1 t2 (PrePacket.modify pk f1 (Const v1)) &&
          do_nodes r1 t2 pk
        | 1 ->
          do_nodes t1 l2 (PrePacket.modify pk f2 (Const v2)) &&
          do_nodes t1 r2 pk
        | 0 ->
          begin match Value.compare v1 v2 with
          | 0 ->
            do_nodes l1 l2 (PrePacket.modify pk f1 (Const v1)) &&
            do_nodes r1 r2 pk
          | -1 ->
            do_nodes l1 all_fls_2 (PrePacket.modify pk f1 (Const v1)) &&
            do_nodes r1 t2 pk
          | 1 ->
            do_nodes all_fls_1 l2 (PrePacket.modify pk f2 (Const v2)) &&
            do_nodes t1 r2 pk
          | _ -> assert false
          end
        | _ -> assert false
        end
      | Branch {test=(f1,v1); tru=l1; fls=r1}, Leaf _ ->
        do_nodes l1 t2 (PrePacket.modify pk f1 (Const v1)) &&
        do_nodes r1 t2 pk
      | Leaf _, Branch {test=(f2,v2); tru=l2; fls=r2} ->
        do_nodes t1 l2 (PrePacket.modify pk f2 (Const v2)) &&
        do_nodes t1 r2 pk
      | Leaf d1, Leaf d2 ->
        do_leaves d1 d2 pk
    and do_leaves d1 d2 pk =
      match List.compare compare_weighted_pk (normalize d1 pk) (normalize d2 pk) with
      | -1 -> TTrue
      | 0 -> TMaybe
      | _ -> TFalse
    and normalize dist pk =
      modulo_mods dist
      |> FactorizedActionDist.to_alist
      |> Util.map_fst ~f:(Packet.(apply (Pk pk)))
      |> List.filter ~f:(fun (pk,_) -> not Packet.(equal pk Emptyset))
      |> List.sort ~compare:compare_weighted_pk
    and modulo_mods dist =
      FactorizedActionDist.pushforward dist ~f:(function
      | Drop ->
        Drop
      | Action act ->
        Action (Field.Map.filteri act ~f:(fun ~key:f ~data:_->
          not (Set.mem modulo f)
        ))
      )
    in
    match do_nodes t1 t2 PrePacket.empty with
    | TTrue -> true
    | _ -> false


  let equivalent ?(modulo=[]) t1 t2 =
    let modulo =
      List.filter_map modulo ~f:(Hashtbl.find field_allocation_tbl)
      |> Field.Set.of_list
    in
    let rec do_nodes t1 t2 pk =
      match unget t1, unget t2 with
      | Branch {test=(f1,v1); tru=l1; fls=r1; all_fls=all_fls_1},
        Branch {test=(f2,v2); tru=l2; fls=r2; all_fls=all_fls_2} ->
        begin match Field.compare f1 f2 with
        | -1 ->
          do_nodes l1 t2 (PrePacket.modify pk f1 (Const v1)) &&
          do_nodes r1 t2 pk
        | 1 ->
          do_nodes t1 l2 (PrePacket.modify pk f2 (Const v2)) &&
          do_nodes t1 r2 pk
        | 0 ->
          begin match Value.compare v1 v2 with
          | 0 ->
            do_nodes l1 l2 (PrePacket.modify pk f1 (Const v1)) &&
            do_nodes r1 r2 pk
          | -1 ->
            do_nodes l1 all_fls_2 (PrePacket.modify pk f1 (Const v1)) &&
            do_nodes r1 t2 pk
          | 1 ->
            do_nodes all_fls_1 l2 (PrePacket.modify pk f2 (Const v2)) &&
            do_nodes t1 r2 pk
          | _ -> assert false
          end
        | _ -> assert false
        end
      | Branch {test=(f1,v1); tru=l1; fls=r1}, Leaf _ ->
        do_nodes l1 t2 (PrePacket.modify pk f1 (Const v1)) &&
        do_nodes r1 t2 pk
      | Leaf _, Branch {test=(f2,v2); tru=l2; fls=r2} ->
        do_nodes t1 l2 (PrePacket.modify pk f2 (Const v2)) &&
        do_nodes t1 r2 pk
      | Leaf d1, Leaf d2 ->
        do_leaves d1 d2 pk
    and do_leaves d1 d2 pk =
      List.equal ~equal:equal_weighted_pk (normalize d1 pk) (normalize d2 pk)
    and normalize dist pk =
      modulo_mods dist
      |> FactorizedActionDist.to_alist
      |> Util.map_fst ~f:(Packet.(apply (Pk pk)))
      |> List.sort ~compare:compare_weighted_pk
    and modulo_mods dist =
      FactorizedActionDist.pushforward dist ~f:(function
      | Drop ->
        Drop
      | Action act ->
        Action (Field.Map.filteri act ~f:(fun ~key:f ~data:_->
          not (Set.mem modulo f)
        ))
      )
    in
    do_nodes t1 t2 PrePacket.empty



(*===========================================================================*)
(* COMPILATION                                                               *)
(*===========================================================================*)

  let of_test hv =
    atom hv FactorizedActionDist.one FactorizedActionDist.zero

  let of_test hv = measure "of_test" (fun () -> of_test hv)

  let of_mod (f,v) =
    const (FactorizedActionDist.dirac (Action.Action (PreAction.T.singleton f v)))

  let of_mod hv = measure "of_mod" (fun () -> of_mod hv)

  let negate fdd =
    dp_map_r fdd ~f:FactorizedActionDist.negate

  let rec of_pred p =
    match p with
    | True      -> id
    | False     -> drop
    | Test(hv)  -> of_test hv
    | And(p, q) -> prod (of_pred p) (of_pred q)
    | Or (p, q) -> sum (of_pred p) (of_pred q)
    | Neg(q)    -> negate (of_pred q)

  let of_pred a = measure "of_pred" (fun () -> of_pred a)


  (* SJS: copied and adopted apply algorithm from Vlr.ml *)
  let convex_sum t1 prob t2 : t =
    Hashtbl.clear binary_cache;
    (* the function to apply at the leaves *)
    let f x y = FactorizedActionDist.convex_sum x prob y in
    let rec sum x y =
      let result = BinTbl.find_or_add binary_cache (x, y) ~default:(fun () -> sum' x y) in
(*       printf "%s-convex combination of\n  %s\nand\n  %s\nis\n  %s\n\n%!"
        (Prob.to_string prob)
        (to_string x)
        (to_string y)
        (to_string result); *)
      result
    and sum' x y =
      match unget x, unget y with
      | Leaf x, _      ->
        dp_map_r (fun y -> f x y) y
      | _     , Leaf y ->
        dp_map_r (fun x -> f x y) x
      | Branch {test=(vx, lx); tru=tx; fls=fx; all_fls=all_fls_x},
        Branch {test=(vy, ly); tru=ty; fls=fy; all_fls=all_fls_y} ->
        begin match Field.compare vx vy with
        |  0 ->
          begin match Value.compare lx ly with
          |  0 -> unchecked_cond (vx,lx) (sum tx ty) (sum fx fy)
          | -1 -> unchecked_cond (vx,lx) (sum tx all_fls_y) (sum fx y)
          |  1 -> unchecked_cond (vy,ly) (sum all_fls_x ty) (sum x fy)
          |  _ -> assert false
          end
        | -1 -> unchecked_cond (vx,lx) (sum tx y) (sum fx y)
        |  1 -> unchecked_cond (vy,ly) (sum x ty) (sum x fy)
        |  _ -> assert false
        end
    in
    if Prob.(equal zero prob) then t2 else
    if Prob.(equal one prob) then t1 else
    sum t1 t2

  let convex_sum t1 p t2 = measure "convex sum" (fun () -> convex_sum t1 p t2)

  let n_ary_convex_sum xs : t =
    let xs =
      List.filter xs ~f:(fun (_, p) -> Prob.(p>zero))
      |> Array.of_list
    in
    (* responsible for k with i <= k < j *)
    let rec devide_and_conquer i j =
      (* number of elements responible for *)
      let n = j - i in
      match n with
      | 0 ->
        (drop, Prob.zero)
      | 1 ->
        xs.(i)
      | _ ->
        assert (n >= 2);
        let k = i + n/2 in
        let (t1,p1) = devide_and_conquer i k in
        let (t2,p2) = devide_and_conquer k j in
        let mass = Prob.(p1 + p2) in
        let p = Prob.(p1 / mass) in
        (convex_sum t1 p t2, mass)
    in
    let (t, mass) = devide_and_conquer 0 (Array.length xs) in
    (* assert Prob.(equal mass one); *)
    t

  let n_ary_convex_sum xs = measure "n-ary convex sum" (fun () -> n_ary_convex_sum xs)

  (* SJS: cannot reuse binary_cache, since seq calls n_ary_convex sum *)
  let seq_cache = BinTbl.create ~size:10000 ()

  (** sequence of FactorizedActionDist.t and t  *)
  let rec dist_seq dist u =
    let t = const dist in
    if equal t drop then drop else
    if equal t id  then u else
    BinTbl.find_or_add seq_cache (t,u) ~default:(fun () ->
      match unget u with
      | Leaf dist' ->
        const (FactorizedActionDist.prod dist dist')
      | Branch {test; tru; fls} ->
        let ((yes,p_yes), (no, p_no), (maybe, p_maybe)) =
          FactorizedActionDist.split_into_conditionals dist test
        in
        n_ary_convex_sum [
          dist_seq yes tru, p_yes;
          dist_seq no fls, p_no;
          unchecked_cond test (dist_seq maybe tru) (dist_seq maybe fls), p_maybe;
        ]
    )

  let dist_seq d u = measure "dist seq" (fun () -> dist_seq d u)



  let seq t u =
    let cond v t f = measure "seq cond" (fun () -> cond v t f) in
    let time, x = Util.time' (fun () ->
      Hashtbl.clear seq_cache;
      match unget u with
      | Leaf _ -> prod t u (* This is an optimization. If [u] is an
                              [Action.Par.t], then it will compose with [t]
                              regardless of however [t] modifies packets. None
                              of the decision variables in [u] need to be
                              removed because there are none. *)
      | Branch _ ->
        dp_map t
          ~f:(fun dist -> dist_seq dist u)
          ~g:(fun v t f -> cond v t f)
          ~find_or_add:(fun t -> BinTbl.find_or_add seq_cache (t,u))
    )
    in
(*     if time >= 0.1 then begin
      printf "Slow sequence:\n";
      printf "t = %f seconds\n" time;
      printf "|t1| = %d\n" (size t);
      printf "|t2| = %d\n" (size u);
      printf "|t1;t2| = %d\n\n" (size x);
      render t;
      render u;
      exit (-1);
    end; *)
    x

  let seq p q = measure "seq" (fun () -> seq p q)


(*   let leaves t : HSet.t =
    let set = HSet.create () in
    let rec do_node t =
      match unget t with
      | Leaf _ -> Hash_set.add set t
      | Branch { tru; fls; _ } -> do_node tru; do_node fls
    in
    do_node t;
    set

  let dist_seq d t =
    FactorizedActionDist.to_joined d
    |> ActionDist.to_alist
    |> Util.map_fst ~f:(function
      | Drop -> drop
      | Action act as a ->
        restrict (Map.to_alist act) t
        |> map_r ~f:(fun fad -> FactorizedActionDist.(prod (dirac a) fad))
    )
    |> n_ary_convex_sum_t

  let seq (t : t) (u : t) =
    leaves t
    |> Hash_set.to_list
    |> List.map ~f:(fun leaf ->
      let t_leave = dp_fold t
        ~f:(fun r -> if equal (const r) leaf then id else drop)
        ~g:unchecked_cond
      in
      let Leaf dist = unget leaf in
      prod t_leave (dist_seq dist u)
    )
    |> List.fold ~init:drop ~f:sum *)


  (* FIXME: this skeleton is very large, so this may become a bottleneck.
     I used to use a smaller skeleton computed from ap and not_a, but I am not
     sure this was sound. Reconsider should this become a bottleneck in the
     future. *)
  let mk_skeleton dom =
    Field.Map.fold dom ~init:id ~f:(fun ~key:field ~data:vs init ->
      Set.fold vs ~init ~f:(fun init v ->
        match v with
        | PrePacket.Atom -> init
        | PrePacket.Const v -> cond (field,v) init (negate init)
      )
    )

  let of_mat (matrix : Matrix.t) : t =
    let skeleton = mk_skeleton Matrix.(matrix.dom) in
    let rec do_node skeleton pk =
      match unget skeleton with
      | Leaf r ->
        const (Matrix.get_pk_action matrix pk |> FactorizedActionDist.factorize)
      | Branch {test=(f,v); tru; fls} ->
        let tru = do_node tru PrePacket.(modify pk f (Const v)) in
        let fls = do_node fls PrePacket.(modify pk f Atom) in
        unchecked_cond (f,v) tru fls
    in
    do_node skeleton PrePacket.empty

  let ocaml_iterate ap not_a =
    let rec loop p =
      let p2 = seq p p in
      if equal p p2 then p else loop p2
    in
    seq (loop (sum ap not_a)) not_a

  let bounded_whl k a p =
    let ap = prod a p in
    let not_a = negate a in
    let rec loop k p =
      if k <= 1 then p else
      let p2 = seq p p in
      if equal p p2 then p else loop (k/2) p2
    in
    seq (loop k (sum ap not_a)) not_a

  let bounded_whl_cps k lctxt a ap =
    let not_a = negate a in
    let q = sum ap not_a in
    let rec loop k p =
      if k <= 0 then p else
      let p2 = seq p q in
      if equal p p2 then p else loop (k - 1) p2
    in
    seq (loop k lctxt) not_a


  let python_iterate ap not_a (coding : (module CODING)) to_py =
    let module Coding = (val coding : CODING) in
    let n = Domain.size Coding.dom in

    let send_matrix_entry (i,j,p) : unit =
      Prob.to_float p
      |> Float.to_string
      |> Out_channel.fprintf to_py "%d %d %s\n" i j
    in

    (* serialize matrices and send it to python process *)
    let send_matrix fdd =
      Out_channel.fprintf to_py "%d %d\n" n n;
      Matrix.iter_fdd_entries fdd coding ~f:send_matrix_entry;
      (* SJS: although it is implicit in the FDD, the emptyset transitions with
         probability 1 to the emptyset
      *)
      let emptyset = Coding.Index0.emptyset in
      send_matrix_entry (emptyset.i, emptyset.i, Prob.one);
      Out_channel.fprintf to_py "\n%!";
    in

    Util.timed "sending fdds as matrices to python" (fun () ->
      Util.timed "sending ap" (fun () -> send_matrix ap);
      Util.timed "sending not_a" (fun () -> send_matrix not_a);
      Out_channel.close to_py
    )


  let try_naive_fixedpoint = ref true

  (*      X = (AP)*¬A
    Thus  X = ¬A + (AP)X
     <=>  (I-AP)X = ¬A
     We are looking for X. We solve the linear (sparse) system to compute it.
  *)
  let whl a p =
    (* printf "a = %s\n" (to_string a); *)
    (* transition matrix for transient states, i.e. those satisfying predicate [a] *)
    let ap = prod a p in

    (* transition matrix for absorbing states, i.e. those not satisfying [a] *)
    let not_a = negate a in

    (* optimize special case where a single iteration suffices *)
    if equal drop ap || equal drop (seq ap a) then sum ap not_a else

    (* printf "ap = %s\n%!" (to_string ap); *)
    (* printf "not_a = %s\n%!" (to_string not_a); *)

    (* compute domain of FDDs; i.e., how many indices do we need for the matrix
       representation and what does each index preresent?
    *)
    let dom = Domain.(merge (of_fdd ap) (of_fdd not_a)) in
    let module Coding = Coding(struct let domain = dom end)() in
    let coding = (module Coding : CODING) in

    (* setup external python script to solve linear system, start in seperate process *)
    let pkg_name = "probnetkat" in
    let script_name = "absorption.pyc" in
    let pyscript = match Findlib.package_directory pkg_name with
      | dir ->
        dir ^ "/" ^ script_name
      | exception Findlib.No_such_package _ ->
        failwith ("missing ocamlfind dependency: " ^ pkg_name)
    in
    let cmd = "python3 " ^ pyscript in
    let (from_py, to_py) as py = Unix.open_process cmd in

    (* pipe for communicating with OCaml child that will try to compute fixpoint *)
    let (from_caml_fd, to_parent_fd) = Unix.pipe () in
    let from_caml = Unix.in_channel_of_descr from_caml_fd in
    let to_parent = Unix.out_channel_of_descr to_parent_fd in


    (* try computing naive fixed-point and analytical fixed-point in parallel *)
    let py_pid = Util.fork (fun () -> python_iterate ap not_a coding to_py) in
    let caml_pid = if not (!try_naive_fixedpoint) then None else Some (
      Util.fork (fun () ->
        let fixpoint = Util.timed "naive fixpoint" (fun () ->
          clear_cache ~preserve:(Int.Set.of_list ([ap; not_a] : t list :> int list));
          ocaml_iterate ap not_a
        )
        in
        Util.timed "serializing & sending Fdd" (fun () ->
          Out_channel.output_lines to_parent [serialize fixpoint]
        )
      )
    )
    in

    (* wait for first result to become available, Python or OCaml *)
    protect ~finally:(fun () ->
      Signal.(send_i kill (`Pid py_pid));
      begin match caml_pid with
      | Some pid -> Signal.(send_i kill (`Pid pid))
      | None -> ()
      end;
      ignore (Unix.close_process py);
      In_channel.close from_caml;
      Out_channel.close to_parent;
      ignore (Unix.waitpid py_pid);
      begin match caml_pid with
      | Some pid -> ignore (Unix.waitpid pid)
      | None -> ()
      end;
    ) ~f:(fun () ->
      let from_py_fd = Unix.descr_of_in_channel from_py in
      match
        Unix.select ~restart:true ~read:[from_py_fd; from_caml_fd]
          ~write:[] ~except:[] ~timeout:`Never ()
      with
      | { read = fd::_; _ } when Unix.File_descr.equal fd from_caml_fd ->
        printf "*** ocaml won race!\n%!";

        (* kill other process *)
        Signal.(send_i kill (`Pid py_pid));

        Util.timed "deserializing & receiving Fdd" (fun () ->
          In_channel.input_line_exn from_caml
          |> deserialize
        )
      | { read = fd::_; _ } when Unix.File_descr.equal fd from_py_fd ->
        printf "*** python won race!\n%!";

        (* kill other process *)
        begin match caml_pid with
        | Some pid -> Signal.(send_i kill (`Pid pid))
        | None -> ()
        end;

        (* wait for reply from Python *)
        let n = Domain.size dom in
        let iterated = Sparse.zeros n n in
        let line () = In_channel.input_line_exn from_py in

        Util.timed "receive result from python" (fun () ->
          begin try
            let (m,n') = String.lsplit2_exn (line ()) ~on:' ' in
            let (m,n') = Int.(of_string m, of_string n') in
            if m <> n || n' <> n then failwith "no bueno"
          with
            | End_of_file -> failwith "python process closed prematurely."
            | _ -> failwith "malformed first output line"
          end;

          begin try while true do
            match String.split (line ()) ~on:' ' with
            | [i; j; v] ->
              let i,j = Int.(of_string i, of_string j) in
              assert (0 <= i && i < n);
              assert (0 <= j && j < n);
              let v = Float.of_string v in
              Sparse.set iterated i j v
            | _ ->
              failwith "malformed output line"
          done with
            | End_of_file -> ()
            | _ -> failwith "malformed output line"
          end;
        );

        (* convert matrix back to FDD *)
        let iterated = Matrix.{ matrix = iterated; dom; coding } in
        Util.timed "matrix -> fdd conversion" (fun () -> of_mat iterated)
      | _ ->
        failwith "unexpected behavior"
    )

  let whl a p = measure "whl" (fun () -> whl a p)


  (* buggy, for some reason *)
(*   let repeat n p =
    let rec loop i p_2n acc =
      if i = 1 then
        List.fold acc ~init:p_2n ~f:seq
      else
        loop (i/2) (seq p_2n p_2n) (if i%2 = 0 then acc else p_2n :: acc)
    in
    if n <= 0 then id else loop n p [] *)

  (** Erases (all matches on) meta field, then all modifications. *)
  let erase t meta_field init =
    let erase_mods =
      FactorizedActionDist.pushforward ~f:(function
        | Drop -> Drop
        | Action act -> Action (PreAction.T.remove act meta_field))
    in
    match init with
    | Const v ->
      restrict [(meta_field,v)] t
      |> dp_map_r ~f:erase_mods
    | Alias alias ->
      dp_fold t
        ~f:(fun dist -> const (erase_mods dist))
        ~g:(fun (field,v) tru fls ->
          if field = meta_field then
            cond (alias, v) tru fls
          else
            cond (field,v) tru fls
        )

  let ite a p q =
    (* SJS: Disjoint union. Ideally, we would have a safe primitive for
       this purpose. Either way, the implementation of sum currently
       enforces that nothing can go wrong here. *)
    sum (prod a p) (prod (negate a) q)


  let use_slow_observe = ref false

  let observe_upon p a =
    if !use_slow_observe then
      seq p (whl (negate a) p)
    else begin
      seq p a
      |> dp_map_r ~f:FactorizedActionDist.observe_not_drop
    end

  let observe_upon p a = measure "obs" (fun () -> observe_upon p a)
  let ite a p q = measure "ite" (fun () -> ite a p q)
  let erase p f init = measure "erase" (fun () -> erase p f init)


let par_branch (bound : int option) branches =
  let pkg_name = "probnetkat" in
  let cmd_name = "compile" in
  let prog = match Findlib.package_directory pkg_name with
    | dir ->
      Format.sprintf "%s/../../bin/%s.%s" dir pkg_name cmd_name
    | exception Findlib.No_such_package _ ->
      failwith ("missing ocamlfind dependency: " ^ pkg_name)
  in
  let order = Field.get_order () |> [%sexp_of: Field.t list] |> Sexp.to_string in
  let args =
    ["-order"; Format.sprintf "%S" order] @
    (match bound with None -> [] | Some b -> ["-bound"; Int.to_string b])
    (* @ ["-j"; Int.to_string Params.j] *)
  in
  let cmd = Format.sprintf "%s %s" prog (String.concat args ~sep:" ") in
  let (from_proc, to_proc) as proc = Unix.open_process cmd in
  List.iter branches ~f:(fun (a,p) ->
    PNK.(filter a >> p)
(*     |> Bin_prot.Utils.bin_dump ~header:true
      (Syntax.bin_writer_policy Field.bin_writer_t)
    |> Bigstring.really_write (Unix.descr_of_out_channel to_proc) *)
    |> [%sexp_of: Field.t policy]
    |> Sexp.to_string
    |> Out_channel.fprintf to_proc "%s\n%!"
  );
  Out_channel.close to_proc;
(*   Bin_prot.Utils.bin_read_stream bin_reader_t ~max_size:1_000_000
    ~read:(fun buf ~pos ~len ->
      Bigstring.really_read (Unix.descr_of_in_channel from_proc )buf ~pos ~len
  ) *)
  In_channel.input_line_exn from_proc
  |> deserialize

let rec of_pol_k bound (p : Field.t policy) k : t =
    match p with
    | Filter a ->
      k (of_pred a)
    | Modify m ->
      k (of_mod m)
    | Seq (p, q) ->
      of_pol_k bound p (fun p' ->
        if equal p' Fdd0.drop then
          k drop
        else
          of_pol_k bound q (fun q' -> k (seq p' q')))
    | Ite (a, p, q) ->
      let a = of_pred a in
      if equal a id then
        of_pol_k bound p k
      else if equal a drop then
        of_pol_k bound q k
      else
        of_pol_k bound p (fun p -> of_pol_k bound q (fun q -> k (ite a p q)))
    | Branch { branches; parallelize = true} ->
      par_branch bound branches
    | Branch {branches} ->
      List.filter_map branches ~f:(fun (a,p) ->
        let a = of_pred a in
        if equal a drop then
          None
        else
          Some (of_pol_k bound p (fun p -> prod a p))
      )
      |> List.fold ~init:drop ~f:sum
      |> k
    | While (a, p) ->
      let a = of_pred a in
      if equal a id then k drop else
      if equal a drop then k id else
      of_pol_k bound p (fun p ->
        k (Util.timed "while loop" (fun () ->
          match bound with
          | None -> whl a p
          | Some k -> bounded_whl k a p
        ))
      )
    | Choice dist ->
      Util.map_fst dist ~f:(fun p -> of_pol_k bound p ident)
      |> n_ary_convex_sum
      |> k
    | Let { id=field; init; mut; body=p } ->
      of_pol_k bound p (fun p -> k (erase p field init))
    | ObserveUpon (p, a) ->
      let a = of_pred a in
      if equal a drop then k drop else
      of_pol_k bound p (fun p -> k (observe_upon p a))


  let rec of_pol_cps bound (lctxt : t) (p : Field.t policy) : t =
    if equal drop lctxt then drop else
    match p with
    | Filter a ->
      seq lctxt (of_pred a)
    | Modify m ->
      prod lctxt (of_mod m)
    | Seq (p,q) ->
      let lctxt = of_pol_cps bound lctxt p in
      of_pol_cps bound lctxt q
    | Ite (a, p, q) ->
      let a = of_pred a in
      sum (of_pol_cps bound a p) (of_pol_cps bound (negate a) q)
      |> seq lctxt
    | Branch {branches} ->
      List.map branches ~f:(fun (a,p) ->
        of_pol_cps bound (of_pred a) p
      )
      |> List.fold ~init:drop ~f:sum
      |> seq lctxt
    | While (a, p) ->
      let a = of_pred a in
      if equal a id then
        drop
      else
        let skip_ctxt = seq lctxt (negate a) in
        if equal lctxt skip_ctxt || equal a drop then
          skip_ctxt
        else
          begin match bound with
          | None -> seq lctxt (whl a (of_pol_cps bound a p))
          | Some k -> bounded_whl_cps k lctxt a (of_pol_cps bound a p)
          end
    | Choice dist ->
      (* SJS: In principle, we could compile all points of the distribution with
         lctxt. But empirically, this leads to much worse performance. *)
      n_ary_convex_sum (Util.map_fst dist ~f:(of_pol_cps bound id))
      |> seq lctxt
    | Let { id=field; init; mut; body=p } ->
      begin match init with
      | Const v ->
        let lctxt = of_pol_cps bound lctxt (Modify (field, v)) in
        let p = of_pol_cps bound lctxt p in
        erase p field init
      | Alias _ ->
        let p = of_pol_cps bound lctxt p in
        erase p field init
      end
    | ObserveUpon (p, a) ->
      let a = of_pred a in
      if equal a drop then drop else
      observe_upon (of_pol_cps bound id p) a
      |> seq lctxt

  let use_cps = ref true

  let of_symbolic_pol ?(lctxt=id) ?(bound=None) (p : Field.t policy) : t =
    if !use_cps then
      of_pol_cps bound lctxt p
    else
      of_pol_k bound p (seq lctxt)

  (* auto_order is set to false by default, so repeated invocations of this
     function don't invalidate old FDDs. *)
  let of_pol ?lctxt ?bound ?(auto_order=false) (p : string policy) : t =
    allocate_fields p
    |> Util.tap ~f:(fun t -> if auto_order then Field.auto_order t)
    |> of_symbolic_pol ?lctxt ?bound


  let output_dist t ~(input_dist : Packet.Dist.t) =
    Packet.Dist.to_alist input_dist
    |> Util.map_fst ~f:(fun pk ->
      seq (const @@ FactorizedActionDist.dirac @@ Packet.to_action pk) t
    )
    |> n_ary_convex_sum
    |> unget
    |> function
      | Fdd0.Branch {test=(f,_)} as fdd ->
        sprintf "Underspecified input distribution. Branching on %s, but input distribution is %s.\n%s\n"
          (Field.to_string f)
          (Packet.Dist.to_string input_dist)
          (to_string (get fdd))
        |> failwith
      | Leaf dist ->
        FactorizedActionDist.to_joined dist
        |> Packet.Dist.of_action_dist


  (* minimum probability of not dropping packet *)
  let min_nondrop_prob t ~(support : Packet.t list) =
    List.map support ~f:(fun pk ->
      seq (const @@ FactorizedActionDist.dirac @@ Packet.to_action pk) t
      |> unget
      |> function
      | Fdd0.Branch _ ->
        failwith "underspecified input support"
      | Leaf dist ->
        FactorizedActionDist.to_alist dist
        |> List.filter_map ~f:(function
            | (Action.Drop,_) -> None
            | (_,p) -> Some p
        )
        |> List.fold ~init:Prob.zero ~f:Prob.(+)
    )
    |> List.fold ~init:Prob.one ~f:Prob.min

   (* minimum probability of not dropping packet *)
  let min_nondrop_prob' t =
    let rec go t acc =
      match unget t with
      | Leaf dist ->
        FactorizedActionDist.to_alist dist
        |> List.filter_map ~f:(function
            | (Action.Drop,_) -> None
            | (_,p) -> Some p
        )
        |> List.fold ~init:Prob.zero ~f:Prob.(+)
        |> Prob.min acc
      | Branch { tru; fls; _ } ->
        go tru acc
        |> go fls
    in
    go t Prob.one


  let set_order order =
    let meta_fields = Field.[
      Meta0; Meta1; Meta2; Meta3; Meta4; Meta5; Meta6; Meta7; Meta8; Meta9;
      Meta10; Meta11; Meta12; Meta13; Meta14; Meta15; Meta16; Meta17; Meta18;
      Meta19
    ]
    in
    let global_fields = List.map order ~f:allocate_global_field in
    let missing_fields =
      List.filter Field.all ~f:(fun f ->
        let mem = List.mem ~equal:Field.equal in
        not (mem global_fields f) && not (mem meta_fields f)
      )
    in
    let order = global_fields @ missing_fields @ meta_fields in
    printf "[fdd] setting order to %s\n%!" (List.to_string order ~f:Field.to_string);
    Field.set_order order


  let pp ?(show=true) fmt t =
    if show then render t;
    pp fmt t
end
