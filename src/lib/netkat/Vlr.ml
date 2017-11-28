open Core

module type HashCmp = sig
  include Hashtbl.Key
  val to_string : t -> string
end

module type Lattice = sig
  include HashCmp
  val subset_eq : t -> t -> bool
  val meet : ?tight:bool -> t -> t -> t option
  val join : ?tight:bool -> t -> t -> t option
end

module type Result = sig
  include HashCmp
  val sum : t -> t -> t
  val prod : t -> t -> t
  val one : t
  val zero : t
end

module IntPair = struct
  type t = (int * int) [@@deriving sexp, compare]
  let hash (t1, t2) = 617 * t1 +  619 * t2
end

module IntPairTbl = Hashtbl.Make(IntPair)

module Make(V:HashCmp)(L:Lattice)(R:Result) = struct
  type v = V.t * L.t [@@deriving sexp, compare]
  type r = R.t [@@deriving sexp, compare]

  type d
    = Leaf of r
    | Branch of v * int * int
  [@@deriving sexp, compare]
  (* A tree structure representing the decision diagram. The [Leaf] variant
   * represents a constant function. The [Branch(v, l, t, f)] represents an
   * if-then-else. When variable [v] takes on the value [l], then [t] should
   * hold. Otherwise, [f] should hold.
   *
   * [Branch] nodes appear in an order determined first by the total order on
   * the [V.t] value with with ties broken by the total order on [L.t]. The
   * least such pair should appear at the root of the diagram, with each child
   * nodes being strictly greater than their parent node. This invariant is
   * important both for efficiency and correctness.
   * *)

  type t = int [@@deriving sexp, compare, eq]
  module T = Frenetic_kernel.Hashcons.Make(struct
      type t = d [@@deriving sexp, compare]

      let hash t = match t with
        | Leaf r ->
          (R.hash r) lsl 1
        | Branch((v, l), t, f) ->
          (1021 * (V.hash v) + 1031 * (L.hash l) + 1033 * t + 1039 * f) lor 0x1

    end)

  let get = T.get
  let unget = T.unget
  let get_uid (t:t) : int = t

  module Tbl = Int.Table

  module BinTbl = IntPairTbl

  let mk_leaf r = T.get (Leaf r)

  let mk_branch (v,l) t f =
    (* When the ids of the diagrams are equal, then the diagram will take on the
       same value regardless of variable assignment. The node that's being
       constructed can therefore be eliminated and replaced with one of the
       sub-diagrams, which are identical.

       If the ids are distinct, then the node has to be constructed and assigned
       a new id. *)
    if equal t f then
      t
    else
      T.get (Branch((v, l), t, f))

  let unchecked_cond = mk_branch

  let drop = mk_leaf (R.zero)
  let id = mk_leaf (R.one)

  let rec to_string t =
    if t = drop then "0" else
    if t = id then "1" else
    match T.unget t with
    | Leaf r ->
       Printf.sprintf "%s" (R.to_string r)
    | Branch((v, l), t, f) ->
       Printf.sprintf "(%s = %s ? %s : %s)"
   (V.to_string v) (L.to_string l) (to_string t) (to_string f)

  let rec fold ~f ~g t = match T.unget t with
    | Leaf r -> f r
    | Branch((v, l), tru, fls) ->
      g (v, l) (fold ~f ~g tru) (fold ~f ~g fls)

  let const r = mk_leaf r
  let atom (v,l) t f = mk_branch (v,l) (const t) (const f)

  let rec map_r ~f t = fold t
    ~f:(fun r -> const (f r))
    ~g:(fun (v, l) tru fls -> mk_branch (v,l) tru fls)

  let restrict lst u =
    let rec loop xs u =
      match xs, T.unget u with
      | []          , _
      | _           , Leaf _ -> u
      | (v,l) :: xs', Branch((v', l'), t, f) ->
        match V.compare v v' with
        |  0 -> if L.subset_eq l l' then loop xs' t else loop xs f
        | -1 -> loop xs' u
        |  1 -> mk_branch (v',l') (loop xs t) (loop xs f)
        |  _ -> assert false
    in
    loop (List.sort (fun (u, _) (v, _) -> V.compare u v) lst) u

  let apply f zero ~(cache: (t*t, t) Hashtbl.t) =
    let rec sum x y =
      BinTbl.find_or_add cache (x, y) ~default:(fun () -> sum' x y)
    and sum' x y =
      match T.unget x, T.unget y with
      | Leaf r, _      ->
        if R.compare r zero = 0 then y
        else map_r (fun y -> f r y) y
      | _     , Leaf r ->
        if R.compare zero r = 0 then x
        else map_r (fun x -> f x r) x
      | Branch((vx, lx), tx, fx), Branch((vy, ly), ty, fy) ->
        begin match V.compare vx vy with
        |  0 ->
          begin match L.compare lx ly with
          |  0 -> mk_branch (vx,lx) (sum tx ty) (sum fx fy)
          | -1 -> mk_branch (vx,lx) (sum tx (restrict [(vx, lx)] y)) (sum fx y)
          |  1 -> mk_branch (vy,ly) (sum (restrict [(vy, ly)] x) ty) (sum x fy)
          |  _ -> assert false
          end
        | -1 -> mk_branch (vx,lx) (sum tx y) (sum fx y)
        |  1 -> mk_branch (vy,ly) (sum x ty) (sum x fy)
        |  _ -> assert false
        end
    in sum

  let sum_tbl : (t*t, t) Hashtbl.t = BinTbl.create ~size:1000 ()
  let sum = apply R.sum R.zero ~cache:sum_tbl

  let prod_tbl : (t*t, t) Hashtbl.t = BinTbl.create ~size:1000 ()
  let prod = apply R.prod R.one ~cache:prod_tbl

  let clear_cache ~(preserve : Int.Set.t) =
    (* SJS: the interface exposes `id` and `drop` as constants,
       so they must NEVER be cleared from the cache *)
    let preserve = Int.Set.(add (add preserve drop) id) in begin
      BinTbl.clear sum_tbl;
      BinTbl.clear prod_tbl;
      T.clear preserve;
    end

  let cond v t f =
    let ok t =
      match unget t with
      | Leaf _ -> true
      | Branch ((f',v'), _, _) -> V.compare (fst v) f' = -1
    in
    if equal t f then t else
    if ok t && ok f then mk_branch v t f else
      (sum (prod (atom v R.one R.zero) t)
           (prod (atom v R.zero R.one) f))

  let map ~(f : R.t -> t)
          ~(g : V.t * L.t -> t -> t -> t)
           (t : t) : t =
    let rec map t = match unget t with
      | Leaf r -> f r
      | Branch ((v, l), tru, fls) -> g (v,l) (map tru) (map fls) in
    map t

  let dp_map ~(f : R.t -> t)
             ~(g : V.t * L.t -> t -> t -> t)
             (t : t)
             ~find_or_add
             : t =
    let rec map t =
      find_or_add t ~default:(fun () -> map' t)
    and map' t =
      match unget t with
        | Leaf r -> f r
        | Branch ((v, l), tru, fls) -> g (v,l) (map tru) (map fls) in
    map t

  let compressed_size (node : t) : int =
    let rec f (node : t) (seen : Int.Set.t) =
      if Int.Set.mem seen node then
        (0, seen)
      else
        match T.unget node with
        | Leaf _ -> (1, Int.Set.add seen node)
        | Branch (_, hi, lo) ->
          (* Due to variable-ordering, there is no need to add node.id to seen
             in the recursive calls *)
          let (hi_size, seen) = f hi seen in
          let (lo_size, seen) = f lo seen in
          (1 + hi_size + lo_size, Int.Set.add seen node) in
    let (size, _) = f node Int.Set.empty in
    size

  let rec uncompressed_size (node : t) : int = match T.unget node with
    | Leaf _ -> 1
    | Branch (_, hi, lo) -> 1 + uncompressed_size hi + uncompressed_size lo

  let to_dot t =
    let open Format in
    let buf = Buffer.create 200 in
    let fmt = formatter_of_buffer buf in
    let seen : Int.Hash_set.t = Int.Hash_set.create ~size:10 () in
    let rank : ((V.t*L.t), Int.Hash_set.t) Hashtbl.t = Hashtbl.Poly.create ~size:20 () in
    pp_set_margin fmt (1 lsl 29);
    fprintf fmt "digraph tdk {@\n";
    let rec loop t =
      if not (Hash_set.mem seen t) then begin
        Hash_set.add seen t;
        match T.unget t with
        | Leaf r ->
          fprintf fmt "%d [shape=box label=\"%s\"];@\n" t (R.to_string r)
        | Branch((v, l), a, b) ->
          begin
            try Hash_set.add (Hashtbl.find_exn rank (v, l)) t
            with Not_found ->
              let s = Int.Hash_set.create ~size:10 () in
              Hash_set.add s t;
              Hashtbl.set rank (v, l) s
          end;
          fprintf fmt "%d [label=\"%s = %s\"];@\n"
            t (V.to_string v) (L.to_string l);
          fprintf fmt "%d -> %d;@\n" t a;
          fprintf fmt "%d -> %d [style=\"dashed\"];@\n" t b;
          loop a;
          loop b
      end
    in
    loop t;
    Hashtbl.iteri rank ~f:(fun ~key:_ ~data:s ->
      fprintf fmt "{rank=same; ";
      Hash_set.iter s ~f:(fun x -> fprintf fmt "%d " x);
      fprintf fmt ";}@\n");
    fprintf fmt "}@.";
    Buffer.contents buf

  let refs (t : t) : Int.Set.t =
    let rec f (node : t) (seen : Int.Set.t) =
      if Int.Set.mem seen node then
        seen
      else
        match T.unget node with
        | Leaf _ -> Int.Set.add seen node
        | Branch (_, hi, lo) ->
          Int.Set.add (f lo (f hi seen)) node in
    f t Int.Set.empty

end
