open Core

module type HashCmp = sig
  type t [@@deriving sexp, compare, eq, hash]
  (* val pp : Format.formatter -> t -> unit *)
  val to_string : t -> string
end

module type Lattice = sig
  include HashCmp
  val subset_eq : t -> t -> bool
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
  type v = V.t * L.t [@@deriving sexp, compare, hash]
  type r = R.t [@@deriving sexp, compare, hash]

  type d
    = Leaf of r
    | Branch of {
        test : v;
        tru : int;
        fls : int;
        all_fls : int [@compare.ignore] (* implies [@hash.ignore] *);
      }
    [@@deriving sexp, compare, hash]
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

  module T = struct
    type t = int [@@deriving sexp, compare, eq]
  end
  include T
  include Comparator.Make(T)

  module D = Frenetic_kernel.Hashcons.Make(struct
    type t = d [@@deriving sexp, compare, hash]
  end)

  let get = D.get
  let unget = D.unget
  let get_uid (t:t) : int = t

  module Tbl = Int.Table
  module BinTbl = IntPairTbl

  let mk_leaf r = D.get (Leaf r)

  let mk_branch ((v,l) as test) tru fls =
    (* When the ids of the diagrams are equal, then the diagram will take on the
       same value regardless of variable assignment. The node that's being
       constructed can therefore be eliminated and replaced with one of the
       sub-diagrams, which are identical.

       If the ids are distinct, then the node has to be constructed and assigned
       a new id. *)
    if equal tru fls then
      fls
    else match unget fls with
    | Branch { test = (v',_); all_fls; _ } when v=v' ->
      if all_fls = tru then
        fls
      else
        D.get (Branch { test; tru; fls; all_fls })
    | _ ->
      D.get (Branch { test; tru; fls; all_fls = fls})

  let unchecked_cond = mk_branch

  let drop = mk_leaf (R.zero)
  let id = mk_leaf (R.one)

  let rec to_string t =
    if t = drop then "0" else
    if t = id then "1" else
    match D.unget t with
    | Leaf r ->
      Printf.sprintf "%s" (R.to_string r)
    | Branch { test = (v, l); tru = t; fls = f } ->
      Printf.sprintf "(%s = %s ? %s : %s)"
      (V.to_string v) (L.to_string l) (to_string t) (to_string f)


  let rec fold ~f ~g t = match D.unget t with
    | Leaf r -> f r
    | Branch { test = (v, l);  tru; fls } ->
      g (v, l) (fold ~f ~g tru) (fold ~f ~g fls)

  let const r = mk_leaf r
  let atom (v,l) t f = mk_branch (v,l) (const t) (const f)

  let rec map_r ~f t = fold t
    ~f:(fun r -> const (f r))
    ~g:(fun (v, l) tru fls -> mk_branch (v,l) tru fls)

  let restrict lst u =
    let rec loop xs u =
      match xs, D.unget u with
      | []          , _
      | _           , Leaf _ -> u
      | (v,l) :: xs', Branch { test = (v', l'); tru = t; fls = f } ->
        match V.compare v v' with
        |  0 -> if L.subset_eq l l' then loop xs' t else loop xs f
        | -1 -> loop xs' u
        |  1 -> mk_branch (v',l') (loop xs t) (loop xs f)
        |  _ -> assert false
    in
    loop (List.sort (fun (u, _) (v, _) -> V.compare u v) lst) u

  let apply f zero ~(cache: (t*t, t) Hashtbl.t) =
    let rec sum x y =
      Hashtbl.find_or_add cache (x, y) ~default:(fun () -> sum' x y)
    and sum' x y =
      match D.unget x, D.unget y with
      | Leaf r, _      ->
        if R.compare r zero = 0 then y
        else map_r (fun y -> f r y) y
      | _     , Leaf r ->
        if R.compare zero r = 0 then x
        else map_r (fun x -> f x r) x
      | Branch {test=(vx, lx); tru=tx; fls=fx; all_fls=all_fls_x},
        Branch {test=(vy, ly); tru=ty; fls=fy; all_fls=all_fls_y} ->
        begin match V.compare vx vy with
        |  0 ->
          begin match L.compare lx ly with
          |  0 -> mk_branch (vx,lx) (sum tx ty) (sum fx fy)
          | -1 -> mk_branch (vx,lx) (sum tx all_fls_y) (sum fx y)
          |  1 -> mk_branch (vy,ly) (sum all_fls_x ty) (sum x fy)
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

  let childreen t =
    let rec loop t acc =
      match unget t with
      | Leaf _ -> acc
      | Branch { tru=l; fls=r } ->
        l::r::acc
        |> loop l
        |> loop r
    in
    loop t []

  let clear_cache ~(preserve : Int.Set.t) =
    (* SJS: the interface exposes `id` and `drop` as constants,
       so they must NEVER be cleared from the cache *)
    let preserve =
      Int.Set.(add (add preserve drop) id)
      |> fun init -> Int.Set.fold init ~init ~f:(fun init root ->
        List.fold (childreen root) ~init ~f:Int.Set.add
      )
    in
    begin
      Hashtbl.clear sum_tbl;
      Hashtbl.clear prod_tbl;
      D.clear preserve;
    end

  let cond v t f =
    let ok t =
      match unget t with
      | Leaf _ -> true
      | Branch { test = (f',v') } -> V.compare (fst v) f' = -1
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
      | Branch { test=(v, l); tru; fls } -> g (v,l) (map tru) (map fls) in
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
        | Branch { test=(v, l); tru; fls } -> g (v,l) (map tru) (map fls) in
    map t

  let compressed_size (node : t) : int =
    let rec f (node : t) (seen : Int.Set.t) =
      if Int.Set.mem seen node then
        (0, seen)
      else
        match D.unget node with
        | Leaf _ -> (1, Int.Set.add seen node)
        | Branch { tru; fls } ->
          (* Due to variable-ordering, there is no need to add node.id to seen
             in the recursive calls *)
          let (tru_size, seen) = f tru seen in
          let (fls_size, seen) = f fls seen in
          (1 + tru_size + fls_size, Int.Set.add seen node)
    in
    f node Int.Set.empty
    |> fst

  let rec uncompressed_size (node : t) : int = match D.unget node with
    | Leaf _ -> 1
    | Branch { tru; fls } -> 1 + uncompressed_size tru + uncompressed_size fls

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
        match D.unget t with
        | Leaf r ->
          fprintf fmt "%d [shape=box label=\"%s\"];@\n" t (R.to_string r)
        | Branch { test=(v, l); tru=a; fls=b } ->
          (* FIXME: temporary hack to avoid Jane Street's annoying warnings. *)
          begin[@warning "-3"]
            try Hash_set.add (Hashtbl.find_exn rank (v, l)) t
            with Not_found | Not_found_s _ ->
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

  let render ?(format="pdf") ?(title="FDD") t =
    Frenetic_kernel.Util.show_dot ~format ~title (to_dot t)

  let refs (t : t) : Int.Set.t =
    let rec f (node : t) (seen : Int.Set.t) =
      if Int.Set.mem seen node then
        seen
      else
        match D.unget node with
        | Leaf _ -> Int.Set.add seen node
        | Branch { tru=hi; fls=lo } ->
          Int.Set.add (f lo (f hi seen)) node in
    f t Int.Set.empty

  let rec node_to_sexp node =
    let open Sexplib.Sexp in
    match node with
    | Leaf r ->
      List [Atom "Leaf"; R.sexp_of_t r]
    | Branch { test; tru; fls } ->
      let tru = node_to_sexp @@ unget tru in
      let fls = node_to_sexp @@ unget fls in
      List [Atom "Branch"; sexp_of_v test; tru; fls]

  let rec node_of_sexp sexp =
    let open Sexplib.Sexp in
    match sexp with
    | List [Atom "Leaf"; sexp] ->
      get (Leaf (R.t_of_sexp sexp))
    | List [Atom "Branch"; test; tru; fls] ->
      let test = v_of_sexp test in
      let tru = node_of_sexp tru in
      let fls = node_of_sexp fls in
      mk_branch test tru fls
    | _ ->
      failwith "unsexpected s-expression!"


  let serialize (t : t) : string =
    unget t
    |> node_to_sexp
    |> Sexp.to_string

  let deserialize (s : string) : t =
    Sexp.of_string s
    |> node_of_sexp
end
