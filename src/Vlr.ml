open Core

module type HashCmp = sig
  type t [@@deriving sexp, compare, eq, hash]
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

module type Value = sig
  include HashCmp
  include (sig
    type t [@@deriving bin_io]
  end with type t := t)
end

module type Lattice = sig
  include Value
  val subset_eq : t -> t -> bool
end

module type Result = sig
  include HashCmp
  val sum : t -> t -> t
  val prod : t -> t -> t
  val one : t
  val zero : t
  val is_one : t -> bool
  val is_zero : t -> bool
end

module IntPair = struct
  type t = (int * int) [@@deriving sexp, compare]
  let hash (t1, t2) = 617 * t1 +  619 * t2
end

module IntPairTbl = Hashtbl.Make(IntPair)

module Make(V:Value)(L:Lattice)(R:Result) = struct
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

  type t = int [@@deriving sexp, compare, eq]
  module T = Hashcons.Make(struct
    type t = d [@@deriving sexp, compare, hash]
  end)

  let cache_size = T.cache_size
  let get = T.get
  let unget = T.unget
  let get_uid (t:t) : int = t

  module Tbl = Int.Table

  module BinTbl = IntPairTbl
  module HSet = Hash_set.Make(Int)

    (** {2} timed versions of the compilation functions *)
  let stats_tbl : (string, float list) Hashtbl.t = String.Table.create ()
  let clear_stats () = Hashtbl.clear stats_tbl
  let measure key f =
    let t,x = Util.time' f in
    Hashtbl.add_multi stats_tbl key t;
    x

  (* total number of measurements *)
  let n_of_meas () =
    Hashtbl.fold stats_tbl ~init:0 ~f:(fun ~key:_ ~data acc ->
      acc + List.length data
    )

  (* total time measured *)
  let t_of_meas () =
    Hashtbl.fold stats_tbl ~init:0. ~f:(fun ~key:_ ~data init ->
      List.fold data ~init ~f:(+.)
    )

  let print_stats () =
    let n = n_of_meas () in
    let t = t_of_meas () in
    Hashtbl.to_alist stats_tbl
    |> List.map ~f:(fun (key, data) ->
      let ni = List.length data in
      let ti = List.fold data ~init:0. ~f:(+.) in
      let k = PrintBox.sprintf "%s" key in
      let rel = PrintBox.sprintf "# = %d (%.1f%%)" ni Float.((of_int ni) / (of_int n) * 100.) in
      let tot = PrintBox.sprintf "t = %.0fs (%.1f%%)" ti (ti /. t *. 100.) in
      [| k; rel; tot; |]
    )
    |> Array.of_list
    |> PrintBox.grid
    |> PrintBox_text.output stdout;
    printf "\n%!"


  let mk_leaf r = T.get (Leaf r)

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
        T.get (Branch { test; tru; fls; all_fls })
    | _ ->
      T.get (Branch { test; tru; fls; all_fls = fls})

  let unchecked_cond = mk_branch

  let drop = mk_leaf (R.zero)
  let id = mk_leaf (R.one)

  let rec pp fmt t =
    Format.fprintf fmt "@[";
    if t = drop then Format.fprintf fmt "0" else
    if t = id then Format.fprintf fmt "1" else
    begin match T.unget t with
    | Leaf r ->
      Format.fprintf fmt "%a" R.pp r
    | Branch { test = (v, l);  tru; fls } ->
      Format.fprintf fmt "(%a = %a ? %a : %a)"
      V.pp v L.pp l pp tru pp fls
    end;
    Format.fprintf fmt "@]"

  let to_string t =
    Format.asprintf "%a" pp t


  let rec fold ~f ~g t = match T.unget t with
    | Leaf r -> f r
    | Branch { test = (v, l);  tru; fls } ->
      g (v, l) (fold ~f ~g tru) (fold ~f ~g fls)

  let unary_cache : (t, t) Hashtbl.t = Int.Table.create ~size:1000 ()

  let dp_fold ~(f: r -> t) ~(g: v -> t -> t -> t) (t : t) : t =
    let () = Hashtbl.clear unary_cache in
    let rec map t =
      Hashtbl.find_or_add unary_cache t ~default:(fun () -> map' t)
    and map' t =
      match unget t with
      | Leaf r -> f r
      | Branch { test=(v, l); tru; fls } -> g (v,l) (map tru) (map fls)
    in
    map t

  let rec map_r ~f t = fold t
    ~f:(fun r -> mk_leaf (f r))
    ~g:(fun (v, l) tru fls -> mk_branch (v,l) tru fls)

  let rec dp_map_r ~f t = dp_fold t
    ~f:(fun r -> mk_leaf (f r))
    ~g:(fun (v, l) tru fls -> mk_branch (v,l) tru fls)

  let const r = mk_leaf r
  let atom (v,l) t f = mk_branch (v,l) (const t) (const f)

  let restrict lst t =
    let () = Hashtbl.clear unary_cache in
    let rec loop xs t =
      Hashtbl.find_or_add unary_cache t ~default:(fun () ->
        match xs, T.unget t with
        | []          , _
        | _           , Leaf _ -> t
        | (v,l) :: xs', Branch { test = (v', l'); tru = t; fls = f } ->
          match V.compare v v' with
          |  0 -> if L.subset_eq l l' then loop xs' t else loop xs f
          | -1 -> loop xs' t
          |  1 -> mk_branch (v',l') (loop xs t) (loop xs f)
          |  _ -> assert false
      )
    in
    loop (List.sort (fun (u, _) (v, _) -> V.compare u v) lst) t

  let binary_cache : (t*t, t) Hashtbl.t = BinTbl.create ~size:10000 ()

  let rec sum' x y =
    let key = if x <= y then (x, y) else (y, x) in
    BinTbl.find_or_add binary_cache key ~default:(fun () ->
      match T.unget x, T.unget y with
      | Leaf r, _      ->
        if R.is_zero r then y
        else map_r (fun y -> R.sum r y) y
      | _     , Leaf r ->
        if R.is_zero r then x
        else map_r (fun x -> R.sum x r) x
      | Branch {test=(vx, lx); tru=tx; fls=fx; all_fls=all_fls_x},
        Branch {test=(vy, ly); tru=ty; fls=fy; all_fls=all_fls_y} ->
        begin match V.compare vx vy with
        |  0 ->
          begin match L.compare lx ly with
          |  0 -> mk_branch (vx,lx) (sum' tx ty) (sum' fx fy)
          | -1 -> mk_branch (vx,lx) (sum' tx all_fls_y) (sum' fx y)
          |  1 -> mk_branch (vy,ly) (sum' all_fls_x ty) (sum' x fy)
          |  _ -> assert false
          end
        | -1 -> mk_branch (vx,lx) (sum' tx y) (sum' fx y)
        |  1 -> mk_branch (vy,ly) (sum' x ty) (sum' x fy)
        |  _ -> assert false
        end
    )

  let sum x y =
    Hashtbl.clear binary_cache;
    sum' x y



  let rec prod' x y =
    let key = if x <= y then (x, y) else (y, x) in
    BinTbl.find_or_add binary_cache key ~default:(fun () ->
      match T.unget x, T.unget y with
      | Leaf r, _      ->
        if R.is_one r then y
        else if R.is_zero r then x
        else map_r (fun y -> R.prod r y) y
      | _     , Leaf r ->
        if R.is_one r then x
        else if R.is_zero r then y
        else map_r (fun x -> R.prod x r) x
      | Branch {test=(vx, lx); tru=tx; fls=fx; all_fls=all_fls_x},
        Branch {test=(vy, ly); tru=ty; fls=fy; all_fls=all_fls_y} ->
        begin match V.compare vx vy with
        |  0 ->
          begin match L.compare lx ly with
          |  0 -> mk_branch (vx,lx) (prod' tx ty) (prod' fx fy)
          | -1 -> mk_branch (vx,lx) (prod' tx all_fls_y) (prod' fx y)
          |  1 -> mk_branch (vy,ly) (prod' all_fls_x ty) (prod' x fy)
          |  _ -> assert false
          end
        | -1 -> mk_branch (vx,lx) (prod' tx y) (prod' fx y)
        |  1 -> mk_branch (vy,ly) (prod' x ty) (prod' x fy)
        |  _ -> assert false
        end
    )

  let prod x y =
    Hashtbl.clear binary_cache;
    prod' x y


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

  let size t : int =
    childreen t
    |> Int.Set.of_list
    |> Set.length

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
      Hashtbl.clear unary_cache;
      Hashtbl.clear binary_cache;
      T.clear preserve;
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
        | Branch { test=(v, l); tru=a; fls=b } ->
          begin
            try Hash_set.add (Hashtbl.find_exn rank (v, l)) t
            with Caml.Not_found ->
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


  (* more sophisticated serialization *)
  module Bin = struct
    type t = {
      root : d;
      graph : d Int.Table.t;
    }
    and d =
      | BLeaf of Sexp.t
      | BBranch of {
        test : V.t * L.t;
        tru : int;
        fls : int;
      }
    [@@deriving bin_io]
  end

  include Binable.Of_binable(Bin)(struct
    type nonrec t = t

    let to_binable (t : t) : Bin.t =
      let graph = Int.Table.create () in
      let rec convert (t : t) : Bin.d =
        match unget t with
        | Leaf r -> BLeaf (R.sexp_of_t r)
        | Branch { test; tru=tru_t; fls=fls_t } ->
          let tru = (tru_t :> int) in
          let fls = (fls_t :> int) in
          Hashtbl.add graph ~key:tru ~data:(convert tru_t) |> ignore;
          Hashtbl.add graph ~key:fls ~data:(convert fls_t) |> ignore;
          BBranch { test; tru; fls }
      in
      let root = convert t in
      { root; graph }

    let of_binable ({ root; graph } : Bin.t) : t =
      let rec of_bin_d : Bin.d -> t = function
        | BLeaf sexp -> const (R.t_of_sexp sexp)
        | BBranch { test; tru; fls} ->
          let tru = Hashtbl.find_exn graph tru |> of_bin_d in
          let fls = Hashtbl.find_exn graph fls |> of_bin_d in
          unchecked_cond test tru fls
      in
      of_bin_d root
  end)

  let restrict lst t = measure "restrict" (fun () -> restrict lst t)
  let sum x y = measure "sum" (fun () -> sum x y)
  let prod x y = measure "prod" (fun () -> prod x y)
  let cond v t f = measure "cond" (fun () -> cond v t f)
  let unchecked_cond v t f = measure "unchecked cond" (fun () -> cond v t f)
end
