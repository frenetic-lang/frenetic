open Core.Std


(*==========================================================================*)
(* Packets & Histories                                                      *)
(*==========================================================================*)

type headerval =
  | Switch of int
  | Port of int
  | Id of int
  | Src of int
  | Dst of int
  [@@ deriving sexp, compare, show]

let string_of_headerval (hv : headerval) = match hv with
  | Switch n  -> "Switch=" ^ (string_of_int n)
  | Port n    -> "Port=" ^ (string_of_int n)
  | Id n      -> "Id=" ^ (string_of_int n)
  | Src n     -> "Src=" ^ (string_of_int n)
  | Dst n     -> "Dst=" ^ (string_of_int n)

module type PSEUDOHISTORY = sig
  type t [@@deriving sexp, compare]
  val dup : t -> t
  val test : t -> hv:headerval -> t option (* None if false, Some t if true. Useful to filter wildcards *)
  val modify : t -> hv:headerval -> t
  val to_string : t -> string
  val make : ?switch:int option -> ?port:int option -> ?id:int option -> ?dst:int option -> unit -> t
end


module Pkt = struct
  type t = { switch : int option [@default None];
             port : int option [@default None];
             id : int option [@default None];
             src : int option [@default None];
             dst : int option [@default None];
           } [@@deriving sexp, compare, show, make]

  let dup pk = pk

  let test pk ~(hv:headerval) : t option =
    match hv with
    | Switch sw -> begin
      match pk.switch with
      | None ->
        Some { pk with switch = Some sw }
      | Some v ->
        if v = sw then Some pk
        else None
      end
    | Port pt -> begin
      match pk.port with
      | None ->
        Some { pk with port = Some pt }
      | Some v ->
        if v = pt then Some pk
        else None
      end
    | Id id -> begin
      match pk.id with
      | None ->
        Some { pk with id = Some id }
      | Some v ->
        if v = id then Some pk
        else None
      end
    | Src s -> begin
      match pk.src with
      | None ->
        Some { pk with src = Some s }
      | Some v ->
        if v = s then Some pk
        else None
      end
    | Dst d -> begin
      match pk.dst with
      | None ->
        Some { pk with dst = Some d }
      | Some v ->
        if v = d then Some pk
        else None
      end

  let modify pk ~(hv:headerval) : t =
    match hv with
    | Switch sw -> { pk with switch = Some sw }
    | Port pt -> { pk with port = Some pt }
    | Id id -> { pk with id = Some id }
    | Src s -> { pk with src = Some s }
    | Dst d -> { pk with dst = Some d }

  let to_string t =
    let opt_string (fld : int option) = match fld with
      | None -> "*"
      | Some n -> string_of_int n in
    Printf.sprintf "%s|%s|%s|%s|%s" (opt_string t.switch) (opt_string t.port) (opt_string t.id) (opt_string t.src)
    (opt_string t.dst)
end


module Hist = struct
  type t = Pkt.t * Pkt.t list [@@deriving sexp, compare, show]

  let dup (pk,h) = (pk, pk::h)

  let test (pk,h) ~hv =
    match Pkt.test pk ~hv with
    | None -> None
    | Some pk' -> Some (pk',h)

  let modify (pk,h) ~hv = (Pkt.modify pk ~hv, h)

  let to_string (pk,h) =
    List.map (pk::h) ~f:Pkt.to_string
    |> String.concat ~sep:"::"

  let make ?switch ?port ?id ?dst () =
    (Pkt.make ?switch ?port ?id ?dst (), [])
end



(*==========================================================================*)
(* Probability & ProbNetKAT Policies                                        *)
(*==========================================================================*)

module type PROB = sig
  type t [@@deriving sexp, compare, show]
  val zero : t
  val one : t
  val (+) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val of_int : int -> t
  val to_string : t -> string
  val to_dec_string : t -> string (* decimal representation *)
end

module PreciseProb = struct
  include Num
  let pp_num f n = Printf.printf "%s" (string_of_num n)
  let num_of_sexp _ = failwith "<>"
  let sexp_of_num n = Sexp.of_string (string_of_num n)
  type t = num [@@deriving sexp, compare, show]
  let zero = Int 0
  let one = Int 1
  let (+) a b = a +/ b
  let ( * ) a b = a */ b
  let ( / ) a b = a // b
  let of_int = num_of_int
  let to_string = string_of_num
  let to_dec_string = approx_num_fix 10
end

module FloatProb = struct
  include Float
  let show _ = ""
  let to_dec_string = Printf.sprintf "%.10f"
end


module Pol (Prob : PROB) = struct
  type t =
    | Id
    | Drop
    | Test of headerval
    | Mod of headerval
    | Union of t * t
    | Seq of t * t
    | Choice of (t * Prob.t) list
    | Star of t
    | Dup
    [@@deriving sexp, compare, show]

  let ( ?? ) hv = Test hv
  let ( !! ) hv = Mod hv
  let ( >> ) p q = Seq (p, q)
  let ( & ) p q = Union (p, q)
  let ( ?@ ) dist = Choice dist (* ?@[p , 1//2; q , 1//2] *)

  let rec to_string = function
    | Id -> "Id"
    | Drop -> "Drop"
    | Test hv -> (string_of_headerval hv)
    | Mod hv ->  Str.global_replace (Str.regexp_string "=") "<-" (string_of_headerval hv)
    | Seq (p, q) -> (to_string p) ^ "; " ^ (to_string q)
    | Union (p, q) -> "(" ^ (to_string p) ^ ")\n& (" ^ (to_string q) ^ ")"
    | Choice dist -> begin
      List.map ~f:(fun (pol, prob) ->
        Printf.sprintf "[%s @ %s]" (to_string pol) (Prob.to_string prob)) dist
      |> String.concat ~sep:" [+] "
      |> Printf.sprintf "(%s)"
      end
    | Star p -> (to_string p) ^ "*"
    | Dup -> "dup"

end



(*==========================================================================*)
(* Distributions & Kernels                                                  *)
(*==========================================================================*)

module Dist (Point : Map.Key) (Prob : PROB) = struct

  module T = Map.Make(Point) (* Point.t -> 'a *)

  type t = Prob.t T.t [@@deriving sexp] (* Point.t -> Prob.t *)

  let empty : t = T.empty

  (* point mass distribution *)
  let dirac (p : Point.t) : t = T.singleton p Prob.one

  (* pointwise sum of distributions *)
  let sum t1 t2 : t =
    Map.merge t1 t2 ~f:(fun ~key vals ->
      Some (match vals with
      | `Both (v1, v2) -> Prob.(v1 + v2)
      | `Left v | `Right v -> v))

  let scale t ~(scalar : Prob.t) : t =
    T.map t ~f:(fun p -> Prob.(p * scalar))

  let weighted_sum (list : (t * Prob.t) list) : t =
    List.map list ~f:(fun (t, p) -> scale t ~scalar:p)
    |> List.fold ~init:empty ~f:sum

  (* expectation of random variable [f] w.r.t distribution [t] *)
  let expectation t ~(f : Point.t -> Prob.t) : Prob.t =
    T.fold t ~init:Prob.zero ~f:(fun ~key:point ~data:prob acc ->
      Prob.(acc + prob * f point))

  (* Markov Kernel *)
  module Kernel = struct
    type kernel = Point.t -> t

    (* lift Markov Kernel to measure transformer *)
    let lift (k : kernel) (dist : t) : t =
      T.fold dist ~init:empty ~f:(fun ~key:point ~data:prob acc ->
        k point (* output distribution for that intermediate result *)
        |> scale ~scalar:prob
        |> sum acc)

    (* sequential composition of kernels *)
    let seq (k1 : kernel) (k2 : kernel) : kernel = fun input_point ->
      k1 input_point
      |> lift k2

    type t = kernel

  end

  (* probability monad *)
  module Monad = struct
    module Let_syntax = struct
      let bind a b = Kernel.lift b a
      let (>>=) a b = bind a b
      let return = dirac
    end
    include Let_syntax
  end



end

module SetDist (Point : Map.Key) (Prob : PROB) = struct
  module PSet = Set.Make(Point)
  module D = Dist(PSet)(Prob)

  module SemiKernel = struct
    type skernel = Point.t -> D.t

    let lift (k : skernel) : D.Kernel.t =
      PSet.fold ~init:(D.dirac PSet.empty) ~f:(fun acc p ->
        let open D.Monad in
        let%bind a1 = k p in
        let%bind a2 = acc in
        return (PSet.union a1 a2))

    type t = skernel
  end

  module Let_syntax = struct
    let bind a b = D.Kernel.lift (SemiKernel.lift b) a
    let (>>=) a b = bind a b
    let return p = D.dirac (PSet.singleton p)
  end

end



(*==========================================================================*)
(* ProbNetKAT Interpreter                                                   *)
(*==========================================================================*)

module Interp (Hist : PSEUDOHISTORY) (Prob : PROB) = struct

  module Hist = Hist
  module Prob = Prob
  module Pol = Pol(Prob)

  module HSet = struct
    include Set.Make(Hist)
    let to_string t =
      to_list t
      |> List.map ~f:Hist.to_string
      |> String.concat ~sep:", "
      |> Printf.sprintf "{ %s }"
  end

  module Dist = struct
    include Dist(HSet)(Prob)
    let to_string t =
      T.to_alist t
      |> List.map ~f:(fun (hs, prob) ->
        Printf.sprintf "%s @ %s" (HSet.to_string hs) (Prob.to_string prob))
      |> String.concat ~sep:"\n"
      |> Printf.sprintf "%s\n"
    let print t =
      print_endline (to_string t)
  end

  let eval (n : int) (p : Pol.t) : Dist.Kernel.t =
    let open Dist.Monad in
    let rec eval (p : Pol.t) (inp : HSet.t) : Dist.t =
      match p with
      | Id ->
        return inp
      | Drop ->
        return HSet.empty
      | Dup ->
        return (HSet.map inp ~f:Hist.dup)
      | Test hv ->
        return (HSet.filter_map inp ~f:(Hist.test ~hv))
      | Mod hv ->
        return (HSet.map inp ~f:(Hist.modify ~hv))
      | Union (q,r) ->
        let%bind a1 = eval q inp in
        let%bind a2 = eval r inp in
        return (HSet.union a1 a2)
      | Seq (q,r)->
        let%bind a = eval q inp in
        eval r a
      | Choice dist ->
        List.map dist ~f:(fun (p, prob) -> (eval p inp, prob))
        |> Dist.weighted_sum
      | Star q ->
        star n q inp

    and star (n : int) (p : Pol.t) (inp : HSet.t) : Dist.t =
      match n with
      | 0 ->
        return inp
      | _ ->
        let%bind a = eval p inp in
        let%bind b = star (n-1) p a in
        return (HSet.union inp b)

    in eval p

  (* Given RV f : 2^H -> |R, compute expectation E[f] w.r.t. distribution
     mu = [|p|]_n inp  *)
  let expectation ?(inp=Dist.dirac (HSet.singleton (Hist.make ()))) (n : int)
    (p : Pol.t) ~(f : HSet.t -> Prob.t) =
    let open Dist.Monad in
    inp >>= eval n p |> Dist.expectation ~f

  let expectation' ?(inp : Dist.t option) (n : int) (p : Pol.t)  ~(f : Hist.t -> Prob.t) =
    expectation n p ?inp ~f:(fun hset ->
      let n = HSet.length hset in
      if n=0 then Prob.of_int 0 else
      let sum = HSet.fold hset ~init:Prob.zero ~f:(fun acc h ->
        Prob.(acc + f h))
      in
      Prob.(sum / of_int n))

  (* allows convenient specification of probabilities *)
  let ( / ) (a : int) (b : int) : Prob.t =
    Prob.(of_int a / of_int b)

  (* export more convenient eval *)
  let eval ?(inp=HSet.singleton (Hist.make ())) n p = eval n p inp

end
