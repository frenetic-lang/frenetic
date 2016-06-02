open Frenetic_Decide_Util

module Ast = Frenetic_Decide_Ast
module Deriv = Frenetic_Decide_Deriv
module DerivTerm = Deriv.BDDDeriv

module WorkList = functor (K : Set.OrderedType) ->
struct
  module S = Set.Make(K)
  type t = S.t * (K.t list)

  let add (e : K.t) (wl : t)  : t =
    let set,worklist = wl in
    if S.mem e set
    then (
      wl)
    else S.add e set,e::worklist

  let singleton (e : K.t ) : t =
    S.singleton e, [e]

  let is_empty wl : bool =
    let set,wl = wl in
    match wl with
      | [] -> true
      | _ -> false

  let hd (set,wl) : K.t =
    List.hd wl

  let tl (set,wl) : t = set, List.tl wl

  let all_seen_items (set,_) =
    S.elements set

end

module TermsWorkList = WorkList(struct
  type t = (DerivTerm.t * DerivTerm.t)

  let compare (a1, b1) (a2, b2) =
    match (DerivTerm.compare a1 a2) with
    | 0 -> (DerivTerm.compare b1 b2)
    | k -> k
end)

module UF = Frenetic_Decide_Util.UnionFind(DerivTerm)

let check_equivalent (t1: Ast.Term.t) (t2: Ast.Term.t) : bool =
  let bisim = UF.create () in

  let rec main_loop work_list =
    if TermsWorkList.is_empty work_list then
      true
    else
      let q1, q2 = TermsWorkList.hd work_list in
      let rest_work_list = TermsWorkList.tl work_list in
      let q1_E = DerivTerm.get_e q1 in
      let q2_E = DerivTerm.get_e q2 in
      if not (DerivTerm.EMatrix.compare q1_E q2_E = 0) then
        false
      else
        let u,f = UF.find bisim q1, UF.find bisim q2 in
        (* find e returns the canonical element for e, so equality of classes
         * is just equality of canonical elements *)
        if DerivTerm.compare u f = 0 then
          main_loop rest_work_list
        else
          let _ = UF.union bisim u f in
          let q1d = DerivTerm.get_d q1 in
          let q2d = DerivTerm.get_d q2 in
          let q1points = DerivTerm.DMatrix.points q1d in
          let q2points = DerivTerm.DMatrix.points q2d in
          let emat = DerivTerm.EMatrix.union q1points q2points in
          let f expanded_work_list pt =
            let q1' = DerivTerm.DMatrix.run q1d pt in
            let q2' = DerivTerm.DMatrix.run q2d pt in
            TermsWorkList.add (DerivTerm.make_term q1',DerivTerm.make_term q2')
            expanded_work_list
          in
          let work_list = DerivTerm.EMatrix.fold emat ~init:rest_work_list ~f in
          main_loop work_list
   in

  let t1' = DerivTerm.make_term (Ast.TermSet.singleton t1) in
  let t2' = DerivTerm.make_term (Ast.TermSet.singleton t2) in
  main_loop (TermsWorkList.singleton (t1', t2'))
