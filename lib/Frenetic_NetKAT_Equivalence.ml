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

  module SymPkt = struct
    include Map.Make(FDD.Field) with type 'a = FDD.Value.t
    let all = emtpy
  end

  let equiv ?(pk=SymPkt.all) (a1 : A.t) (a2 : A.t) =
    let cache = Hash_set.Poly.create () in
    
    let rec eq_states pk (s1 : state) (s2 : state) =
      let mask = SymPkt.to_alist pk in
      let (e1, d1) = Hashtbl.find_exn a1.states s1 in
      let (e2, d2) = Hashtbl.find_exn a1.states s2 in
      let (e1, d1) = FDD.(restrict mask e1, restrict mask d1) in
      let (e2, d2) = FDD.(restrict mask e2, restrict mask d2) in
      Hash_set.mem cache (e1,d1,e2,d2) || begin
        Hash_set.add cache (e1,d1,e2,d2);
        eq_es pk e1 e2 && eq_ds pk d1 d2
      end

    and eq_es pk = eq_fdd pk ~leaf_eq: begin fun pk par1 par2 ->
      let pks1 = Set.Poly.map par1 ~f:(Map.fold ~init:pk ~f:SymPkt.add) in
      let pks2 = Set.Poly.map par2 ~f:(Map.fold ~init:pk ~f:SymPkt.add) in
      Set.Poly.equal (SymPkt.equal FDD.Value.equal) pks1 pks2
    end

    and eq_ds pk = eq_fdd pk ~leaf_eq: begin fun pk par1 par2 ->
      failwith "todo"
    end

    and eq_fdd ~leaf_eq pk fdd1 fdd2 =
      match FDD.(unget fdd1, fdd1, unget fdd2, fdd2) with
      | Leaf r1,_, Leaf r2,_ ->
        leaf_eq pk r1 r2
      | Leaf _,fdd, Leaf ((f,n), tru, fls),_
      | Leaf ((f,n), tru, fls),_, Leaf _,fdd ->
        eq_fdd ~leaf_eq pk fdd fls && begin
          let pk' = SymPkt.add pk ~key:f ~data:n in
          match SymPkt.find pk f with
          | None 
          | Some m when m = n -> eq_fdd ~leaf_eq pk' fdd1 fls
          | _ -> true
        end
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

    in
    eq_states (mask, a1.source) (mask, a2.source)

end
