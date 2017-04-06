open Core.Std

module A = Frenetic_NetKAT_Compiler.Automaton
module FDD = Frenetic_NetKAT_Compiler.FDD


module Naive = struct

  type state = int

  module SymPkt = struct
    include Map.Make(Frenetic_Fdd.Field)
    let all = empty
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
      (* let add ~key ~data m = SymPkt.add m ~key ~data in *)
      (* let pks1 = Set.Poly.map par1 ~f:(Map.fold ~init:pk ~f:add) in *)
      (* let pks2 = Set.Poly.map par2 ~f:(Map.fold ~init:pk ~f:add) in *)
      (* Set.equal pks1 pks2 *)
      failwith "todo"
    end

    and eq_ds pk = eq_fdd pk ~leaf_eq: begin fun pk par1 par2 ->
      failwith "todo"
    end

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
    eq_states pk a1.source a2.source

end
