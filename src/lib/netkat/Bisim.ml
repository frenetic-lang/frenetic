open Core

module Auto = Global_compiler.Automaton

module Config = struct
  type t = int64 * Packet.T.t [@@deriving compare, hash, sexp]
  let of_first (state : int64) (pk : Packet.t) : t =
    Int64.(state*2L, pk)
  let of_second (state : int64) (pk : Packet.t) : t =
    Int64.(state*2L+1L, pk)
end

let check (a1 : Auto.t) (a2 : Auto.t) : bool =

  (* add dead states to both automata *)
  let dead = (Fdd.FDD.drop, Fdd.FDD.drop) in
  let dead1 = Auto.add_to_t a1 dead in
  let dead2 = Auto.add_to_t a2 dead in

  let () = Out_channel.write_all "auto1.dot" ~data:(Auto.to_dot a1) in
  let () = Out_channel.write_all "auto2.dot" ~data:(Auto.to_dot a2) in
  
  (* initialize worklist *)
  let dom = Domain.(merge (of_automaton a1) (of_automaton a2)) in
  let pks = Domain.representative_pks dom in
  let worklist = List.map pks ~f:(fun pk -> (a1.source, a2.source, pk)) in

  let rel : (Config.t, Config.t Union_find.t) Hashtbl.t = 
    Hashtbl.create (module Config)
  in

  (* let pp pk =
   *   let buf = Buffer.create 101 in
   *   Printf.bprintf buf "{";
   *   Map.iteri pk ~f:(fun ~key:f ~data:v -> Printf.bprintf buf ", %s=%Ld" (Fdd.Field.to_string f) v);
   *   Printf.bprintf buf "}";
   *   Buffer.contents buf in
   * let sp s = (List.fold_right ~init:"{" ~f:(fun pk acc -> acc ^ ", " ^ pp pk) (Set.elements s)) ^ "}" in           *)
    
  (* main loop *)
  let rec loop worklist =
    match worklist with
    | [] ->
      true
    | (s1, s2, pk) :: worklist ->
       (* Printf.printf "----- CHECKING [state_%Ld] [state_%Ld] -----\n" s1 s2;
        * Printf.printf "PKT  = %s\n" (pp pk); *)
       let conf1, conf2 = Config.of_first s1 pk, Config.of_second s2 pk in
       let class1 = Hashtbl.find_or_add rel conf1 ~default:(fun () -> 
                        Union_find.create conf1) 
       in
       let class2 = Hashtbl.find_or_add rel conf2 ~default:(fun () ->
                        Union_find.create conf2)
       in
       if Union_find.same_class class1 class2 then
         loop worklist
       else
         begin
           let e1, d1 = Hashtbl.find_exn a1.states s1 in
           let e2, d2 = Hashtbl.find_exn a2.states s2 in
           (* Printf.printf "D1   = %s\n" (Fdd.FDD.serialize d1);
            * Printf.printf "E1   = %s\n" (Fdd.FDD.serialize e1);
            * Printf.printf "D2   = %s\n" (Fdd.FDD.serialize d2);
            * Printf.printf "E2   = %s\n" (Fdd.FDD.serialize e2);            *)
           let set1 = Packet.eval_e_fdd e1 pk in
           let set2 = Packet.eval_e_fdd e2 pk in
           if not (Set.equal set1 set2) then
             begin
               (* Printf.printf "@@@@@ DIFFERENT OBSERVATIONS [state_%Ld] [state_%Ld] @@@@@\n" s1 s2;
                * Printf.printf "SET1 = %s\n" (sp set1);
                * Printf.printf "SET2 = %s\n" (sp set2);  *)
               false
             end
           else
             let () = Union_find.union class1 class2 in 
             Map.fold2
               (Packet.eval_d_fdd d1 pk)
               (Packet.eval_d_fdd d2 pk)
               ~init:worklist
               ~f:(fun ~key:pk ~data worklist ->
                 match data with
                 | `Both (s1, s2) ->
                    (* Printf.printf "BOTH: state_%Ld state_%Ld\n" s1 s2; *)
                    (s1, s2, pk)::worklist
                 | `Left s1 ->
                    (* Printf.printf "LEFT: state_%Ld state_%Ld\n" s1 dead2; *)
                    (s1,dead2,pk)::worklist
                 | `Right s2 ->
                    (* Printf.printf "RIGHT: state_%Ld state_%Ld\n" dead1 s2;               *)
                    (dead1,s2,pk)::worklist
               )
             |> loop
         end       
  in
  loop worklist
    
