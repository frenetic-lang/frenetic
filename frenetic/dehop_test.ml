(* KATNetic is a more civilized name than what Nate suggested. *)
open Types
module Example = struct
  open Dehop

  let rec par_list lst =
    match lst with
      | [] -> Filter False
      | a :: lst -> Par(a, par_list lst)

  let rec seq_list lst =
    match lst with
      | [] -> Filter True
      | a :: lst -> Seq(a, seq_list lst)

  (* Test policy/topology *)
  (*  1 
     / \
    2   3
     \ /
      4
  *)


  (* let test_pol =  *)
  (*   let s0 = VInt.Int16 0 in *)
  (*   let s1 = VInt.Int16 1 in *)
  (*   let p1 = VInt.Int16 1 in *)
  (*   let p2 = VInt.Int16 2 in *)
  (*   Seq(Filter (Test (Switch, s0)), *)
  (*       Par(Mod (SDN_Types.InPort, p1), *)
  (*           Mod (SDN_Types.InPort, p2))) *)

  let test_pol =
    let s0 = VInt.Int16 0 in
    let s1 = VInt.Int16 1 in
    let _ (* s2 *) = VInt.Int16 2 in
    let _ (* s3 *) = VInt.Int16 3 in
    let p0 = VInt.Int16 0 in
    let _ (* p1 *) = VInt.Int16 1 in
    let _ (* p2 *) = VInt.Int16 2 in
    let t = Link(s0, p0, s1, p0)
    in
    Seq (Filter (Test (Switch, s0)),
              t)
              (* Filter (Test (Switch, s1)); *)
              (* Mod (Header SDN_Types.InPort, p1) *)
end

open Dehop

let () =
  Printf.printf "test_pol: %s\n%!" (Pretty.string_of_policy Example.test_pol);
  let i,s,t,e = dehop_policy Example.test_pol in
  Printf.printf "%s\n%!" (Pretty.string_of_policy s)
