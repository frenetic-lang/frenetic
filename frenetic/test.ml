(* KATNetic is a more civilized name than what Nate suggested. *)

module Example = struct
  open NetKAT_Types
  open SDN_Headers
  open Dehop

  let rec lpar_list lst =
    match lst with
      | [] -> Filter False
      | a :: lst -> Par(a, lpar_list lst)

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
    let s2 = VInt.Int16 2 in
    let s3 = VInt.Int16 3 in
    let p0 = VInt.Int16 0 in
    let p1 = VInt.Int16 1 in
    let p2 = VInt.Int16 2 in
    let t = lpar_list [Link(s0, p1, s1, p0)
                       (* Link(s0, p2, s2, p0); *)
                       (* Link(s1, p1, s3, p0) *)
                       (* Link(s2, p1, s3, p1) *)] in
    (* Link(s0, p1, s1, p0) *)
    seq_list [Filter (Test (Switch, s0));
              Mod (SDN_Types.InPort, p1);
              (* t; *)
              Filter (Test (Switch, s1));
              Mod (SDN_Types.InPort, p1)]
end

open Dehop

let () =
  Printf.printf "test_pol: %s\n%!" (string_of_epolicy Example.test_pol);
  let dehop = dehop_policy_opt Example.test_pol in
  Printf.printf "%s\n%!" (NetKAT_Types.string_of_policy dehop)
