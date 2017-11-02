open Core
open Probnetkat
open Fdd

let fprintf = Format.fprintf
let fmt = Format.std_formatter
(*
module Dense = Owl.Dense.Matrix.D
module Sparse = Owl.Sparse.Matrix.D

let print_dom_size n dom = begin
  printf "size of state space = %s" (Int.to_string_hum n);
  if Map.length dom > 1 then
    Map.data dom
    |> List.map ~f:(fun vs -> Int.to_string (Set.length vs + 1))
    |> String.concat ~sep:" x "
    |> printf " (%s)";
  printf "\n\n%!";
end


let run ?(print=true) ?(lbl=true) ?(transpose=false) ?(debug=false) ?(verbose=false) p =
  printf "\n========================= EIGEN ==========================\n\n%!";
  fprintf fmt "policy = %a\n\n%!" pp_policy p;
  let dom = domain p in
  let module Repr = Packet_Repr.Make(struct let domain = dom end) in
  let n = Repr.Index0.max.i + 1 in
  print_dom_size n dom;
  let module Mc = Markov_chain.MakeOwl(Repr) in
  if print && not lbl then begin
    fprintf fmt "index packet mapping:\n%!";
    Array.init n ~f:ident
    |> Array.iter ~f:(fun i -> fprintf fmt " %d = %a\n%!" i Repr.Index0.pp' i);
    fprintf fmt "\n%!";
  end;
  let (t, mc) = time (Mc.of_pol ~debug ~verbose) p in
(*   if print then begin (if transpose then Sparse.transpose else ident) mc |> Sparse.to_dense |>
    Format.printf "@[MATRIX:@\n%a@\n@]@."
      (if not lbl then Owl_pretty.pp_fmat else
         Owl_pretty.pp_labeled_fmat
          ~pp_left:(Some (fun fmt -> fprintf fmt "%a|" Repr.Index.pp'))
          ~pp_head:None
          ~pp_foot:None
          ~pp_right:None ())
  end; *)
  print_time t;
  ()

let run' ?(print=true) ?(lbl=true) ?(debug=false) p =
  printf "\n========================== BLAS ==========================\n\n%!";
  fprintf fmt "policy = %a\n\n%!" pp_policy p;
  let dom = domain p in
  let module Repr = Packet_Repr.Make(struct let domain = dom end) in
  let module Mc = Markov_chain.MakeLacaml(Repr) in
  let n = Repr.Index.max.i in
  print_dom_size n dom;
  if print && not lbl then begin
    fprintf fmt "index packet mapping:\n%!";
    Array.init n ~f:ident
    |> Array.iter ~f:(fun i -> fprintf fmt " %d = %a\n%!" i Repr.Index0.pp' i);
    fprintf fmt "\n%!";
  end;
  let (t, mc) = time (Mc.of_pol ~debug) p in
  let mc =
    match mc with
    | M m -> assert Lacaml.D.Mat.(dim1 m = n && dim2 m = n); m
    | V v -> assert Lacaml.D.Vec.(dim v = n); Lacaml.D.Mat.of_diag v
  in
  if print then begin mc |>
  Format.printf "@[<2>MATRIX:@\n%a@\n@]@."
    (Lacaml.Io.pp_lfmat
      ~row_labels:
        (Array.init n (fun i -> Format.asprintf "%a%!" Repr.Index0.pp' i))
      ~ellipsis:"*"
      ~print_right:false
      ~print_foot:false ())
  end;
  print_time t;
  ()

let () = begin
(*   printf "Hello, World!\n";
  printf "Here is a nice matrix:\n\n%a\n" pp_mat mat;
  let pk = Field.Map.empty in
  printf "codepoint = %d\n" (Codepoint.(of_pk pk) :> int);
  printf "max codepoint = %d\n" (Codepoint.max :> int);
  ignore (Codepoint.to_pk Codepoint.max); *)
  (* let open Probnetkat.Syntax in *)
  let open Probnetkat.Syntax.Smart in
  let pwhile n =
    mk_while ??("f", 0) ?@[
      skip       @ (n-1)//n;
      !!("f", 1) @ 1//n
    ]
  in
  let qwhile n =
    mk_while (neg ??("f", 1)) ?@[
      skip       @ (n-2)//n;
      !!("f", 1) @ 1//n;
      !!("f", 2) @ 1//n
    ]
  in
  (** this while loop is singular: it never terminates  *)
  let pwhile' =
    mk_while ??("f", 0) ?@[
      skip       @ 1//1;
      !!("f", 1) @ 0//1
    ]
  in
  let dependent =
    ?@[ !!("f", 1) @ 1//2; !!("f", 2) @ 1//2] >>
    ite ??("f", 1)  !!("g", 1) !!("g", 2)
  in
  let blowup n =
    seqi n ~f:(fun i -> let l = sprintf "l%d" i in
                ?@[ !!(l,0) @ 1//2; !!(l,1) @ 1//2])
  in

  let kfail n k =
    failwith "todo"
  in
  run skip;
  run drop;
  run (??("f", 0));
  run (mk_while ??("f", 0) !!("f", 1));
  run (pwhile 10);
  run (neg ??("f", 1));
  run ~debug:false (qwhile 10);
  run' ~debug:false (qwhile 10);
  run (blowup 9) ~print:false ~debug:true;
  (* run (pwhile 100);
  run (pwhile 100_000_000);
  try run pwhile' with e -> printf "%s\n" (Exn.to_string e);
  run dependent;
  run (blowup 3);
  run (blowup 9) ~print:false;

  run' skip;
  run' drop;
  run' (??("f", 0));
  run' (mk_while ??("f", 0) !!("f", 1));
  run' (pwhile 10);
  run' (pwhile 100);
  run' (pwhile 100_000_000);
  try run' pwhile' with e -> printf "%s\n" (Exn.to_string e);
  run' dependent;
  run' (blowup 3);
  run' (blowup 7) ~print:false; *)

  (* run' (mk_while ??("f", 0) !!("f", 1)); *)
(*   let uniform =
    seqi 4 ~f:(fun i -> let l = sprintf "l%d" i in
                ?@[!!(l,0), Q.(1//2);
                   !!(l,1), Q.(1//2)])
    >>
    mk_while (
      seqi 4 ~f:(fun i -> let l = sprintf "l%d" i in
                          let t = sprintf "take%d" i in
                          )) *)

end *)
