open Core
open Probnetkat
open Syntax
open Dist
open Symbolic
open Packet_Repr

let fprintf = Format.fprintf
let fmt = Format.std_formatter

let run p =
  Fdd.of_pol p
  |> Fdd.to_string
  |> printf "%s\n"


module Dense = Owl.Dense.Matrix.D
module Sparse = Owl.Sparse.Matrix.D

let run ?(print=true) ?(debug=false) ?(verbose=false) p =
  printf "\n========================= Probnetkat ==========================\n%!";
  fprintf fmt "policy = %a\n\n%!" pp_policy p;
  let fdd = Symbolic.Fdd.of_pol p in
  if print then fprintf fmt "fdd = %s\n\n%!" (Symbolic.Fdd.to_string fdd);
(*   if print then begin
    let dom = Symbolic.Domain.of_fdd fdd in
    let module Repr = Symbolic.Coding(struct let domain = dom end) in
    let repr = (module Repr : CODING) in
    Repr.print();
    let matrix = Matrix.of_fdd fdd repr in
    Owl.Sparse.Matrix.Generic.pp_spmat matrix.matrix;
  end; *)
  ()

let blowup n =
  let open PNK in
  seqi n ~f:(fun i -> let l = sprintf "l%d" i in
              ?@[ !!(l,0) @ 1//2; !!(l,1) @ 1//2])


let blowup' m n =
  let open PNK in
  let field i = sprintf "F%d" i in
  seqi m ~f:(fun i ->
    ite (???(field 0, i)) (
      uniform n ~f:(fun j ->
        !!(field (i+1), j)
      ) >>
      !! (field 0, (i+1) mod m)
    ) (
      skip
    )
  )
  |> whl (conji m ~f:(fun i -> ???(field (i+1), 0)) |> neg)
  |> (fun p -> (!!(field 0, 0)) >> p >> (!!(field 0, 0)))

let () = begin
  let open PNK in

  (* run (??("sw", 1) >> !!("port", 1)); *)
(* 
  run (?@[ !!("x", 1) @ 1//2 ;
           !!("x", 2) @ 1//2 ] >>
       ?@[ !!("y", 1) @ 1//2 ;
           !!("y", 2) @ 1//2 ]
      );
 *)
  
  (* run ~print:false ( blowup' 10 2 ) *)

  (* run ~print:false (blowup 15); *)
  (* run ~print:false (blowup 20); *)

  run ~print:true (blowup' 5 2);

  (* run (While (True, skip)); *)
(*
  run(
    neg (???("X", 0)) |> filter
  ) *)

end

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
    whl ??("f", 0) ?@[
      skip       @ (n-1)//n;
      !!("f", 1) @ 1//n
    ]
  in
  let qwhile n =
    whl (neg ??("f", 1)) ?@[
      skip       @ (n-2)//n;
      !!("f", 1) @ 1//n;
      !!("f", 2) @ 1//n
    ]
  in
  (** this while loop is singular: it never terminates  *)
  let pwhile' =
    whl ??("f", 0) ?@[
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
  run (whl ??("f", 0) !!("f", 1));
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
  run' (whl ??("f", 0) !!("f", 1));
  run' (pwhile 10);
  run' (pwhile 100);
  run' (pwhile 100_000_000);
  try run' pwhile' with e -> printf "%s\n" (Exn.to_string e);
  run' dependent;
  run' (blowup 3);
  run' (blowup 7) ~print:false; *)

  (* run' (whl ??("f", 0) !!("f", 1)); *)
(*   let uniform =
    seqi 4 ~f:(fun i -> let l = sprintf "l%d" i in
                ?@[!!(l,0), Q.(1//2);
                   !!(l,1), Q.(1//2)])
    >>
    whl (
      seqi 4 ~f:(fun i -> let l = sprintf "l%d" i in
                          let t = sprintf "take%d" i in
                          )) *)

end *)
