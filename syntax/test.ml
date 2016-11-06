(* let declaration *)
let%nk x = {| hi |} and y = {| foo |}

let%nk p = {| foo |}

let test =
  (* let expression*)
  let%nk p = {| more |} and q = {| more |} in
  p

(* in *)
(* SJS: the latter form will be rejected *)
(* let%nk x = "hi" in  *)
(* let%nk y = "hi" in *)
(* "ciao" *)
(*  *)