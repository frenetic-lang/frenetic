let%nk x = {| hi |} in
(* SJS: the latter form will be rejected *)
(* let%nk x = "hi" in  *)
let%nk y = "hi" in
"ciao"
