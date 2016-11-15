(* let declaration *)
let%nk p = {| drop |}
let%nk q = {| filter true; $p; (port:=2 + port:=pipe("test") ) |}
let%nk_pred egress = {| switch=1 and port=1 |}
let%nk egress' = {| filter $egress |}
let%nk r = {| let `inport := port in while !$egress do $q + drop |}

(* can have open terms *)
let%nk r = {| `inport := 1 |}

(*  *)
let%nk r = {| `aux := 192.168.2.1; filter `aux = 00:0a:95:9d:68:16 |}

(* ERRORS *)
(* let%nk s = {| filter typo = 1 |} *)
(* let%nk r = {| while !$egress' do $q |} *)
(* let%nk r = {| `inport := port |} *)