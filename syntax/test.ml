(* let declaration *)
let%nk p = {| drop |}
let%nk q = {| filter true; $p; (port:=2 + port:=pipe("test") ) |}
let%nk egress = {| filter (switch=1 and port=1) |}
let%nk r = {| while !$egress do $q |}