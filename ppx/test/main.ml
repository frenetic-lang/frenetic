open Core

(* let declaration *)
let%nk p = {| drop |}
let%nk q = {| filter true; {p}; (port:=2 + port:=pipe("test") ) |}
let%nk_pred egress = {| switch=1 and port=1 |}
let%nk egress' = {| filter {egress} |}
let%nk loop = {| let `inport := port in while not {egress} do {q} + drop |}

(* let expressions *)
let letin =
  let%nk s = {| {p}; ({q} + {loop}) |} in
  let open Frenetic.Netkat.Optimize in
  mk_seq s s

(* can have open terms *)
let%nk opent = {| `inport := 1 |}

(* can use IP and MAC accresses with meta fields *)
let%nk addresses = {| `aux := 192.168.2.1; filter `aux = 00:0a:95:9d:68:16 |}
(* maximum addresses *)
let%nk ip_and_mac = {| `ip := 255.255.255.0; `mac := ff:ff:ff:ff:ff:ff |}
(* above maximum, but still accepted by parser currently *)
let%nk illegal = {| `ip := 255.255.255.255 |}

(* iverson bracket's allow one to use OCaml predicates right in NetKAT *)
let%nk iverson = {| [2 = 1+1]; port:=pipe("true") + [2=1]; port:=pipe("false")  |}
let%nk iverson_pred = {| filter [2 > 1]; [2 < 1] |}

(* advanced iverson examples *)
let mk_link adj a b =
  let src,dst = Int32.(of_int_exn adj.(a).(b), of_int_exn adj.(b).(a)) in
  let a,b = Int64.(of_int a + 1L, of_int b + 1L) in
  let%nk l = {| [src <> 0l]; filter (switch={a} and port={src}); switch:={b}; port:={dst} |} in
  l

let mk_links adj =
  let open Frenetic.Netkat.Optimize in
  let n = Array.length adj in
  List.init n ~f:(fun a -> List.init n ~f:(fun b -> mk_link adj a b))
  |> List.concat
  |> mk_big_union

let advanced_iverson = mk_links [|
  [|  0; 12;  0; |];
  [| 21;  0; 23; |];
  [|  0; 32;  0; |];
|]

(* The declarations below should cause compile-time errors with approproate
   source locations. *)
(* let%nk s = {| filter typo = 1 |} *)
(* let%nk r = {| while not {egress'} do {q} |} *)
(* let%nk r = {| `inport := port |} *)
(* let%nk iverson_pred = {| filter [2 > 1]; [2 < ( 1] |} *)
(* let%nk iverson_pred = {| filter [2 > 1]; |} *)

let () =
  let open Frenetic.Netkat.Pretty in
  let open Printf in
  printf "p = %s\n" (string_of_policy p);
  printf "q = %s\n" (string_of_policy q);
  printf "egress = %s\n" (string_of_pred egress);
  printf "egress' = %s\n" (string_of_policy egress');
  printf "loop = %s\n" (string_of_policy loop);
  printf "letin = %s\n" (string_of_policy letin);
  printf "opent = %s\n" (string_of_policy opent);
  printf "addresses = %s\n" (string_of_policy addresses);
  printf "ip_and_mac = %s\n" (string_of_policy ip_and_mac);
  printf "illegal = %s\n" (string_of_policy illegal);
  printf "iverson = %s\n" (string_of_policy iverson);
  printf "iverson_pred = %s\n" (string_of_policy iverson_pred);
  printf "advanced_iverson = %s\n" (string_of_policy advanced_iverson);
  ()
