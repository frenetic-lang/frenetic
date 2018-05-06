open! Core
open! Expect_test_helpers_kernel


(* let declaration *)
let%nk p = {| drop |}
let%nk q = {| true; {p}; (port:=2 + port:=pipe("test") ) |}
let%nk_pred egress = {| switch=1 and port=1 |}
(* the `filter` keyword is necessary here to force the parser to interpret
   {egress} as a predicate, not a policy.
*)
let%nk egress' = {| filter {egress} |}
let%nk loop = {| let `inport := port in while not {egress} do {q} + drop |}

(* let expressions *)
let letin =
  let%nk s = {| {p}; ({q} + {loop}) |} in
  let s' = [%nk {| {p}; ({q} + {loop}) |}] in
  let open Frenetic.Netkat.Optimize in
  mk_seq s s'

(* constant string expressions *)
let p = [%nk "drop"]
let q = [%nk {| true; {q}; (port:=2 + port:=pipe("test") ) |}]

(* can have open terms *)
let%nk opent = {| `inport := 1 |}

(* can use IP and MAC accresses with meta fields *)
let%nk addresses = {| `aux := 192.168.2.1; `aux = 00:0a:95:9d:68:16 |}
(* maximum addresses *)
let%nk ip_and_mac = {| `ip := 255.255.255.0; `mac := ff:ff:ff:ff:ff:ff |}
(* above maximum, but still accepted by parser currently *)
let%nk illegal = {| `ip := 255.255.255.255 |}

(* iverson bracket's allow one to use OCaml predicates right in NetKAT *)
let%nk iverson = {| [2 = 1+1]; port:=pipe("true") + [2=1]; port:=pipe("false")  |}
let%nk iverson_pred = {| [2 > 1]; [2 < 1] |}

(* advanced iverson examples *)
let mk_link adj a b =
  let src,dst = Int32.(of_int_exn adj.(a).(b), of_int_exn adj.(b).(a)) in
  let a,b = Int64.(of_int a + 1L, of_int b + 1L) in
  let%nk l = {| [src <> 0l]; (switch={a} and port={src}); switch:={b}; port:={dst} |} in
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
(* let%nk s = {| typo = 1 |} *)
(* let%nk r = {| while not {egress'} do {q} |} *)
(* let%nk r = {| `inport := port |} *)
(* let%nk iverson_pred = {| [2 > 1]; [2 < ( 1] |} *)
(* let%nk iverson_pred = {| [2 > 1]; |} *)



(* Regression tests below... *)


let test p =
  [%sexp_of: Frenetic.Netkat.Syntax.policy] p
  |> Sexp.to_string (* _hum *)
  |> print_endline

let test' p =
  print_endline (Frenetic.Netkat.Pretty.string_of_policy p)

let test_pred p =
  [%sexp_of: Frenetic.Netkat.Syntax.pred] p
  |> Sexp.to_string (* _hum *)
  |> print_endline

let%expect_test "p" = test p;
  [%expect {| (Filter False) |}]

let%expect_test "q" = test q;
  [%expect {|
    (Seq(Seq(Filter True)(Seq(Seq(Filter True)(Filter False))(Union(Mod(Location(Physical 2)))(Mod(Location(Pipe test))))))(Union(Mod(Location(Physical 2)))(Mod(Location(Pipe test))))) |}]

let%expect_test "egress" = test_pred egress;
  [%expect {| (And(Test(Switch 1))(Test(Location(Physical 1)))) |}]

let%expect_test "egress'" = test egress';
  [%expect {| (Filter(And(Test(Switch 1))(Test(Location(Physical 1))))) |}]

let%expect_test "loop" = test' loop;
  [%expect {|
    let inport := Location in (filter not (switch = 1 and port = 1);
                               (id; drop; (port := 2 + port := pipe("test"))))*;
                              filter not not (switch = 1 and port = 1) +
                              drop |}]

let%expect_test "letin" = test' letin;
  [%expect {|
    drop;
    (id; drop; (port := 2 + port := pipe("test")) +
     let inport := Location in (filter not (switch = 1 and port = 1);
                                (id; drop; (port := 2 + port := pipe("test"))))*;
                               filter not not (switch = 1 and port = 1) +
                               drop);
    (drop;
     (id; drop; (port := 2 + port := pipe("test")) +
      let inport := Location in (filter not (switch = 1 and port = 1);
                                 (id; drop; (port := 2 + port := pipe("test"))))*;
                                filter not not (switch = 1 and port = 1) +
                                drop)) |}]

let%expect_test "opent" = test opent;
  [%expect {| (Mod(Meta inport 1)) |}]

let%expect_test "addresses" = test addresses;
  [%expect {| (Seq(Mod(Meta aux -1062731263))(Filter(Test(Meta aux 45459793942)))) |}]

let%expect_test "ip_and_mac" = test ip_and_mac;
  [%expect {|
    (Seq(Mod(Meta ip -256))(Mod(Meta mac 281474976710655))) |}]

let%expect_test "illegal" = test illegal;
  [%expect {| (Mod(Meta ip -1)) |}]

let%expect_test "iverson" = test iverson;
  [%expect {|
    (Union(Seq(Filter True)(Mod(Location(Pipe true))))(Seq(Filter False)(Mod(Location(Pipe false))))) |}]

let%expect_test "iverson_pred" = test iverson_pred;
  [%expect {|
    (Seq(Filter True)(Filter False)) |}]

let%expect_test "advanced_iverson" = test advanced_iverson;
  [%expect {|
    (Union(Union(Union(Union(Union(Union(Union(Union(Seq(Seq(Seq(Filter False)(Filter(And(Test(Switch 1))(Test(Location(Physical 0))))))(Mod(Switch 1)))(Mod(Location(Physical 0))))(Seq(Seq(Seq(Filter True)(Filter(And(Test(Switch 1))(Test(Location(Physical 12))))))(Mod(Switch 2)))(Mod(Location(Physical 21)))))(Seq(Seq(Seq(Filter False)(Filter(And(Test(Switch 1))(Test(Location(Physical 0))))))(Mod(Switch 3)))(Mod(Location(Physical 0)))))(Seq(Seq(Seq(Filter True)(Filter(And(Test(Switch 2))(Test(Location(Physical 21))))))(Mod(Switch 1)))(Mod(Location(Physical 12)))))(Seq(Seq(Seq(Filter False)(Filter(And(Test(Switch 2))(Test(Location(Physical 0))))))(Mod(Switch 2)))(Mod(Location(Physical 0)))))(Seq(Seq(Seq(Filter True)(Filter(And(Test(Switch 2))(Test(Location(Physical 23))))))(Mod(Switch 3)))(Mod(Location(Physical 32)))))(Seq(Seq(Seq(Filter False)(Filter(And(Test(Switch 3))(Test(Location(Physical 0))))))(Mod(Switch 1)))(Mod(Location(Physical 0)))))(Seq(Seq(Seq(Filter True)(Filter(And(Test(Switch 3))(Test(Location(Physical 32))))))(Mod(Switch 2)))(Mod(Location(Physical 23)))))(Seq(Seq(Seq(Filter False)(Filter(And(Test(Switch 3))(Test(Location(Physical 0))))))(Mod(Switch 3)))(Mod(Location(Physical 0))))) |}]
