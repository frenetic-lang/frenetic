# Frenetic PPX Syntax Extension
The Frenetic syntax extension allows embedding NetKAT code right into OCaml:
```
let%nk fwd = {|
  (* This is NetKAT code! :) *) 
  if ip4Dst=10.0.0.1 then port := 1 else
  if ip4Dst=10.0.0.2 then port := 2 else
  port := pipe("controller")
|}
```

**Antiquotations** allow conveniently composing existing policies to build larger programs:
```
let%nk firewall = {| if `inport = 1 then drop else id |}
let%nk main = {| let `inport := port in {fwd}; {firewall} |}
```
More generally, antiquotations allow to evaluate arbitrary OCaml expressions inside a NetKAT quotation:
```
let%nk p = {| filter {<some Ocaml code evaluating to a NetKAT predicate>};
              port := {<some OCaml code evaluating to a Int32.t>};
              switch := {<some OCaml code evaluating to a Int64.t>}
           |}
```

**Iverson brackets** are a special form of antiquotations that take a boolean OCaml expression and evaluate to the NetKAT primitives
* `id` or `drop` when appearing in place of a NetKAT policy
* `true` or `false` when appearing in place of a NetKAT predicate

The syntax extension comes in 4 forms:
* let declarations:
    - let%nk ID = {| NETKAT |}
    - let%nk_pred ID = {| NETKAT PREDICATE |}
* let expressions:
    - let%nk ID = {| NETKAT |} in e
    - let%nk_pred ID = {| NETKAT PREDICATE |} in e

For more examples, see `ppx/test/main.ml`.

# How to use
To use the syntax extension in your program, simply build it with the findlib package `frenetic.ppx`. When using jbuilder, using the syntax extension is as easy as including the line
```
(preprocess (pps (frenetic.ppx <potentially other rewriters...>)))
```
in your `jbuild` file.

See `ppx/test` for a full working example including two minimal build systems:
* the `jbuild` file for using frenetic.ppx with jbuilder
* the `_oasis` and `Makefile` for using frenetic.ppx with oasis


If you have further questions, you may want to consult the [pull request introducing the syntax extension](https://github.com/frenetic-lang/frenetic/pull/527).