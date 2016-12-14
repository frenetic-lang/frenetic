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

*Antiquotations* allow conveniently composing existing policies to build larger programs:
```
let%nk firewall = {| if `inport = 1 then drop else id |}
let%nk main = {| let `inport := port in $fwd; $firewall |}
```

The syntax extension comes in 4 forms:
* let declarations:
    - let%nk <id> = {| <netkat> |}
    - let%nk_pred <id> = {| <netkat predicate> |}
* let expressions:
    - let%nk <id> = {| <netkat> |} in e
    - let%nk_pred <id> = {| <netkat predicate> |} in e

# How to use
To use the syntax extension in your program, you must build it with the packages `frenetic` and `frenetic.ppx`. See `ppx/test` for a full working example including a minimal build system.

For more examples, see `ppx/test/main.ml`.
If you have further questions, you may want to consult the [pull request introducing the syntax extension](https://github.com/frenetic-lang/frenetic/pull/527).