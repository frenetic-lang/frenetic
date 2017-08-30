open Core
open Frenetic_Nominal

let () = begin
  let%nk barbell = {|
    filter port=1; port:=3; 1@3=>3@3; port:=1 +
    filter port=2; port:=3; 1@3=>3@3; port:=2
  |} in
  let%nk pol = {| filter switch = 1; port := 2 |} in
  Automaton.of_pol barbell
  |> Automaton.to_yojson
  |> printf "%a" (Yojson.Safe.pretty_to_channel ~std:false)
end
