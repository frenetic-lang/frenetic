(* See OUnitHack.ml *)
let _ =
  Pa_ounit_lib.Runtime.unset_lib "dummy";
  Pa_ounit_lib.Runtime.set_lib "netkat"


TEST "filter true" =
  <:netkat<filter true>> = NetKAT_Types.(Filter True)

TEST "filter switch" =
  <:netkat<filter (switch = 100L)>> = NetKAT_Types.(Filter (Test (Switch 100L)))

TEST "or filter" =
  <:netkat<filter (port = 0x200l || port = 1l)>> =
  NetKAT_Types.(Filter (Or (Test (Location (Physical 0x200l)),
                            Test (Location (Physical 1l)))))

TEST "antiquote" =
  let f (x : NetKAT_Types.policy) = <:netkat< $x$ >> in
  f <:netkat<filter true>> = NetKAT_Types.(Filter True)

let _ = Pa_ounit_lib.Runtime.summarize ()
