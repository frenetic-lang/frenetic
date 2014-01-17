(* This is a hack to set the name of the test library to "openflow" instead of
   "dummy", which is the default. Core ships with all its unit tests in the
   dummy library. So, if we link to core, then we first run the core test suite
   before we run ours (which takes time).

   The advertised way to set the library name is to send "-pa-ounit-lib libname" 
   as a flag to camlp4:

   https://github.com/janestreet/pa_ounit/blob/master/readme.md#building-and-running-the-tests-outside-of-jane-street

   But, this turns out to be hard/impossible to do with ocamlbuild:

   http://caml.inria.fr/mantis/view.php?id=6103

   The solution below works just fine. *)
let _ = 
  Pa_ounit_lib.Runtime.unset_lib "dummy";
  Pa_ounit_lib.Runtime.set_lib "netcore"