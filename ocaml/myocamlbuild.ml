(* From https://sympa.inria.fr/sympa/arc/caml-list/2010-06/msg00172.html *)

(* Configuration 
**************************************************************)

(* Set to the list of directories containing *.mlpack files. *)
let mlpack_dirs = ["."]

(* Ocamlbuild Plugin Code 
*****************************************************)

open Ocamlbuild_plugin

module PackageLinkFix =
struct
  let packages_in_dir dir =
    Array.fold_right
      (fun f l ->
         if (Pathname.check_extension f "mlpack") then
           (dir / (Pathname.remove_extension f)) :: l
         else
           l)
      (Sys.readdir dir)
      []

  let byte_dep_mlpack arg out env _build =
    let arg = env arg and out = env out in
      Echo (([arg; ":"] @
               (List.map
                  (fun s -> " "^s)
                  (string_list_of_file (arg))) @
               ["\n"]), out)

  let after_rules () =
    begin
      rule "ocaml dependencies mlpack"
        ~prod:"%.ml.depends"
        ~dep:"%.mlpack"
        (byte_dep_mlpack "%.mlpack" "%.ml.depends")
      ;
      List.iter
        (fun p ->
           dep
             ["ocaml"; "byte"; "pack"; "extension:cmo"; "file:"^p^".cmo"]
             [p^".ml.depends"])
        (List.concat (List.map packages_in_dir (List.map Pathname.mk 
mlpack_dirs)))
    end
end

;;

dispatch
  begin
    function
        After_rules -> PackageLinkFix.after_rules ()
      | _ -> ()
  end
