open Ocamlbuild_plugin;;
open Command;;

let packages = "oUnit" (* "pkg1,pkg2,..." *);;

let ocamlfind cmd =
  S[A"ocamlfind"; A cmd; A"-package"; A packages];;

flag ["ocaml"; "link"] (A"-linkpkg");;

dispatch begin function
  | After_options ->
      Options.ocamlc := ocamlfind "ocamlc";
      Options.ocamlopt := ocamlfind "ocamlopt";
  | _ -> ()
end
