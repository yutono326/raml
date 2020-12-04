open Ocamlbuild_plugin
open Command

let clp_inc_dir = Sys.getenv "INCDIRS"
let clp_lib_dir = Sys.getenv "LIBDIRS"

let clp_stubs = Filename.concat "clp" "clp_stubs.o"

let flag_clp_include = [A "-ccopt"; A ("-O2" ^ " -Wall" ^ " -I" ^ clp_inc_dir)]
let flag_clp_link =
  [A clp_stubs] @
  [A "-cclib"; A ("-L" ^ clp_lib_dir ^ " -lClp" ^ " -lCoinUtils")]

let () =
  dispatch begin function
    | After_rules ->
      dep ["compile"; "ocaml"; "use_clp"] [clp_stubs];

      flag ["compile"; "c"; "use_clp"] (S flag_clp_include);
      flag ["link"; "ocaml"; "native"; "use_clp"] (S flag_clp_link);
      flag ["link"; "ocaml"; "byte"; "use_clp"] (S (A "-custom" :: flag_clp_link));

    | _ -> ()
  end
