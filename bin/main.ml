(* cloc --by-file --include-lang=OCaml . *)
open Camelgo.Main_controller
open OcamlCanvas.V1

let () =
  Backend.init ();
  Random.self_init ();
  start_app ()
