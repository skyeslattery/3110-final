(* Authors: Skye Slattery (dss354), Clarence Dagins (cod8), Aakshay Gupta
   (ag2226)*)
open Camelgo.Main_controller
open OcamlCanvas.V1

let () =
  Backend.init ();
  Random.self_init ();
  start_app ()
