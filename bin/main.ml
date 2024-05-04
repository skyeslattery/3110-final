open Camelgo.Game
open OcamlCanvas.V1

let () =
  Backend.init ();
  Random.self_init ();
  start ()
