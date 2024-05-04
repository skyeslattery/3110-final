open Camelgo.Menu (* Import the menu module instead of the game module *)
open OcamlCanvas.V1

let () =
  Backend.init ();
  Random.self_init ();
  start_menu ()
