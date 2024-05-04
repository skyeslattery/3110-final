let current_score = ref 0

let rec start_game () =
  (* Game.start should now just setup the game and notify when it's done,
     perhaps via a callback or an event that Main_controller can handle *)
  Game.start !current_score game_finished

and start_menu () =
  (* Similar setup for Menu, it should not directly start the game *)
  Menu.start_menu !current_score menu_finished

and game_finished new_score =
  current_score := new_score;
  start_menu ()

and menu_finished new_score =
  current_score := new_score;
  start_game ()

let start_app () = start_menu ()
