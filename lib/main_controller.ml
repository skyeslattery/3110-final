let current_score = ref 0

let rec start_game () = Game.start !current_score game_finished
and start_menu () = Menu.start_menu !current_score menu_finished

and game_finished new_score =
  current_score := new_score;
  start_menu ()

and menu_finished new_score =
  current_score := new_score;
  start_game ()

let start_app () = start_menu ()
