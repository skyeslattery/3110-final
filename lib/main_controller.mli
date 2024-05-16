(* The reference for the current score, checked each game. *)
val current_score : int ref

(* Starts the main game loop *)
val start_game : unit -> unit

(* Starts the main menu loop *)
val start_menu : unit -> unit

(* A function that is passed to game that will be executed upon finishing the
   game. *)
val game_finished : int -> unit

(* A function that is passed to menu that will be executed upon exiting the
   menu. *)
val menu_finished : int -> unit

(* Starts the entire program *)
val start_app : unit -> unit
