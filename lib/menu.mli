(* List of events to process in the game. *)
val events : unit React.event list ref

(* TBA *)
val retain_event : unit React.event -> unit

(* Gets rid of all active events. *)
val clear_events : unit -> unit

(* State of the background. *)
type bg_state = { mutable image_opt : OcamlCanvas.V1.Canvas.t option }

(* Value representing the active background. *)
val bg_img : bg_state

(* The type of the state of the menu. *)
type menu_state = {
  mutable best_score : int;
  mutable is_active : bool;
}

(* Initializes the menu state. *)
val initialize_menu_state : int -> menu_state

(* Creates the menu itself. *)
val start_menu : int -> (int -> unit) -> 'a
