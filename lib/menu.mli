val events : unit React.event list ref
val retain_event : unit React.event -> unit
val clear_events : unit -> unit

type bg_state = { mutable image_opt : OcamlCanvas.V1.Canvas.t option }

val bg_img : bg_state

type menu_state = {
  mutable best_score : int;
  mutable is_active : bool;
}

val initialize_menu_state : int -> menu_state
val start_menu : int -> (int -> unit) -> 'a
