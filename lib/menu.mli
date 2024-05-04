val events : unit React.event list ref
val retain_event : unit React.event -> unit
val clear_events : unit -> unit

type menu_state = {
  mutable best_score : int;
  mutable is_active : bool;
}

val initialize_menu_state : int -> menu_state
val start_menu : int -> (int -> unit) -> 'a
