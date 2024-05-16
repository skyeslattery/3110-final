type t = {
  mutable pos : float * float;
  mutable vel : float * float;
  mutable is_alive : bool;
  mutable best_score : int;
}

val create_player : int -> t
val update_player : t -> float -> float -> float -> float -> t
val jump_impulse : float
val jump : t -> unit
val draw_box : OcamlCanvas.V1.Canvas.t -> t -> unit
val get_pl_pos : t -> float * float
val get_x : t -> float
val get_y : t -> float
val get_vx : t -> float
val get_vy : t -> float
val is_alive : t -> bool
val best_score : t -> int
