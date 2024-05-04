type t = {
  mutable pos : float * float;
  mutable vel : float * float;
}

val create_player : unit -> t
val update_player : t -> float -> float -> float -> float -> t
val jump_impulse : float
val jump : t -> unit
val draw_box : OcamlCanvas.V1.Canvas.t -> t -> unit
val get_x : t -> float
val get_y : t -> float
val get_vx : t -> float
val get_vy : t -> float
