type t = {
  mutable pos : float * float;
  vel : float * float;
  style : int;
}

val adjust_position : 'a -> float -> 'a * float
val create_dec : float -> float -> float -> float -> t
val update_dec : t -> float -> float -> t
val get_ob_x : t -> float
val get_ob_y : t -> float
val get_ob_pos : t -> float * float
val get_ob_vel : t -> float * float
val get_ob_type : t -> int
