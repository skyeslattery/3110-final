type t = {
  mutable pos : float * float;
  vel : float * float;
  style : int;
}

val adjust_position : 'a -> float -> 'a * float
val create_dec : float -> float -> float -> float -> t
val update_dec : t -> float -> float -> t
val get_dec_x : t -> float
val get_dec_y : t -> float
val get_dec_pos : t -> float * float
val get_dec_vel : t -> float * float
val get_dec_type : t -> int
