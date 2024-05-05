type cactus_type =
  | Short
  | Tall
  | Normal

type t = {
  mutable pos : float * float;
  vel : float * float;
  style : cactus_type;
}

val adjust_position : float -> float -> int -> float * float
val create_obstacle : float -> float -> float -> float -> t
val update_obstacle : t -> float -> float -> t
val get_x : t -> float
val get_y : t -> float
val get_pos : t -> float * float
val get_vel : t -> float * float
val get_type : t -> int
val get_height : t -> int
val get_width : t -> int
