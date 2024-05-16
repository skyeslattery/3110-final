(**The type of a decoration representation whose elements are type
   decoration_type*)
type decoration_type =
  | Star1
  | Star2
  | Grass1
  | Grass2
  | Cloud1
  | Cloud2
  | Cloud3
  | Bump1
  | Bump2

type t = {
  mutable pos : float * float;
  vel : float * float;
  style : decoration_type;
}
(**The type of a decoration whose elements are type t*)

val adjust_position : 'a -> float -> 'a * float
(**Adjust position x y adjusts the *)

val create_star : float -> float -> float -> float -> t
val create_cloud : float -> float -> float -> float -> t
val create_grass : float -> float -> float -> float -> t
val update_dec : t -> float -> float -> t
val create_bump : float -> float -> float -> float -> t
val get_dec_x : t -> float
val get_dec_y : t -> float
val get_dec_pos : t -> float * float
val get_dec_vel : t -> float * float
val get_dec_type : t -> int
