(* The type of a decoration object's style (not to be confused with the object itself). *)
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

(* The type of a decoration object. *)
type t = {
  mutable pos : float * float;
  vel : float * float;
  style : decoration_type;
}

(* Increments an input position with screen scrolling. *)
val adjust_position : 'a -> float -> 'a * float

(* Creates a star decoration with a particular position and velocity. *)
val create_star : float -> float -> float -> float -> t

(* Creates a cloud decoration with a particular position and velocity. *)
val create_cloud : float -> float -> float -> float -> t

(* Creates a grass decoration with a particular position and velocity. *)
val create_grass : float -> float -> float -> float -> t

(* Updates the position of a decoration. *)
val update_dec : t -> float -> float -> t

(* Creates a bump on the ground with a particular position and velocity. *)
val create_bump : float -> float -> float -> float -> t

(* Returns the x-component of a decoration's position. *)
val get_dec_x : t -> float

(* Returns the y-component of a decoration's position. *)
val get_dec_y : t -> float

(* Returns a tuple [(x, y)] representing a decoration's position. *)
val get_dec_pos : t -> float * float

(* Returns a tuple [(x, y)] representing a decoration's velocity. *)
val get_dec_vel : t -> float * float

(* Returns the type of a particular decoration (star, cloud, etc.) as an int. *)
val get_dec_type : t -> int
