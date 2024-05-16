(* The type of a cactus obstacle style (not to be confused with the object itself). *)
type cactus_type =
  | Short
  | Tall
  | Normal

(* The type of a cactus obstacle with position and velocity vectors [pos] and [vel] and 
  style [style] (affects hitbox detection). *)
type t = {
  mutable pos : float * float;
  vel : float * float;
  style : cactus_type;
}

(* Increments an inputted position based on the style of the cactus. *)
val adjust_position : float -> float -> int -> float * float

(* Initializes a cactus obstacle with a specified position and velocity. *)
val create_obstacle : float -> float -> float -> float -> t

(* Updates the position of a cactus obstacle. *)
val update_obstacle : t -> float -> float -> t

(* Returns the x-component of a cactus's position. *)
val get_x : t -> float

(* Returns the y-component of a cactus's position. *)
val get_y : t -> float

(* Returns a tuple [(x, y)] representing the cactus's position. *)
val get_pos : t -> float * float

(* Returns a tuple [(x, y)] representing the cactus's velocity. *)
val get_vel : t -> float * float

(* Returns an int representing a cactus's style. *)
val get_type : t -> int

(* Returns the height of a cactus. (Varies with style.) *)
val get_height : t -> int

(* Returns the width of a cactus. (Varies with style.) *)
val get_width : t -> int
