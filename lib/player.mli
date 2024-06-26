(* The type of a player object with position and velocity vectors [pos] and
   [vel], a life status [is_alive], and best recorded score [best_score]. *)
type t = {
  mutable pos : float * float;
  mutable vel : float * float;
  mutable is_alive : bool;
  mutable best_score : int;
}

(* Initializes a player with a particular score. *)
val create_player : int -> t

(* Updates the player to a new position with a new velocity. *)
val update_player : t -> float -> float -> float -> float -> t

(* Value representing the impulse for the player jumps. *)
val jump_impulse : float

(* Makes the player jump. *)
val jump : t -> unit

(* Draws the player as a box on the canvas. For testing. *)
(* val draw_box : OcamlCanvas.V1.Canvas.t -> t -> unit *)

(* Returns a tuple [(x, y)] representing the player's position. *)
val get_pl_pos : t -> float * float

(* Returns the x-component of the player's position. *)
val get_pl_x : t -> float

(* Returns the y-component of the player's position. *)
val get_pl_y : t -> float

(* Returns the x-component of the player's velocity. *)
val get_pl_vx : t -> float

(* Returns the y-component of the player's velocity. *)
val get_pl_vy : t -> float

(* Returns [true] if the player is still alive (i.e., hasn't hit an object
   yet). *)
val is_alive : t -> bool

(* Returns the highest score this player has been able to achieve so far. *)
val best_score : t -> int
