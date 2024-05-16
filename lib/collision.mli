(* Returns [true] if the player has collided with a particular obstacle. *)
val collision : Player.t -> Obstacle.t -> float -> float -> bool
