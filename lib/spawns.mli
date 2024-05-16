(* The initial speed of the obstacles. *)
val speed : float

(* The factor by which the speed increases as time moves on. *)
val speed_increase_factor : float

(* Calculates the interval between obstacle spawns given the current score. *)
val ob_spawn_interval : float -> float

(* Calculates the interval between grass spawns given the current score. *)
val grass_spawn_interval : float -> float

(* Calculates the interval between bump spawns given the current score. *)
val bump_spawn_interval : float -> float

(* Calculates the interval between clopud spawns given the current score. Less
   frequent than obstacles.*)
val cloud_spawn_interval : float -> float

(* Calculates the interval between spawns given the current score. Less frequent
   than obstacles. *)
val star_spawn_interval : float -> float
