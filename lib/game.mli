(* Value representing player acceleration due to gravity. *)
val gravity_acceleration : float

(* List of active events for the GUI to process. *)
val events : unit React.event list ref

(* [true] if the player is on the ground; [false], otherwise. *)
val grounded : bool ref

(* Holds an event to be processed by the GUI. *)
val retain_event : unit React.event -> unit

(* Clears all active events. *)
val clear_events : unit -> unit

(* List of obstacles for the player to avoid. *)
val obstacles : Obstacle.t list ref

(* List of decorations on the screen. *)
val decorations : Decorations.t list ref

(* Value storing the player's score. *)
val score : float ref


(* The type of the game sprites. *)
type state = { mutable image_opt : OcamlCanvas.V1.Canvas.t option }

(* The background image. *)
val bg_img : state

(* The sprite currently representing the player. *)
val player_img : state

(* Walking sprites for the camel. *)
val camel_images : state array

(* Grass sprites for decorations. *)
val grass1_img : state
val grass2_img : state

(* Cactus sprites for obstacles. *)
val cactus_short_img : state
val cactus_tall_img : state
val cactus_normal_img : state

(* Starts the game. *)
val start : int -> (int -> unit) -> 'a
