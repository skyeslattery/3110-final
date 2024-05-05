val gravity_acceleration : float
val events : unit React.event list ref
val grounded : bool ref
val retain_event : unit React.event -> unit
val clear_events : unit -> unit
val obstacles : Obstacle.t list ref
val decorations : Decorations.t list ref
val score : float ref

type state = { mutable image_opt : OcamlCanvas.V1.Canvas.t option }

val bg_img : state
val player_img : state
val camel_images : state array
val grass1_img : state
val grass2_img : state
val cactus_short_img : state
val cactus_tall_img : state
val cactus_normal_img : state
val start : int -> (int -> unit) -> 'a
