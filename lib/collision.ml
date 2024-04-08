open Obstacle
open Player

let collision player (obstacle : Obstacle.t) ob_height ob_width =
  let pl_x, pl_y = player.pos in
  let ob_x, ob_y = obstacle.pos in
  (* Slightly lenient hitbox detection*)
  let pl_width = 33. in
  let pl_height = 27. in
  (* Check if any of the player's edges are within the obstacle's edges *)
  pl_x +. pl_width >= ob_x
  && pl_x <= ob_x +. ob_width
  && pl_y +. pl_height >= ob_y
  && pl_y <= ob_y +. ob_height
