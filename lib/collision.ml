open Obstacle
open Player

let collision player (obstacle :Obstacle.t) =
  let (pl_x, pl_y) = player.pos in
  let (ob_x, ob_y) = obstacle.pos in
  (* Slightly leniant hitbox detection*)
  let pl_width = 17. in  
  let pl_height = 17. in  
  let ob_width = 47. in   
  let ob_height = 47. in  
  pl_x +. pl_width >= ob_x && ob_x +. ob_width >= pl_x &&
  pl_y +. pl_height >= ob_y && ob_y +. ob_height >= pl_y