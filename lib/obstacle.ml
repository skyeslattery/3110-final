open OcamlCanvas.V1

type t = {
  mutable pos : float * float;
  vel : float * float;
}

let create_obstacle x y vx vy = { pos = (x, y); vel = (vx, vy) }

let update_obstacle ob x y = ob.pos <- (x, y); ob

let draw_obstacle c obstacle =
  let (x, y) = obstacle.pos in
  Canvas.fillRect c ~pos:(x, y) ~size:(20.0, 20.0)