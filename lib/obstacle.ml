open OcamlCanvas.V1

type t = {
  mutable pos : float * float;
  vel : float * float;
}

let create_obstacle x y vx vy = { pos = (x, y); vel = (vx, vy) }

let update_obstacle ob x y =
  ob.pos <- (x, y);
  ob

let draw_obstacle c obstacle =
  let x, y = obstacle.pos in
  Canvas.fillRect c ~pos:(x, y) ~size:(20.0, 20.0)

let get_x ob = fst ob.pos
let get_y ob = snd ob.pos
let get_pos ob = ob.pos
let get_vel ob = ob.vel
