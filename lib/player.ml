open OcamlCanvas.V1

type t = {
  mutable pos : float * float;
  mutable vel : float * float;
}

let create_player () = { pos = (50., 173.); vel = (0., 0.) }

let update_player player x y vx vy =
  player.pos <- (x, y);
  player.vel <- (vx, vy);
  player

let jump_impulse = 95.
let jump t = t.vel <- (fst t.vel, snd t.vel -. jump_impulse)

let draw_box c player =
  let x, y = player.pos in
  Canvas.fillRect c ~pos:(x, y) ~size:(35., 30.)

let get_x player = fst player.pos
let get_y player = fst player.pos
let get_vx player = fst player.vel
let get_vy player = fst player.vel
