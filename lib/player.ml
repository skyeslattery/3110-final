open OcamlCanvas.V1

type t = {
  mutable pos : float * float;
  mutable vel : float * float;
  mutable is_alive : bool;
  mutable best_score : int;
}

let create_player score =
  { pos = (50., 173.); vel = (0., 0.); is_alive = true; best_score = score }

let update_player player x y vx vy =
  player.pos <- (x, y);
  player.vel <- (vx, vy);
  player

let jump_impulse = 175.
let jump t = t.vel <- (fst t.vel, snd t.vel -. jump_impulse)

let draw_box c player =
  let x, y = player.pos in
  Canvas.fillRect c ~pos:(x, y) ~size:(35., 30.)

let get_pl_pos player = player.pos
let get_pl_x player = fst player.pos
let get_pl_y player = snd player.pos
let get_pl_vx player = fst player.vel
let get_pl_vy player = fst player.vel
let is_alive player = player.is_alive
let best_score player = player.best_score
