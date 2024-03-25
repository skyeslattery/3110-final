open OcamlCanvas.V1

type t = {
  mutable pos : Point.t;
  vel : float * float;
}

let create x y vx vy = { pos = (x, y); vel = (vx, vy) }