type t = {
  mutable pos : float * float;
  vel : float * float;
  style : int;
}

let adjust_position x y = (x, y +. 10.)

let create_dec x y vx vy =
  let rand = Random.int 2 in
  let pos = adjust_position x y in
  { pos; vel = (vx, vy); style = (if rand = 0 then 0 else 1) }

let update_dec dec x y =
  dec.pos <- (x, y);
  dec

let get_ob_x dec = fst dec.pos
let get_ob_y dec = snd dec.pos
let get_ob_pos dec = dec.pos
let get_ob_vel dec = dec.vel
let get_ob_type dec = dec.style
