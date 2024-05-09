type decoration_type =
  | Star1
  | Star2
  | Grass1
  | Grass2
  | Cloud1
  | Cloud2
  | Cloud3
  | Bump1
  | Bump2

type t = {
  mutable pos : float * float;
  vel : float * float;
  style : decoration_type;
}

let adjust_position x y = (x, y +. 10.)

let create_star x y vx vy =
  let rand = Random.int 2 in
  let pos = (x, y) in
  { pos; vel = (vx, vy); style = (if rand = 0 then Star1 else Star2) }

let create_cloud x y vx vy =
  let rand = Random.int 3 in
  let pos = (x, y) in
  {
    pos;
    vel = (vx, vy);
    style = (if rand = 0 then Cloud1 else if rand = 1 then Cloud2 else Cloud3);
  }

let create_grass x y vx vy =
  let rand = Random.int 2 in
  let pos = adjust_position x y in
  { pos; vel = (vx, vy); style = (if rand = 0 then Grass1 else Grass2) }

let create_bump x y vx vy =
  let rand = Random.int 2 in
  let pos = (x, y +. 14.) in
  { pos; vel = (vx, vy); style = (if rand = 0 then Bump1 else Bump2) }

let update_dec dec x y =
  dec.pos <- (x, y);
  dec

let get_dec_x dec = fst dec.pos
let get_dec_y dec = snd dec.pos
let get_dec_pos dec = dec.pos
let get_dec_vel dec = dec.vel

let get_dec_type dec =
  match dec.style with
  | Grass1 -> 0
  | Grass2 -> 1
  | Star1 -> 2
  | Star2 -> 3
  | Cloud1 -> 4
  | Cloud2 -> 5
  | Cloud3 -> 6
  | Bump1 -> 7
  | Bump2 -> 8
