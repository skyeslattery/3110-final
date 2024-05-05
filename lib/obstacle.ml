type cactus_type =
  | Short
  | Tall
  | Normal

type t = {
  mutable pos : float * float;
  vel : float * float;
  style : cactus_type;
}

let adjust_position x y style =
  match style with
  | 0 -> (x +. 3., y +. 3.)
  | 1 -> (x -. 9., y -. 9.)
  | _ -> (x -. 12., y -. 12.)

let create_obstacle x y vx vy =
  let rand = Random.int 3 in
  {
    pos = adjust_position x y rand;
    vel = (vx, vy);
    style = (if rand = 0 then Short else if rand = 1 then Tall else Normal);
  }

let update_obstacle ob x y =
  ob.pos <- (x, y);
  ob

let get_x ob = fst ob.pos
let get_y ob = snd ob.pos
let get_pos ob = ob.pos
let get_vel ob = ob.vel

let get_type ob =
  match ob.style with
  | Short -> 0
  | Normal -> 1
  | Tall -> 2

let get_height ob =
  match ob.style with
  | Short -> 9
  | Tall -> 31
  | _ -> 22

let get_width ob =
  match ob.style with
  | Short -> 31
  | Tall -> 14
  | _ -> 15
