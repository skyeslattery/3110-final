let ob_min_spawn_interval = 8.
let ob_max_spawn_interval = 9.
let dec_min_spawn_interval = 2.
let dec_max_spawn_interval = 4.
let speed = 170.

(* Factor by which obstacle speed increases with score *)
let speed_increase_factor = 1.05

let ob_spawn_interval score =
  let min_interval =
    ob_min_spawn_interval /. (speed_increase_factor ** (score /. 50.))
  in
  let max_interval =
    ob_max_spawn_interval /. (speed_increase_factor ** (score /. 50.))
  in
  Random.float (max_interval -. min_interval) +. min_interval

let grass_spawn_interval score =
  let min_interval =
    dec_min_spawn_interval /. (speed_increase_factor ** (score /. 30.))
  in
  let max_interval =
    dec_max_spawn_interval /. (speed_increase_factor ** (score /. 30.))
  in
  Random.float (max_interval -. min_interval) +. min_interval

let bump_spawn_interval score =
  let min_interval =
    dec_min_spawn_interval *. 4. /. (speed_increase_factor ** (score /. 90.))
  in
  let max_interval =
    dec_max_spawn_interval *. 4. /. (speed_increase_factor ** (score /. 90.))
  in
  Random.float (max_interval -. min_interval) +. min_interval

let cloud_spawn_interval score =
  let min_interval =
    dec_min_spawn_interval *. 12. /. (speed_increase_factor ** (score /. 50.))
  in
  let max_interval =
    dec_max_spawn_interval *. 12. /. (speed_increase_factor ** (score /. 50.))
  in
  Random.float (max_interval -. min_interval) +. min_interval

let star_spawn_interval score =
  let min_interval =
    dec_min_spawn_interval *. 18. /. (speed_increase_factor ** (score /. 50.))
  in
  let max_interval =
    dec_max_spawn_interval *. 18. /. (speed_increase_factor ** (score /. 50.))
  in
  Random.float (max_interval -. min_interval) +. min_interval
