open OcamlCanvas.V1
open Obstacle
open Player
open Collision
open Decorations

let gravity_acceleration = 240.
let events = ref []
let grounded = ref true
let retain_event e = events := e :: !events
let clear_events () = events := []
let obstacles = ref []
let decorations = ref []

(* Function to spawn decorations *)

let score = ref 0.

type state = { mutable image_opt : Canvas.t option }

let bg_img = { image_opt = None }
let player_img = { image_opt = None }
let camel_images = Array.init 7 (fun _ -> { image_opt = None })
(* Initialize an array for the camel images *)

let grass1_img = { image_opt = None }
let grass2_img = { image_opt = None }
let cactus_short_img = { image_opt = None }
let cactus_tall_img = { image_opt = None }
let cactus_normal_img = { image_opt = None }
let cloud1_img = { image_opt = None }
let cloud2_img = { image_opt = None }
let cloud3_img = { image_opt = None }
let star1_img = { image_opt = None }
let star2_img = { image_opt = None }
let bump1_img = { image_opt = None }
let bump2_img = { image_opt = None }

let start best_score game_finished =
  Backend.init ();
  obstacles := [];
  score := 0.0;
  grounded := true;
  decorations := [ create_grass 1000. 183. (-170.) 0. ];
  decorations :=
    create_star (Random.float 200. +. 400.) 25. (-8.5) 0. :: !decorations;
  decorations :=
    create_cloud (Random.float 50. +. 50.) 70. (-17.) 0. :: !decorations;
  decorations :=
    create_cloud (Random.float 200. +. 500.) 90. (-17.) 0. :: !decorations;
  events := [];
  let player_state = create_player best_score in

  let width = 800 in
  let height = 250 in
  let c =
    Canvas.createOnscreen ~title:"CamelGO" ~pos:(300, 200) ~size:(width, height)
      ()
  in

  Canvas.show c;

  let camel1_image = Canvas.createOffscreenFromPNG "./assets/camel1.png" in
  let camel2_image = Canvas.createOffscreenFromPNG "./assets/camel2.png" in
  let camel3_image = Canvas.createOffscreenFromPNG "./assets/camel3.png" in
  let camel4_image = Canvas.createOffscreenFromPNG "./assets/camel4.png" in
  let camel5_image = Canvas.createOffscreenFromPNG "./assets/camel5.png" in
  let camel6_image = Canvas.createOffscreenFromPNG "./assets/camel6.png" in
  let camel7_image = Canvas.createOffscreenFromPNG "./assets/camel7.png" in

  retain_event
  @@ React.E.map
       (fun img -> camel_images.(0).image_opt <- Some img)
       camel1_image;
  retain_event
  @@ React.E.map
       (fun img -> camel_images.(1).image_opt <- Some img)
       camel2_image;
  retain_event
  @@ React.E.map
       (fun img -> camel_images.(2).image_opt <- Some img)
       camel3_image;
  retain_event
  @@ React.E.map
       (fun img -> camel_images.(3).image_opt <- Some img)
       camel4_image;
  retain_event
  @@ React.E.map
       (fun img -> camel_images.(4).image_opt <- Some img)
       camel5_image;
  retain_event
  @@ React.E.map
       (fun img -> camel_images.(5).image_opt <- Some img)
       camel6_image;
  retain_event
  @@ React.E.map
       (fun img -> camel_images.(6).image_opt <- Some img)
       camel7_image;

  let draw_player canvas player_state =
    if is_alive player_state then
      if !grounded then
        let x, y = player_state.pos in
        let index = int_of_float (!score /. 2.) mod 6 in
        (* Cycle through camel images *)
        match camel_images.(index).image_opt with
        | Some image ->
            Canvas.blit ~dst:c
              ~dpos:(int_of_float x, int_of_float y)
              ~src:image ~spos:(0, 0) ~size:(40, 30);
            Canvas.show canvas
        | _ -> ()
      else
        let x, y = player_state.pos in
        match camel_images.(6).image_opt with
        | Some image ->
            Canvas.blit ~dst:c
              ~dpos:(int_of_float x, int_of_float y)
              ~src:image ~spos:(0, 0) ~size:(40, 30);
            Canvas.show canvas
        | _ -> ()
  in

  let rec update_obstacles (obstacles : Obstacle.t list) =
    match obstacles with
    | [] -> []
    | h :: t ->
        let ob_x, ob_y = get_pos h in
        let ob_vx, ob_vy = get_vel h in
        let dt = 0.033 in
        let ob_new_x = ob_x +. (ob_vx *. dt) in
        let ob_new_y = ob_y +. (ob_vy *. dt) in
        let updated_obstacle = update_obstacle h ob_new_x ob_new_y in
        updated_obstacle :: update_obstacles t
  in

  let rec update_decorations decs =
    match decs with
    | [] -> []
    | h :: t ->
        let dx, dy = get_dec_vel h in
        let x, y = get_dec_pos h in
        let dt = 0.033 in
        let new_x = x +. (dx *. dt) in
        let new_y = y +. (dy *. dt) in
        update_dec h new_x new_y :: update_decorations t
  in

  let rec check_collisions player_state obstacles =
    if is_alive player_state then
      match obstacles with
      | [] -> false
      | h :: t ->
          collision player_state h
            (float_of_int (get_height h))
            (float_of_int (get_width h))
          || check_collisions player_state t
    else false
  in

  let draw_score canvas =
    Canvas.setFillColor canvas (Color.of_rgb 27 23 27);
    Canvas.setFont canvas "Geonica" ~size:28. ~slant:Font.Roman ~weight:50;
    Canvas.setLineWidth canvas 15.;
    Canvas.fillText canvas (string_of_int (int_of_float !score)) (740., 24.)
  in

  let bg_image = Canvas.createOffscreenFromPNG "./assets/bg.png" in

  let camel1_image = Canvas.createOffscreenFromPNG "./assets/camel1.png" in

  let cactus_normal_image =
    Canvas.createOffscreenFromPNG "./assets/cactus_normal.png"
  in
  let cactus_short_image =
    Canvas.createOffscreenFromPNG "./assets/cactus_short.png"
  in
  let cactus_tall_image =
    Canvas.createOffscreenFromPNG "./assets/cactus_tall.png"
  in

  let grass1_image = Canvas.createOffscreenFromPNG "./assets/grass1.png" in

  let grass2_image = Canvas.createOffscreenFromPNG "./assets/grass2.png" in

  let cloud1_image = Canvas.createOffscreenFromPNG "./assets/cloud1.png" in
  let cloud2_image = Canvas.createOffscreenFromPNG "./assets/cloud2.png" in
  let cloud3_image = Canvas.createOffscreenFromPNG "./assets/cloud3.png" in
  let star1_image = Canvas.createOffscreenFromPNG "./assets/star1.png" in
  let star2_image = Canvas.createOffscreenFromPNG "./assets/star2.png" in
  let bump1_image = Canvas.createOffscreenFromPNG "./assets/bump1.png" in
  let bump2_image = Canvas.createOffscreenFromPNG "./assets/bump2.png" in

  let load_bg canvas =
    match bg_img.image_opt with
    | Some bg_image ->
        Canvas.blit ~dst:c ~dpos:(0, 0) ~src:bg_image ~spos:(0, 0)
          ~size:(width, height);
        Canvas.show canvas
    | _ -> ()
  in

  let draw_short c ob =
    let x, y = get_pos ob in
    match cactus_short_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(36, 14);
        Canvas.show c
    | _ -> ()
  in

  let draw_tall c ob =
    let x, y = get_pos ob in
    match cactus_tall_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(19, 36);
        Canvas.show c
    | _ -> ()
  in

  let draw_normal c ob =
    let x, y = get_pos ob in
    match cactus_normal_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(20, 27);
        Canvas.show c
    | _ -> ()
  in

  let draw_grass1 c g1 =
    let x, y = get_dec_pos g1 in
    match grass1_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(20, 7);
        Canvas.show c
    | _ -> ()
  in

  let draw_grass2 c g2 =
    let x, y = get_dec_pos g2 in
    match grass2_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(20, 7);
        Canvas.show c
    | _ -> ()
  in
  let draw_cloud1 c c1 =
    let x, y = get_dec_pos c1 in
    match cloud1_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(77, 20);
        Canvas.show c
    | _ -> ()
  in
  let draw_cloud2 c c2 =
    let x, y = get_dec_pos c2 in
    match cloud2_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(73, 36);
        Canvas.show c
    | _ -> ()
  in
  let draw_cloud3 c c3 =
    let x, y = get_dec_pos c3 in
    match cloud3_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(80, 23);
        Canvas.show c
    | _ -> ()
  in
  let draw_star1 c s1 =
    let x, y = get_dec_pos s1 in
    match star1_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(6, 7);
        Canvas.show c
    | _ -> ()
  in
  let draw_star2 c s2 =
    let x, y = get_dec_pos s2 in
    match star2_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(6, 7);
        Canvas.show c
    | _ -> ()
  in
  let draw_bump1 c b1 =
    let x, y = get_dec_pos b1 in
    match bump1_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(9, 3);
        Canvas.show c
    | _ -> ()
  in
  let draw_bump2 c b2 =
    let x, y = get_dec_pos b2 in
    match bump2_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(14, 3);
        Canvas.show c
    | _ -> ()
  in

  let draw_dec c (dec : Decorations.t) =
    match get_dec_type dec with
    | 0 -> draw_grass1 c dec
    | 1 -> draw_grass2 c dec
    | 2 -> draw_star1 c dec
    | 3 -> draw_star2 c dec
    | 4 -> draw_cloud1 c dec
    | 5 -> draw_cloud2 c dec
    | 6 -> draw_cloud3 c dec
    | 7 -> draw_bump1 c dec
    | 8 -> draw_bump2 c dec
    | _ -> failwith "unrecognized decoration type"
  in

  let draw_obstacle c obstacle =
    match get_type obstacle with
    | 0 -> draw_short c obstacle
    | 1 -> draw_tall c obstacle
    | 2 -> draw_normal c obstacle
    | _ -> failwith "unrecognized obstacle type"
  in

  let rec draw_obstacles canvas (obstacles : Obstacle.t list) =
    match obstacles with
    | [] -> ()
    | h :: t ->
        draw_obstacle canvas h;
        draw_obstacles canvas t
  in

  let rec draw_decs canvas (decs : Decorations.t list) =
    match decs with
    | [] -> ()
    | h :: t ->
        draw_dec canvas h;
        draw_decs canvas t
  in

  let add_obstacle vel =
    obstacles := create_obstacle 800. 183. (vel *. -1.) 0. :: !obstacles
  in

  let add_grass vel =
    decorations := create_grass 800. 183. (vel *. -1.) 0. :: !decorations
  in

  let add_bump vel =
    decorations := create_bump 800. 183. (vel *. -1.) 0. :: !decorations
  in

  let add_cloud vel =
    decorations :=
      create_cloud 800. (Random.float 100.) (vel *. -1.) 0. :: !decorations
  in

  let add_star vel =
    decorations :=
      create_star 800. (Random.float 25.) (vel *. -1.) 0. :: !decorations
  in

  let ob_min_spawn_interval = 8. in
  let ob_max_spawn_interval = 10. in
  let dec_min_spawn_interval = 2. in
  let dec_max_spawn_interval = 4. in
  let speed = 170. in
  (* Factor by which obstacle speed increases with score *)
  let speed_increase_factor = 1.05 in

  let ob_spawn_interval () =
    let min_interval =
      ob_min_spawn_interval /. (speed_increase_factor ** (!score /. 50.))
    in
    let max_interval =
      ob_max_spawn_interval /. (speed_increase_factor ** (!score /. 50.))
    in
    Random.float (max_interval -. min_interval) +. min_interval
  in

  let grass_spawn_interval () =
    let min_interval =
      dec_min_spawn_interval /. (speed_increase_factor ** (!score /. 30.))
    in
    let max_interval =
      dec_max_spawn_interval /. (speed_increase_factor ** (!score /. 30.))
    in
    Random.float (max_interval -. min_interval) +. min_interval
  in
  let bump_spawn_interval () =
    let min_interval =
      dec_min_spawn_interval *. 4. /. (speed_increase_factor ** (!score /. 90.))
    in
    let max_interval =
      dec_max_spawn_interval *. 4. /. (speed_increase_factor ** (!score /. 90.))
    in
    Random.float (max_interval -. min_interval) +. min_interval
  in

  let cloud_spawn_interval () =
    let min_interval =
      dec_min_spawn_interval *. 12. /. (speed_increase_factor ** (!score /. 50.))
    in
    let max_interval =
      dec_max_spawn_interval *. 12. /. (speed_increase_factor ** (!score /. 50.))
    in
    Random.float (max_interval -. min_interval) +. min_interval
  in

  let star_spawn_interval () =
    let min_interval =
      dec_min_spawn_interval *. 18. /. (speed_increase_factor ** (!score /. 50.))
    in
    let max_interval =
      dec_max_spawn_interval *. 18. /. (speed_increase_factor ** (!score /. 50.))
    in
    Random.float (max_interval -. min_interval) +. min_interval
  in

  let spawn_obstacle () =
    let vel = speed +. (!score /. 10.) in
    add_obstacle vel
  in

  let spawn_grass () =
    let vel = speed +. (!score /. 10.) in
    add_grass vel
  in
  let spawn_bump () =
    let vel = speed +. (!score /. 10.) in
    add_bump vel
  in

  let spawn_cloud () =
    let vel = (speed /. 10.) +. (!score /. 1000.) in
    add_cloud vel
  in

  let spawn_star () =
    let vel = (speed /. 20.) +. (!score /. 1000.) in
    add_star vel
  in

  let draw_frame () =
    let dt = 0.033 in

    (* Time increment per frame *)

    (* Apply gravity to the player *)
    let pl_x, pl_y = player_state.pos in
    let pl_vx, pl_vy = player_state.vel in
    let gravity_force = gravity_acceleration *. dt in
    let pl_new_vy = pl_vy +. gravity_force in
    let pl_new_x = pl_x +. (pl_vx *. dt) in
    let pl_new_y = pl_y +. (pl_new_vy *. dt) in
    (* Prevent player from falling through the ground *)
    let pl_new_y =
      if pl_new_y > 173. then (
        grounded := true;
        173.)
      else pl_new_y
    in
    let pl_new_vy = if pl_new_y >= 173. then 0. else pl_new_vy in
    let player_state =
      update_player player_state pl_new_x pl_new_y pl_vx pl_new_vy
    in

    if Random.float 1.0 < dt /. ob_spawn_interval () then spawn_obstacle ();
    obstacles := update_obstacles !obstacles;

    if Random.float 1.0 < dt /. grass_spawn_interval () then spawn_grass ();

    if Random.float 1.0 < dt /. bump_spawn_interval () then spawn_bump ();

    if Random.float 1.0 < dt /. cloud_spawn_interval () then spawn_cloud ();

    if Random.float 1.0 < dt /. star_spawn_interval () then spawn_star ();
    decorations := update_decorations !decorations;

    if check_collisions player_state !obstacles then (
      player_state.is_alive <- false;
      clear_events ();
      events := [];
      Canvas.hide c;
      game_finished (max (int_of_float !score) best_score))
    else (
      Canvas.setFillColor c Color.white;
      load_bg c;

      draw_player c player_state;
      draw_decs c !decorations;
      draw_obstacles c !obstacles;
      draw_score c;

      score := !score +. 0.3;
      Canvas.fill c ~nonzero:true;
      Canvas.stroke c)
  in

  retain_event
  @@ React.E.map
       (fun bgImage ->
         bg_img.image_opt <- Some bgImage;
         load_bg c)
       bg_image;

  retain_event
  @@ React.E.map
       (fun player_image ->
         player_img.image_opt <- Some player_image;
         draw_player c player_state)
       camel1_image;

  retain_event
  @@ React.E.map
       (fun cac_norm_img -> cactus_normal_img.image_opt <- Some cac_norm_img)
       cactus_normal_image;

  retain_event
  @@ React.E.map
       (fun g1_img -> grass1_img.image_opt <- Some g1_img)
       grass1_image;

  retain_event
  @@ React.E.map
       (fun g2_img -> grass2_img.image_opt <- Some g2_img)
       grass2_image;

  retain_event
  @@ React.E.map (fun b1_img -> bump1_img.image_opt <- Some b1_img) bump1_image;
  retain_event
  @@ React.E.map (fun b2_img -> bump2_img.image_opt <- Some b2_img) bump2_image;

  retain_event
  @@ React.E.map
       (fun c1_img -> cloud1_img.image_opt <- Some c1_img)
       cloud1_image;

  retain_event
  @@ React.E.map
       (fun c2_img -> cloud2_img.image_opt <- Some c2_img)
       cloud2_image;

  retain_event
  @@ React.E.map
       (fun c3_img -> cloud3_img.image_opt <- Some c3_img)
       cloud3_image;

  retain_event
  @@ React.E.map (fun s1_img -> star1_img.image_opt <- Some s1_img) star1_image;

  retain_event
  @@ React.E.map (fun s2_img -> star2_img.image_opt <- Some s2_img) star2_image;

  retain_event
  @@ React.E.map
       (fun cac_short_img -> cactus_short_img.image_opt <- Some cac_short_img)
       cactus_short_image;

  retain_event
  @@ React.E.map
       (fun cac_tall_img -> cactus_tall_img.image_opt <- Some cac_tall_img)
       cactus_tall_image;

  retain_event
  @@ React.E.map
       (fun _ ->
         Backend.stop ();
         clear_events ())
       Event.close;

  retain_event
  @@ React.E.map
       (fun { Event.data = { Event.key; _ }; _ } ->
         if (key = KeySpacebar || key = KeyUpArrow) && !grounded then (
           Player.jump player_state;
           grounded := false))
       Event.key_down;

  retain_event
  @@ React.E.map
       (fun { Event.data = { Event.key; _ }; _ } ->
         if key = KeyEscape then exit 0)
       Event.key_down;

  retain_event
  @@ React.E.map
       (fun _ -> if is_alive player_state then draw_frame ())
       Event.frame;

  Backend.run (fun () -> clear_events ())
