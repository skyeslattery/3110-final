open OcamlCanvas.V1
open Obstacle
open Player
open Collision

let gravity_acceleration = 95.
let events = ref []
let grounded = ref true
let retain_event e = events := e :: !events
let clear_events () = events := []
let obstacles = ref []
let score = ref 0.

type state = { mutable image_opt : Canvas.t option }

let bg_img = { image_opt = None }
let player_img = { image_opt = None }
let camel_images = Array.init 3 (fun _ -> { image_opt = None })
(* Initialize an array for the camel images *)

let cactus_short_img = { image_opt = None }
let cactus_tall_img = { image_opt = None }
let cactus_normal_img = { image_opt = None }

let start best_score game_finished =
  Backend.init ();
  score := 0.0;
  grounded := true;
  obstacles := [ create_obstacle 800. 183. (-200.) 0. ];
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

  let draw_player canvas player_state =
    if is_alive player_state then
      let x, y = player_state.pos in
      let index = int_of_float (!score /. 3.) mod 3 in
      (* Cycle through camel images *)
      match camel_images.(index).image_opt with
      | Some image ->
          Canvas.blit ~dst:c
            ~dpos:(int_of_float x, int_of_float y)
            ~src:image ~spos:(0, 0) ~size:(35, 30);
          Canvas.show canvas
      | _ -> ()
  in

  let rec update_obstacles obstacles =
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

  let initial_obstacle = create_obstacle 800. 183. (-200.) 0. in
  obstacles := initial_obstacle :: !obstacles;

  let rec check_collisions player_state obstacles ob_height ob_width =
    if is_alive player_state then
      match obstacles with
      | [] -> false
      | h :: t ->
          collision player_state h ob_height ob_width
          || check_collisions player_state t ob_height ob_width
    else false
  in

  let draw_score canvas =
    Canvas.setFillColor canvas Color.black;
    Canvas.setFont canvas "arial" ~size:24. ~slant:Font.Roman ~weight:50;
    Canvas.fillText canvas (string_of_int (int_of_float !score)) (745., 24.)
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
          ~src:image ~spos:(0, 0) ~size:(35, 30);
        Canvas.show c
    | _ -> ()
  in

  let draw_tall c ob =
    let x, y = get_pos ob in
    match cactus_tall_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(35, 30);
        Canvas.show c
    | _ -> ()
  in

  let draw_normal c ob =
    let x, y = get_pos ob in
    match cactus_normal_img.image_opt with
    | Some image ->
        Canvas.blit ~dst:c
          ~dpos:(int_of_float x, int_of_float y)
          ~src:image ~spos:(0, 0) ~size:(35, 30);
        Canvas.show c
    | _ -> ()
  in

  let draw_obstacle c obstacle =
    match get_type obstacle with
    | 0 -> draw_short c obstacle
    | 1 -> draw_tall c obstacle
    | _ -> draw_normal c obstacle
  in

  let rec draw_obstacles canvas obstacles =
    match obstacles with
    | [] -> ()
    | h :: t ->
        draw_obstacle canvas h;
        draw_obstacles canvas t
  in

  let add_obstacle vel =
    obstacles := create_obstacle 800. 183. (vel *. -1.) 0. :: !obstacles
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

    if Random.int 50 = 0 then add_obstacle 200.;
    obstacles := update_obstacles !obstacles;

    if check_collisions player_state !obstacles 47. 47. then (
      player_state.is_alive <- false;
      clear_events ();
      events := [];
      Canvas.hide c;
      game_finished (max (int_of_float !score) best_score))
    else (
      Canvas.setFillColor c Color.white;
      load_bg c;
      draw_score c;

      draw_player c player_state;
      draw_obstacles c !obstacles;

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
         if key = KeySpacebar && !grounded then (
           Player.jump player_state;
           grounded := false))
       Event.key_down;

  retain_event
  @@ React.E.map
       (fun _ -> if is_alive player_state then draw_frame ())
       Event.frame;

  Backend.run (fun () -> clear_events ())
