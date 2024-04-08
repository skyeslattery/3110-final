open OcamlCanvas.V1
open Obstacle
open Player
open Collision

let gravity_acceleration = 95.
let events = ref []
let grounded = ref true (* Flag indicating whether the player is grounded *)
let retain_event e = events := e :: !events
let clear_events () = events := []
let obstacles = ref []

let start () =
  Backend.init ();

  let width = 800 in
  (* Width of the canvas *)
  let height = 250 in

  (* Height of the canvas *)
  let c =
    Canvas.createOnscreen ~title:"CamelGO" ~pos:(300, 200) ~size:(width, height)
      ()
  in

  Canvas.show c;

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

  let player_state = create_player () in
  let initial_obstacle = create_obstacle 800. 180. (-200.) 0. in
  obstacles := initial_obstacle :: !obstacles;

  let rec check_collisions player_state obstacles ob_height ob_width =
    match obstacles with
    | [] -> false
    | h :: t ->
        collision player_state h ob_height ob_width
        || check_collisions player_state t ob_height ob_width
  in

  let rec draw_obstacles canvas obstacles =
    match obstacles with
    | [] -> ()
    | h :: t ->
        draw_obstacle canvas h;
        draw_obstacles canvas t
  in

  let add_obstacle vel =
    obstacles := create_obstacle 800. 180. (vel *. -1.) 0. :: !obstacles
  in

  retain_event
  @@ React.E.map
       (fun _ ->
         Backend.stop ();
         clear_events ();
         Printf.printf "Goodbye !\n")
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
       (fun _ ->
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
           if pl_new_y > 170. then (
             grounded := true;
             170.)
           else pl_new_y
         in
         let pl_new_vy = if pl_new_y >= 170. then 0. else pl_new_vy in
         let player_state =
           update_player player_state pl_new_x pl_new_y pl_vx pl_new_vy
         in

         if Random.int 50 = 0 then add_obstacle 200.;
         (* Update the obstacles *) obstacles := update_obstacles !obstacles;

         (* Check for collision between player and obstacle *)
         if check_collisions player_state !obstacles 47. 47. then (
           Backend.stop ();
           clear_events ();
           Printf.printf "Game Over! You collided with the obstacle.\n";
           exit 0 (* Exit the program *));

         (* Clear canvas and draw objects *)
         Canvas.clearPath c;
         Canvas.setFillColor c Color.white;
         Canvas.fillRect c ~pos:(0.0, 0.0)
           ~size:(float_of_int width, float_of_int height);
         (* Clear canvas *)
         Canvas.setFillColor c Color.black;

         draw_player c player_state;
         draw_obstacles c !obstacles;

         Canvas.fill c ~nonzero:true;
         Canvas.stroke c)
       Event.frame;

  Backend.run (fun () -> clear_events ())
