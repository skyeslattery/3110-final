open OcamlCanvas.V1
open Obstacle
open Player
open Collision

let gravity_acceleration = 95.   

let events = ref []
let grounded = ref true  (* Flag indicating whether the player is grounded *)

let retain_event e =
  events := e :: !events

let clear_events () =
  events := []

let start () =
  Backend.init ();

  let width = 800 in   (* Width of the canvas *)
  let height = 200 in  (* Height of the canvas *)

  let c = Canvas.createOnscreen ~title:"CamelGO"
            ~pos:(300, 200) ~size:(width, height) () in

  Canvas.show c;

  let player_state = create_player () in 
  let obstacle_state = create_obstacle 800. 180. (-200.) 0. in  

  retain_event @@
    React.E.map (fun _ ->
        Backend.stop ();
        clear_events ();
        Printf.printf "Goodbye !\n"
      ) Event.close;

  retain_event @@
    React.E.map (fun { Event.data = { Event.key; _ }; _ } ->
        if key = KeySpacebar && !grounded then (
          Player.jump player_state;
          grounded := false
        )
      ) Event.key_down;

  retain_event @@
    React.E.map (fun _ ->
        let dt = 0.033 in  (* Time increment per frame *)

        (* Apply gravity to the player *)
        let (pl_x, pl_y) = player_state.pos in
        let (pl_vx, pl_vy) = player_state.vel in
        let gravity_force = gravity_acceleration *. dt in
        let pl_new_vy = pl_vy +. gravity_force in
        let pl_new_x = pl_x +. (pl_vx *. dt) in  
        let pl_new_y = pl_y +. (pl_new_vy *. dt) in
        (* Prevent player from falling through the ground *)
        let pl_new_y =
          if pl_new_y > 180. then (
            grounded := true;
            180.
          ) else pl_new_y
        in 
        let pl_new_vy = if pl_new_y >= 180. then 0. else pl_new_vy in
        let player_state = update_player player_state pl_new_x pl_new_y pl_vx pl_new_vy in

        (* Update the obstacle *)
        let (ob_x, ob_y) = obstacle_state.pos in
        let (ob_vx, ob_vy) = obstacle_state.vel in
        let ob_new_x = ob_x +. (ob_vx *. dt) in
        let ob_new_y = ob_y +. (ob_vy *. dt) in
        let obstacle_state = update_obstacle obstacle_state ob_new_x ob_new_y in

        (* Check for collision between player and obstacle *)
        if collision player_state obstacle_state 47. 47. then (
          Backend.stop ();
          clear_events ();
          Printf.printf "Game Over! You collided with the obstacle.\n";
          exit 0;  (* Exit the program *)
        );

        (* Clear canvas and draw objects *)
        Canvas.clearPath c;
        Canvas.setFillColor c Color.white;
        Canvas.fillRect c ~pos:(0.0, 0.0) ~size:(float_of_int width, float_of_int height);  (* Clear canvas *)
        Canvas.setFillColor c Color.black;

        draw_player c player_state;
        draw_obstacle c obstacle_state;

        Canvas.fill c ~nonzero:true;
        Canvas.stroke c;
      ) Event.frame;

  Backend.run (fun () ->
      clear_events ()
    )
