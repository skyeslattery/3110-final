open OcamlCanvas.V1
open State

let events = ref []

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

  let game_state = State.create () in

  retain_event @@
    React.E.map (fun _ ->
        Backend.stop ()
      ) Event.close;

  retain_event @@
    React.E.map (fun _ ->
        let dt = 0.033 in  (* Time increment per frame *)
        let (x, y) = game_state.pos in
        let (vx, vy) = game_state.vel in
        let new_x = x +. (vx *. dt) in  (* Update position based on velocity *)
        let new_y = y +. (vy *. dt) in
        game_state.pos <- (new_x, new_y);

        (* If the box reaches the end of the canvas, wrap around *)
        if new_x >= float_of_int width then game_state.pos <- (0.0, y);

        Canvas.clearPath c;
        Canvas.setFillColor c Color.white;
        Canvas.fillRect c ~pos:(0.0, 0.0) ~size:(float_of_int width, float_of_int height);  (* Clear canvas *)
        Canvas.setFillColor c Color.black;
        Canvas.fillRect c ~pos:game_state.pos ~size:(50.0, 50.0);  (* Draw the box *)

        Canvas.fill c ~nonzero:true;

        Canvas.stroke c;
      ) Event.frame;

  Backend.run (fun () ->
      clear_events ();
      Printf.printf "Goodbye !\n")