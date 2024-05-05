open OcamlCanvas.V1

let events = ref []
let retain_event e = events := e :: !events
let clear_events () = events := []

type bg_state = { mutable image_opt : Canvas.t option }

let bg_img = { image_opt = None }

type menu_state = {
  mutable best_score : int;
  mutable is_active : bool;
}

let initialize_menu_state best_score = { best_score; is_active = true }

let start_menu best_score menu_finished =
  Backend.init ();

  let bg_image = Canvas.createOffscreenFromPNG "./assets/bg.png" in

  let width = 800. in
  let height = 250. in
  let canvas =
    Canvas.createOnscreen ~title:"CamelGO" ~pos:(300, 200)
      ~size:(int_of_float width, int_of_float height)
      ()
  in
  Canvas.show canvas;

  let load_bg canvas =
    match bg_img.image_opt with
    | Some bg_image ->
        Canvas.blit ~dst:canvas ~dpos:(0, 0) ~src:bg_image ~spos:(0, 0)
          ~size:(int_of_float width, int_of_float height);
        Canvas.show canvas
    | _ -> ()
  in

  let menu_state = initialize_menu_state best_score in

  let draw_menu () =
    if menu_state.is_active then Canvas.clearPath canvas;

    Canvas.setFillColor canvas Color.white;
    Canvas.fillRect canvas ~pos:(0., 0.) ~size:(width, height);
    Canvas.setFillColor canvas (Color.of_rgb 27 23 27);
    Canvas.setFont canvas "Geonica" ~size:28. ~slant:Font.Roman ~weight:50;
    Canvas.setLineWidth canvas 15.;
    load_bg canvas;
    let text = "Press SPACE to Start Game" in
    let text_x = 240. in
    let text_y = height /. 2.0 in
    Canvas.fillText canvas text (text_x, text_y);

    let score_text = "Best Score: " ^ string_of_int menu_state.best_score in
    let score_text_x = 315. in
    Canvas.fillText canvas score_text (score_text_x, text_y +. 40.0)
  in

  let close_menu () =
    menu_state.is_active <- false;
    Canvas.hide canvas;
    clear_events ();
    events := []
  in

  retain_event
  @@ React.E.map
       (fun { Event.data = { Event.key; _ }; _ } ->
         if key = KeySpacebar && menu_state.is_active then (
           close_menu ();
           menu_finished menu_state.best_score))
       Event.key_down;

  retain_event @@ React.E.map (fun _ -> Backend.stop ()) Event.close;

  retain_event
  @@ React.E.map
       (fun { Event.data = { Event.key; _ }; _ } ->
         if key = KeyEscape then exit 0)
       Event.key_down;

  retain_event
  @@ React.E.map
       (fun _ -> if menu_state.is_active then draw_menu ())
       Event.frame;

  retain_event
  @@ React.E.map
       (fun bgImage ->
         bg_img.image_opt <- Some bgImage;
         load_bg canvas)
       bg_image;

  Backend.run (fun () -> clear_events ())
