open OcamlCanvas.V1

let width = 1000
let height = 400

let () = 
  Backend.init ();

  let c = Canvas.createOnscreen ~title:"CamelGO"
            ~pos:(300, 200) ~size:(width, height) () in
  
  Canvas.show c;

  Backend.run (fun () ->
    Printf.printf "Goodbye !\n")