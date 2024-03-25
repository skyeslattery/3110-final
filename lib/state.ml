open OcamlCanvas.V1

(* Define state type and related functions *)
type t = {
  mutable pos : Point.t;
  vel : float * float;
}

let create () = { pos = (800.0, 150.0); vel = (-200.0, 0.0) }

(* Add state manipulation functions *)
