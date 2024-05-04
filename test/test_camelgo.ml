open OUnit2
open Camelgo
open Collision
open Player
open Obstacle

let create_test_obstacle x y vx vy = create_obstacle x y vx vy

(* Note: player is created at 50, 180. *)
let create_test_player = create_player 0

let tests =
  "test suite"
  >::: [
         ( "check collision normal" >:: fun _ ->
           let p1 = create_test_player in
           let ob1 = create_test_obstacle 30. 180. 0. 0. in
           assert_bool "obstacle 20 units away with side length 22 will colide"
             (collision p1 ob1 22. 22.) );
         ( "check collision edge" >:: fun _ ->
           let p1 = create_test_player in
           let ob1 = create_test_obstacle 50. 160. 0. 0. in
           assert_bool "obstacle on the player's edge will collide"
             (collision p1 ob1 20. 20.) );
         ( "check collision corner" >:: fun _ ->
           let p1 = create_test_player in
           let ob1 = create_test_obstacle 30. 160. 0. 0. in
           assert_bool "obstacle on the player's corner will collide"
             (collision p1 ob1 20. 20.) );
         ( "check no collision" >:: fun _ ->
           let p1 = create_test_player in
           let ob1 = create_test_obstacle 100. 100. 0. 0. in
           assert_bool "obstacle 100 units above will not collide"
             (not (collision p1 ob1 22. 22.)) );
       ]

let _ = run_test_tt_main tests
