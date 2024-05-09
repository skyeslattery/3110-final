open OUnit2
open Camelgo
open Collision
open Player
open Obstacle
open Decorations
open Menu

(* Helper function to simplify test writing *)
let almost_equal a b = abs_float (a -. b) < 1e-6
let events = ref []
let retain_event e = events := e :: !events
let clear_events () = events := []

(* Player state tests *)
let test_player_jump _ =
  let player = create_player 0 in
  Player.jump player;
  assert_bool "Player velocity Y should be negative after jumping"
    (snd player.vel < 0.);
  assert_bool "Player should be alive after jumping" (is_alive player)

(* Obstacle and collision detection tests *)
let test_obstacle_collision _ =
  let player = create_player 0 in
  let obstacle = create_obstacle 30. 180. 0. 0. in
  assert_bool "Player should collide with obstacle close by"
    (collision player obstacle 22. 22.)

let test_obstacle_no_collision _ =
  let player = create_player 0 in
  let obstacle = create_obstacle 100. 100. 0. 0. in
  assert_bool "Player should not collide with distant obstacle"
    (not (collision player obstacle 22. 22.))

let in_range value (min, max) = value >= min && value <= max

(* Test decoration types *)
let test_decoration_types _ =
  let star = create_star 100. 100. 0. 0. in
  let cloud = create_cloud 100. 100. 0. 0. in
  let grass = create_grass 100. 100. 0. 0. in
  let bump = create_bump 100. 100. 0. 0. in
  assert_bool "Star should be of type Star1 or Star2"
    (match get_dec_type star with
    | 2 | 3 -> true
    | _ -> false);
  assert_bool "Cloud should be of type Cloud1, Cloud2, or Cloud3"
    (match get_dec_type cloud with
    | 4 | 5 | 6 -> true
    | _ -> false);
  assert_bool "Grass should be of type Grass1 or Grass2"
    (match get_dec_type grass with
    | 0 | 1 -> true
    | _ -> false);
  assert_bool "Bump should be of type Bump1 or Bump2"
    (match get_dec_type bump with
    | 7 | 8 -> true
    | _ -> false)

(* Test decoration velocities within expected ranges *)
let test_decoration_velocities _ =
  let star = create_star 100. 100. (-5.) 0. in
  let cloud = create_cloud 100. 100. (-10.) 0. in
  assert_bool "Star velocity X should be within range"
    (in_range (fst (get_dec_vel star)) (-5., 0.));
  assert_bool "Cloud velocity X should be within range"
    (in_range (fst (get_dec_vel cloud)) (-10., 0.))

(* Obstacle creation type and adjustment tests *)
let test_obstacle_type_and_adjustment _ =
  let obstacles = List.init 10 (fun _ -> create_obstacle 100. 100. 0. 0.) in
  List.iter
    (fun ob ->
      let _, y = get_pos ob in
      let type_id = get_type ob in
      match type_id with
      | 0 ->
          (* Short *)
          assert_bool "Short type adjustment incorrect" (almost_equal y 103.)
      | 1 ->
          (* Normal *)
          assert_bool "Normal type adjustment incorrect" (almost_equal y 88.)
      | 2 ->
          (* Tall *)
          assert_bool "Tall type adjustment incorrect" (almost_equal y 91.)
      | _ -> assert_failure "Unknown obstacle type")
    obstacles

(* Additional tests for handling multiple collisions and game events *)
let test_multiple_collisions _ =
  let player = create_player 0 in
  let obstacles =
    [
      create_obstacle 50. 180. 0. 0.;
      create_obstacle 52. 180. 0. 0.;
      create_obstacle 54. 180. 0. 0.;
    ]
  in
  List.iter
    (fun ob ->
      assert_bool "Player should collide with each obstacle"
        (collision player ob 22. 22.))
    obstacles

let test_player_after_collision _ =
  let player = create_player 0 in
  let obstacle = create_obstacle 30. 180. 0. 0. in
  let _ = collision player obstacle 22. 22. in
  Player.jump player;
  (* Simulate reaction *)
  assert_bool "Player should still be able to jump after collision"
    (snd player.vel < 0.)

let test_event_retention _ =
  let initial_event_count = List.length !events in
  retain_event (React.E.map (fun _ -> ()) React.E.never);
  retain_event (React.E.map (fun _ -> ()) React.E.never);
  assert_bool "Two events should be added"
    (List.length !events = initial_event_count + 2)

let test_clear_events _ =
  retain_event (React.E.map (fun _ -> ()) React.E.never);
  clear_events ();
  assert_bool "Events list should be empty after clear" (List.length !events = 0)

(* Decoration behavior under game mechanics *)
let test_decoration_movement _ =
  let decoration = create_star 100. 100. (-5.) 0. in
  let initial_x, _ = get_dec_pos decoration in
  let updated_decoration = update_dec decoration (initial_x -. 5.) 100. in
  assert_bool "Decoration X should decrease by 5"
    (almost_equal (fst (get_dec_pos updated_decoration)) (initial_x -. 5.))

let test_menu_initialization _ =
  let initial_score = 100 in
  let menu_state = initialize_menu_state initial_score in
  assert_equal initial_score menu_state.best_score;
  assert_bool "Menu should be active on initialization" menu_state.is_active

let test_background_image_loading _ =
  let bg = bg_img.image_opt in
  match bg with
  | Some _ ->
      assert_failure "Background image shouldn't be loaded until initialization"
  | None -> ()

(* Mock implementations of game and menu states during gameplay*)
module MockGame = struct
  let start initial_score callback = callback (initial_score + 10)
end

module MockMenu = struct
  let start_menu initial_score callback = callback initial_score
end

let game_start = MockGame.start
let menu_start = MockMenu.start_menu

let test_game_start current_score game_finished =
  game_start current_score game_finished

and test_menu_start current_score menu_finished =
  menu_start current_score menu_finished

let test_transition_from_menu_to_game _ =
  let score_ref = ref 100 in
  test_menu_start !score_ref (fun new_score ->
      score_ref := new_score;
      test_game_start !score_ref (fun final_score ->
          score_ref := final_score (* Game should increase score *)));
  assert_equal ~printer:string_of_int 110 !score_ref

let test_transition_from_game_to_menu _ =
  let score_ref = ref 0 in
  test_game_start !score_ref (fun new_score ->
      score_ref := new_score;
      (* Game increases score *)
      test_menu_start !score_ref (fun final_score ->
          score_ref := final_score (* Menu should not change score *)));
  assert_equal ~printer:string_of_int 10
    !score_ref (* Check score consistency after menu *)

let test_score_reset_on_new_game _ =
  let score_ref = ref 30 in
  test_menu_start !score_ref (fun _ ->
      (* Simulate selecting "New Game" *)
      score_ref := 0;
      (* Resetting score for new game *)
      test_game_start !score_ref (fun final_score -> score_ref := final_score));
  assert_equal ~printer:string_of_int 10 !score_ref

let test_continue_with_previous_score _ =
  let score_ref = ref 100 in
  test_menu_start !score_ref (fun new_score ->
      (* Assume player chooses to continue with the previous score *)
      score_ref := new_score;
      (* No reset, continue with 100 *)
      test_game_start !score_ref (fun final_score ->
          score_ref := final_score (* Increment score by game logic *)));
  assert_equal ~printer:string_of_int 110 !score_ref

let test_reactivation_of_menu _ =
  let score_ref = ref 40 in
  test_game_start !score_ref (fun new_score ->
      score_ref := new_score;
      (* Game ends with some score *)
      test_menu_start !score_ref (fun final_score ->
          score_ref := final_score
          (* Back to menu, score should be carried over *)));
  assert_equal ~printer:string_of_int 50 !score_ref

let test_multiple_game_sessions _ =
  let score_ref = ref 0 in
  (* First game session *)
  test_game_start !score_ref (fun new_score ->
      score_ref := new_score;
      test_menu_start !score_ref (fun menu_score ->
          score_ref := menu_score;
          (* Back to menu after first game *)
          (* Second game session *)
          test_game_start !score_ref (fun second_game_score ->
              score_ref := second_game_score)));
  assert_equal ~printer:string_of_int 20 !score_ref

(* Comprehensive test suite *)
let tests =
  "test suite"
  >::: [
         "test player jump" >:: test_player_jump;
         "test obstacle collision" >:: test_obstacle_collision;
         "test obstacle no collision" >:: test_obstacle_no_collision;
         "test decoration types" >:: test_decoration_types;
         "test decoration velocities" >:: test_decoration_velocities;
         "test obstacle types and adjustments"
         >:: test_obstacle_type_and_adjustment;
         "test multiple collisions" >:: test_multiple_collisions;
         "test player after collision" >:: test_player_after_collision;
         "test event retention" >:: test_event_retention;
         "test clear events" >:: test_clear_events;
         "test decoration movement" >:: test_decoration_movement;
         "menu initialization" >:: test_menu_initialization;
         "background image loading" >:: test_background_image_loading;
         "transition from menu to game" >:: test_transition_from_menu_to_game;
         "transition from game to menu" >:: test_transition_from_game_to_menu;
         "score reset on new game" >:: test_score_reset_on_new_game;
         "continue with previous score" >:: test_continue_with_previous_score;
         "reactivation of menu" >:: test_reactivation_of_menu;
         "multiple game sessions handling" >:: test_multiple_game_sessions;
       ]

let _ = run_test_tt_main tests
