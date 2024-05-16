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
    (snd player.vel < 0.)

let test_player_jump_alive _ =
  let player = create_player 0 in
  Player.jump player;
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

let test_collision_edge _ =
  let player = create_player 0 in
  let obstacle = create_obstacle 50. 150. 0. 0. in
  assert_bool "Player should collide with obstacle on edge"
    (collision player obstacle 50. 50.)

let test_collision_corner _ =
  let player = create_player 0 in
  let obstacle = create_obstacle 50. 173. 0. 0. in
  assert_bool "Player should collide with obstacle in corner"
    (collision player obstacle 50. 173.)

let test_collision_negative _ =
  let player = create_player 0 in
  let obstacle = create_obstacle (-10.) (-10.) 0. 0. in
  assert_bool "Player should collide with obstacle negative f"
    (collision player obstacle 200. 200.)

let in_range value (min, max) = value >= min && value <= max

(* Test decoration types *)
let test_decoration_types _ =
  Random.self_init ();
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

let test_star_type _ =
  Random.self_init ();
  for i = 1 to 50 do
    let star = create_star 100. 100. 0. 0. in
    assert_bool
      ("Star should be of type Star1 or Star2, iteration: " ^ string_of_int i)
      (match get_dec_type star with
      | 2 | 3 -> true
      | _ -> false)
  done

let test_cloud_type _ =
  Random.self_init ();
  for i = 1 to 50 do
    let cloud = create_cloud 100. 100. 0. 0. in
    assert_bool
      ("Cloud should be of type Cloud1, Cloud2, or Cloud3, iteration: "
     ^ string_of_int i)
      (match get_dec_type cloud with
      | 4 | 5 | 6 -> true
      | _ -> false)
  done

let test_grass_type _ =
  Random.self_init ();
  for i = 1 to 50 do
    let grass = create_grass 100. 100. 0. 0. in
    assert_bool
      ("Grass should be of type Grass1 or Grass2, iteration: " ^ string_of_int i)
      (match get_dec_type grass with
      | 0 | 1 -> true
      | _ -> false)
  done

let test_bump_type _ =
  Random.self_init ();
  for i = 1 to 50 do
    let bump = create_bump 100. 100. 0. 0. in
    assert_bool
      ("Bump should be of type Bump1 or Bump2, iteration: " ^ string_of_int i)
      (match get_dec_type bump with
      | 7 | 8 -> true
      | _ -> false)
  done

(* Test decoration velocities within expected ranges *)
let test_star_velocity =
  let star = create_star 100. 100. (-5.) 0. in
  assert_bool "Star velocity X should be within range"
    (in_range (fst (get_dec_vel star)) (-5., 0.))

let test_cloud_velocity =
  let cloud = create_cloud 100. 100. (-10.) 0. in
  assert_bool "Cloud velocity X should be within range"
    (in_range (fst (get_dec_vel cloud)) (-10., 0.))

let test_decoration_velocities _ =
  test_cloud_velocity;
  test_star_velocity

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
          assert_bool "Short type adjustment correct" (almost_equal y 103.)
      | 1 ->
          (* Normal *)
          assert_bool "Normal type adjustment correct" (almost_equal y 88.)
      | 2 ->
          (* Tall *)
          assert_bool "Tall type adjustment correct" (almost_equal y 91.)
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
  (* Simulate reaction *)
  assert_bool "Players velocity should reset on collision. "
    (snd player.vel <= 0.)

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

let test_obstacle_movement _ =
  let obstacle = create_obstacle 100. 100. (-5.) 0. in
  let initial_x, _ = get_pos obstacle in
  let updated_obstacle = update_obstacle obstacle (initial_x -. 5.) 100. in
  assert_bool "Decoration X should decrease by 5"
    (almost_equal (fst (get_pos updated_obstacle)) (initial_x -. 5.))

let test_player_movement _ =
  let player = create_player 0 in
  let initial_x, _ = get_pl_pos player in
  let updated_player = update_player player (initial_x -. 5.) 100. 5. 0. in
  assert_bool "Decoration X should decrease by 5"
    (almost_equal (fst (get_pl_pos updated_player)) (initial_x -. 5.))

let test_menu_initialization _ =
  let initial_score = 100 in
  let menu_state = initialize_menu_state initial_score in
  assert_equal initial_score menu_state.best_score

let test_menu_active _ =
  let initial_score = 100 in
  let menu_state = initialize_menu_state initial_score in
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
      test_game_start !score_ref (fun final_score -> score_ref := final_score));
  assert_equal ~printer:string_of_int 110 !score_ref

let test_transition_from_game_to_menu _ =
  let score_ref = ref 0 in
  test_game_start !score_ref (fun new_score ->
      score_ref := new_score;

      test_menu_start !score_ref (fun final_score -> score_ref := final_score));
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
      score_ref := new_score;
      test_game_start !score_ref (fun final_score -> score_ref := final_score));
  assert_equal ~printer:string_of_int 110 !score_ref

let test_reactivation_of_menu _ =
  let score_ref = ref 40 in
  test_game_start !score_ref (fun new_score ->
      score_ref := new_score;
      test_menu_start !score_ref (fun final_score ->
          score_ref := final_score (* Score should be carried over *)));
  assert_equal ~printer:string_of_int 50 !score_ref

let test_multiple_game_sessions _ =
  let score_ref = ref 0 in
  test_game_start !score_ref (fun new_score ->
      score_ref := new_score;
      test_menu_start !score_ref (fun menu_score ->
          score_ref := menu_score;
          (* Second game session *)
          test_game_start !score_ref (fun second_game_score ->
              score_ref := second_game_score)));
  assert_equal ~printer:string_of_int 20 !score_ref

(* Comprehensive test suite *)
let tests =
  "test suite"
  >::: [
         "test player jump" >:: test_player_jump;
         "test player jumping is alive" >:: test_player_jump_alive;
         "test obstacle collision" >:: test_obstacle_collision;
         "test obstacle no collision" >:: test_obstacle_no_collision;
         "test edge collision" >:: test_collision_edge;
         "test corner collision" >:: test_collision_corner;
         "test collision with negatives" >:: test_collision_negative;
         "test decoration types" >:: test_decoration_types;
         "test decoration velocities" >:: test_decoration_velocities;
         "test obstacle types and adjustments"
         >:: test_obstacle_type_and_adjustment;
         "test multiple collisions" >:: test_multiple_collisions;
         "test player after collision" >:: test_player_after_collision;
         "test event retention" >:: test_event_retention;
         "test clear events" >:: test_clear_events;
         "test decoration movement" >:: test_decoration_movement;
         "test obstacle movement" >:: test_obstacle_movement;
         "test player movement" >:: test_player_movement;
         "menu initialization" >:: test_menu_initialization;
         "background image loading" >:: test_background_image_loading;
         "transition from menu to game" >:: test_transition_from_menu_to_game;
         "transition from game to menu" >:: test_transition_from_game_to_menu;
         "score reset on new game" >:: test_score_reset_on_new_game;
         "continue with previous score" >:: test_continue_with_previous_score;
         "reactivation of menu" >:: test_reactivation_of_menu;
         "multiple game sessions handling" >:: test_multiple_game_sessions;
         "test menu state is active on start" >:: test_menu_active;
         "test cloud type" >:: test_cloud_type;
         "test grass type" >:: test_grass_type;
         "test star type" >:: test_star_type;
         "test bump type" >:: test_bump_type;
       ]

let _ = run_test_tt_main tests
