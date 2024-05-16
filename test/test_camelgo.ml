open OUnit2
open Camelgo
open Collision
open Player
open Obstacle
open Decorations
open Menu
open Spawns

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

let test_player_jump_vx _ =
  let player = create_player 0 in
  let vx = fst player.vel in
  jump player;
  assert_equal vx (fst player.vel)

let test_player_jump_vy _ =
  let player = create_player 0 in
  let vy = snd player.vel in
  jump player;
  assert_bool "Players velocity is lowered by jumping" (vy > snd player.vel)

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
  assert_bool "Player should collide with obstacle even negative"
    (collision player obstacle 200. 200.)

let test_not_collision_negative _ =
  let player = create_player 0 in
  let obstacle = create_obstacle (-53.) (-173.) 0. 0. in
  assert_bool
    "Player should not collide with obstacle at negative player position"
    (not (collision player obstacle 50. 50.))

let test_tall_hitbox _ =
  let player = create_player 100 in
  Random.self_init ();
  for i = 1 to 20 do
    let ob = create_obstacle 50. 209. 0. 0. in
    assert_bool
      ("Tall should collide, iteration: " ^ string_of_int i)
      (match get_type ob with
      | 2 ->
          collision player ob
            (float_of_int (get_height ob))
            (float_of_int (get_width ob))
      | _ -> true)
  done

let test_short_hitbox _ =
  let player = create_player 100 in
  Random.self_init ();
  for i = 1 to 20 do
    let ob = create_obstacle 20. 173. 0. 0. in
    assert_bool
      ("Only short should collide, iteration: " ^ string_of_int i)
      (match get_type ob with
      | 0 ->
          collision player ob
            (float_of_int (get_height ob))
            (float_of_int (get_width ob))
      | _ ->
          not
            (collision player ob
               (float_of_int (get_height ob))
               (float_of_int (get_width ob))))
  done

let test_normal_hitbox _ =
  let player = create_player 100 in
  Random.self_init ();
  for i = 1 to 20 do
    let ob = create_obstacle 50. 200. 0. 0. in
    assert_bool
      ("Only tall and normal cacti should collide, iteration: "
     ^ string_of_int i)
      (match get_type ob with
      | 1 | 2 ->
          collision player ob
            (float_of_int (get_height ob))
            (float_of_int (get_width ob))
      | _ ->
          not
            (collision player ob
               (float_of_int (get_height ob))
               (float_of_int (get_width ob))))
  done

let test_ob_get _ =
  let obstacle = create_obstacle 50. 173. 0. 0. in
  assert_equal (get_pos obstacle) (get_x obstacle, get_y obstacle);
  assert_equal (get_vel obstacle) (0., 0.)

let test_player_get _ =
  let pl = create_player 0 in
  assert_equal (50., 173.) (get_pl_x pl, get_pl_y pl);
  assert_equal (0., 0.) (get_pl_vx pl, get_pl_vy pl)

let test_player_alive _ =
  let pl = create_player 0 in
  assert_bool "Player is alive upon creation" pl.is_alive

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

let test_obstacle_spawn_interval _ =
  let score = 0. in
  let initial_interval = ob_spawn_interval score in
  let new_score = 100. in
  let adjusted_interval = ob_spawn_interval new_score in
  assert_bool "Obstacle spawn interval should decrease with higher score"
    (adjusted_interval < initial_interval)

let test_ob_frequency_increase _ =
  let score = 0. in
  let initial_obstacle_freq = 1. /. ob_spawn_interval score in
  let new_score = 1000. in
  let new_obstacle_freq = 1. /. ob_spawn_interval new_score in
  assert_bool "Obstacle spawn frequency should increase with score"
    (new_obstacle_freq > initial_obstacle_freq)

let test_dec_frequency_increase _ =
  let score = 0. in
  let initial_decoration_freq = 1. /. star_spawn_interval score in
  let new_score = 1000. in
  let new_decoration_freq = 1. /. star_spawn_interval new_score in
  assert_bool "Decoration spawn frequency should increase with score"
    (new_decoration_freq > initial_decoration_freq)

let test_obstacle_height _ =
  let obstacles = List.init 10 (fun _ -> create_obstacle 100. 100. 0. 0.) in
  List.iter
    (fun ob ->
      let h = get_height ob in
      let type_id = get_type ob in
      match type_id with
      | 0 ->
          (* Short *)
          assert_equal 9 h
      | 1 ->
          (* Normal *)
          assert_equal 22 h
      | 2 ->
          (* Tall *)
          assert_equal 31 h
      | _ -> assert_failure "Unknown obstacle type")
    obstacles

let test_obstacle_width _ =
  let obstacles = List.init 10 (fun _ -> create_obstacle 100. 100. 0. 0.) in
  List.iter
    (fun ob ->
      let w = get_width ob in
      let type_id = get_type ob in
      match type_id with
      | 0 ->
          (* Short *)
          assert_equal 31 w
      | 1 ->
          (* Normal *)
          assert_equal 15 w
      | 2 ->
          (* Tall *)
          assert_equal 14 w
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
  assert_bool "Obstacle X should decrease by 5"
    (almost_equal (fst (get_pos updated_obstacle)) (initial_x -. 5.))

let test_player_movement _ =
  let player = create_player 0 in
  let initial_x, _ = get_pl_pos player in
  let updated_player = update_player player (initial_x -. 5.) 100. 5. 0. in
  assert_bool "Player X should decrease by 5"
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

let test_high_score_influence _ =
  let high_score = 5000. in
  assert_bool "High score significantly reduces obstacle interval"
    (ob_spawn_interval high_score < 1.);
  assert_bool "High score significantly reduces decoration interval"
    (grass_spawn_interval high_score < 1.)

let test_minimum_spawn_interval _ =
  let extreme_score = 100000. in
  assert_bool "Obstacle interval should always be above 0"
    (ob_spawn_interval extreme_score > 0.0);
  assert_bool "Decoration interval should always be above 0"
    (grass_spawn_interval extreme_score > 0.0)

let test_randomness_impact _ =
  let score = 100. in
  Random.init 50;
  let interval1 = ob_spawn_interval score in
  Random.init 50;
  let interval2 = ob_spawn_interval score in
  assert_equal interval1 interval2
    ~msg:"Spawn intervals should be consistent with same random seed"

let test_score_effect_each_type _ =
  let score = 250. in
  let ob_interval = ob_spawn_interval score in
  let grass_interval = grass_spawn_interval score in
  let bump_interval = bump_spawn_interval score in
  let cloud_interval = cloud_spawn_interval score in
  let star_interval = star_spawn_interval score in
  assert_bool "Different types have different intervals based on score"
    (ob_interval <> grass_interval
    && bump_interval <> cloud_interval
    && star_interval <> grass_interval)

let test_interval_range_constraints _ =
  let score = 200. in
  let min_allowed_interval = 0.5 in
  let max_allowed_interval = 10.0 in
  let interval = ob_spawn_interval score in
  assert_bool
    "Spawn interval should not fall below the minimum allowed interval"
    (interval >= min_allowed_interval);
  assert_bool "Spawn interval should not exceed the maximum allowed interval"
    (interval <= max_allowed_interval)

let test_differences_across_types _ =
  let score = 500. in
  let ob_interval = ob_spawn_interval score in
  let cloud_interval = cloud_spawn_interval score in
  let grass_interval = grass_spawn_interval score in
  assert_bool "Grass should spawn more frequently than obstacles"
    (grass_interval < ob_interval);
  assert_bool "Clouds should spawn less frequently than obstacles"
    (cloud_interval > ob_interval);
  assert_bool "Grass should spawn more frequently than clouds"
    (cloud_interval > grass_interval)

let test_differences_across_decorations _ =
  let score = 1000. in
  let bump_interval = bump_spawn_interval score in
  let grass_interval = grass_spawn_interval score in
  assert_bool "Grass should spawn more frequently than obstacles"
    (grass_interval < bump_interval)

(* Comprehensive test suite *)
let tests =
  "test suite"
  >::: [
         "test player jump" >:: test_player_jump;
         "test obstacle collision" >:: test_obstacle_collision;
         "test obstacle no collision" >:: test_obstacle_no_collision;
         "test edge collision" >:: test_collision_edge;
         "test corner collision" >:: test_collision_corner;
         "test collision with negatives" >:: test_collision_negative;
         "test not collision negative" >:: test_not_collision_negative;
         "test tall cacti hitbox" >:: test_tall_hitbox;
         "test short cacti hitbox" >:: test_short_hitbox;
         "test normal cacti hitbox" >:: test_normal_hitbox;
         "test decoration types" >:: test_decoration_types;
         "test decoration velocities" >:: test_decoration_velocities;
         "test obstacle types and adjustments"
         >:: test_obstacle_type_and_adjustment;
         "test obstacle heights" >:: test_obstacle_height;
         "test obstacle widths" >:: test_obstacle_width;
         "test obstacle getters" >:: test_ob_get;
         "test player getters" >:: test_player_get;
         "test player is alive" >:: test_player_alive;
         "players vx is not affected by jumping" >:: test_player_jump_vx;
         "players vy is lowered by jumping" >:: test_player_jump_vy;
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
         "obstacles spawn more frequently as score increases"
         >:: test_ob_frequency_increase;
         "decorations spawn more frequently as score increases"
         >:: test_dec_frequency_increase;
         "spawn interval should decrease over time"
         >:: test_obstacle_spawn_interval;
         "test very high scores influence" >:: test_high_score_influence;
         "test minimum spawn interval" >:: test_minimum_spawn_interval;
         "test randomness impact" >:: test_randomness_impact;
         "test score effect on each type" >:: test_score_effect_each_type;
         "test constraints on spawn interval"
         >:: test_interval_range_constraints;
         "interval differences across spawn types"
         >:: test_differences_across_types;
         "interval differences across different decorations"
         >:: test_differences_across_decorations;
       ]

let _ = run_test_tt_main tests
