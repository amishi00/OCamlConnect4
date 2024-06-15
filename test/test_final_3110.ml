open Final_3110.State
open OUnit2
open Final_3110.Minimax
open Final_3110.Easy
open Final_3110
open Unix

let in_prog_status _ =
  let board = create_board () in
  assert (get_status board = InProgress)

let vertical_win_red_board =
  let board = create_board () in
  let board = make_move board Red 0 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 0 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 0 in
  let board = make_move board Blue 2 in
  make_move board Red 0

let vertical_win_blue_board =
  let board = create_board () in
  let board = make_move board Blue 0 in
  let board = make_move board Red 2 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 2 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 2 in
  make_move board Blue 0

let diagonal_win_board_blue =
  let board = create_board () in
  let board = make_move board Red 1 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 2 in
  let board = make_move board Blue 1 in
  let board = make_move board Red 2 in
  let board = make_move board Blue 3 in
  let board = make_move board Red 3 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 3 in
  make_move board Blue 3

let in_prog_after_moves _ =
  let board = create_board () in
  let board = make_move board Red 0 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 0 in
  assert (get_status board = InProgress)

let diagonal_win_board_red =
  let board = create_board () in
  let board = make_move board Blue 1 in
  let board = make_move board Red 0 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 1 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 3 in
  let board = make_move board Blue 3 in
  let board = make_move board Red 2 in
  let board = make_move board Blue 3 in
  make_move board Red 3

let draw_results_board_full_game =
  let board = create_board () in
  let board = make_move board Blue 0 in
  let board = make_move board Blue 0 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 0 in
  let board = make_move board Red 0 in
  let board = make_move board Red 0 in
  let board = make_move board Red 1 in
  let board = make_move board Red 1 in
  let board = make_move board Red 1 in
  let board = make_move board Blue 1 in
  let board = make_move board Blue 1 in
  let board = make_move board Blue 1 in
  let board = make_move board Blue 2 in
  let board = make_move board Blue 2 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 2 in
  let board = make_move board Red 2 in
  let board = make_move board Red 2 in
  let board = make_move board Red 3 in
  let board = make_move board Red 3 in
  let board = make_move board Red 3 in
  let board = make_move board Blue 3 in
  let board = make_move board Blue 3 in
  let board = make_move board Blue 3 in
  let board = make_move board Blue 4 in
  let board = make_move board Blue 4 in
  let board = make_move board Blue 4 in
  let board = make_move board Red 4 in
  let board = make_move board Red 4 in
  let board = make_move board Red 4 in
  let board = make_move board Red 5 in
  let board = make_move board Red 5 in
  let board = make_move board Red 5 in
  let board = make_move board Blue 5 in
  let board = make_move board Blue 5 in
  let board = make_move board Blue 5 in
  let board = make_move board Blue 6 in
  let board = make_move board Blue 6 in
  let board = make_move board Blue 6 in
  let board = make_move board Red 6 in
  let board = make_move board Red 6 in
  let board = make_move board Red 6 in
  let board = make_move board Red 7 in
  let board = make_move board Red 7 in
  let board = make_move board Red 7 in
  let board = make_move board Blue 7 in
  let board = make_move board Blue 7 in
  let board = make_move board Blue 7 in
  let board = make_move board Blue 8 in
  let board = make_move board Blue 8 in
  let board = make_move board Blue 8 in
  let board = make_move board Red 8 in
  let board = make_move board Red 8 in
  make_move board Red 8

let horizontal_win_board_red =
  let board = create_board () in
  let board = make_move board Red 0 in
  let board = make_move board Red 1 in
  let board = make_move board Red 2 in
  make_move board Red 3

let horizontal_win_board_blue =
  let board = create_board () in
  let board = make_move board Blue 0 in
  let board = make_move board Blue 1 in
  let board = make_move board Blue 2 in
  make_move board Blue 3

let horizontal_win_red _ =
  assert (get_status horizontal_win_board_red = Won Red)

let horizontal_win_blue _ =
  assert (get_status horizontal_win_board_blue = Won Blue)

let vertical_win_red _ = assert (get_status vertical_win_red_board = Won Red)
let vertical_win_blue _ = assert (get_status vertical_win_blue_board = Won Blue)
let diagonal_win_blue _ = assert (get_status diagonal_win_board_blue = Won Blue)
let draw _ = assert (get_status draw_results_board_full_game = Draw)
let diagonal_win_red _ = assert (get_status diagonal_win_board_red = Won Red)

let empty_board _ =
  let board = create_board () in
  assert (Array.length board = 6);
  Array.iter
    (fun row ->
      assert (Array.length row = 9);
      Array.iter (fun cell -> assert (cell = None)) row)
    board

let string_horizontal_board_red _ =
  assert (
    ".........\n.........\n.........\n.........\n.........\nRRRR.....\n"
    = string_of_board horizontal_win_board_red)

let string_board_empty _ =
  assert (
    ".........\n.........\n.........\n.........\n.........\n.........\n"
    = string_of_board (create_board ()))

let string_vertical_board_red _ =
  assert (
    string_of_board vertical_win_red_board
    = ".........\n.........\nR........\nR.B......\nR.B......\nR.B......\n")

let string_diagonal_board_blue _ =
  assert (
    string_of_board diagonal_win_board_blue
    = ".........\n.........\n...B.....\n..BR.....\n.BRR.....\nBRRB.....\n")

let invalid_move _ =
  let board = create_board () in
  assert (get_status board = InProgress);
  try
    let _ = make_move board Red (-1) in
    assert_failure "Expected\nInvalidMove exception"
  with
  | InvalidMove _ -> ()
  | _ -> assert_failure "Unexpected exception occurred"

let is_valid_move_true _ =
  let board = create_board () in
  assert (is_valid_move board 0 = true)

let is_valid_vertical_false _ =
  let board = create_board () in
  let board = make_move board Red 0 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 0 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 0 in
  let board = make_move board Blue 0 in
  assert (is_valid_move board 0 = false)

let is_valid_vertical_end_false _ =
  let board = create_board () in
  let board = make_move board Red 8 in
  let board = make_move board Blue 8 in
  let board = make_move board Red 8 in
  let board = make_move board Blue 8 in
  let board = make_move board Red 8 in
  let board = make_move board Blue 8 in
  assert (is_valid_move board 8 = false)

let is_valid_horizontal_false _ =
  let board = create_board () in
  assert (is_valid_move board 10 = false)

let test_copy_board _ =
  assert (vertical_win_red_board = copy_board vertical_win_red_board)

let score_draw_game _ =
  let board = draw_results_board_full_game in
  assert (score board = 0)

let copy_board_random_placement _ =
  let board = create_board () in
  let board = make_move board Red 0 in
  let board = make_move board Blue 6 in
  let board = make_move board Red 2 in
  let board = make_move board Blue 4 in
  let board = make_move board Red 1 in
  let board = make_move board Blue 8 in
  let new_board = copy_board board in
  assert (board = new_board)

let string_player_red _ = assert (string_of_player Red = "R")
let string_player_blue _ = assert (string_of_player Blue = "B")

let easy_best_move_depth_1 _ =
  let board = create_board () in
  let player = Red in
  let depth = 1 in
  let col = best_move board player depth 0 in
  assert (is_valid_move board col)

let ai_bigger_depth_valid _ =
  let board = create_board () in
  let player = Red in
  let depth = 3 in
  let col = best_move board player depth 0 in
  assert (is_valid_move board col)

let medium_best_move_depth_1 _ =
  let board = create_board () in
  let player = Red in
  let depth = 1 in
  let col = Medium.best_move board player depth 0 in
  assert (Medium.is_valid_move board col)

let easy_best_move_depth_2 _ =
  let board = create_board () in
  let player = Blue in
  let depth = 2 in
  let col = best_move board player depth 0 in
  assert (is_valid_move board col)

let medium_best_move_depth_2 _ =
  let board = create_board () in
  let player = Blue in
  let depth = 2 in
  let col = Medium.best_move board player depth 0 in
  assert (Medium.is_valid_move board col)

let reset_scores () =
  red_score := 0;
  blue_score := 0

let test_empty_board_score _ =
  let board = create_board () in
  assert_equal (score board) 0

let test_red_win_board_score _ =
  assert_equal (score horizontal_win_board_red) max_int

let test_blue_win_board_score _ =
  assert_equal (score horizontal_win_board_blue) min_int

let test_draw_board_score _ =
  assert_equal (score draw_results_board_full_game) 0

let test_in_progress_board_score _ =
  let board = create_board () in
  let _ = make_move board Red 0 in
  let _ = make_move board Blue 1 in
  assert_equal (score board) 0

let test_mixed_board_score _ =
  let board = create_board () in
  let _ = make_move board Red 0 in
  let _ = make_move board Blue 1 in
  let _ = make_move board Red 2 in
  let _ = make_move board Blue 3 in
  let _ = make_move board Red 4 in
  let _ = make_move board Blue 5 in
  let _ = make_move board Red 6 in
  assert_equal (score board) 0

let score_empty_board _ = assert (score (create_board ()) = 0)

let copy_board_diff _ =
  let original_board = create_board () in
  let copied_board = copy_board original_board in
  original_board.(0).(0) <- Some Red;
  assert (original_board <> copied_board)

let valid_move_partial_column _ =
  let board = create_board () in
  let _ = make_move board Red 0 in
  let _ = make_move board Blue 0 in
  let _ = make_move board Red 0 in
  let _ = make_move board Blue 0 in
  assert (is_valid_move board 0 = true)

let test_random_move_empty_board _ =
  let board = create_board () in
  let move = random_move board in
  assert (move >= 0 && move < cols)

let test_medium_random_move_empty_board _ =
  let board = create_board () in
  let move = Medium.random_move board in
  assert (move >= 0 && move < cols)

let test_random_move_partial_board _ =
  let move = random_move vertical_win_red_board in
  assert (move >= 0 && move < cols)

let test_medium_random_move_partial_board _ =
  let move = Medium.random_move vertical_win_red_board in
  assert (move >= 0 && move < cols)

let best_move _ =
  let board = create_board () in
  let player = Red in
  let depth = 3 in
  let move_count = 4 in
  let col = best_move board player depth move_count in
  assert (col >= 0 && col < cols)

let medium_best_move _ =
  let board = create_board () in
  let player = Red in
  let depth = 3 in
  let move_count = 4 in
  let col = Medium.best_move board player depth move_count in
  assert (col >= 0 && col < cols)

let score_updates_blue_lose _ =
  reset_scores ();
  assert (get_status vertical_win_red_board = Won Red);
  assert (!blue_score = 0);
  reset_scores ()

let pattern_matching_status board =
  match get_status board with
  | Won p -> begin
      match p with
      | Red -> red_score := !red_score + 1
      | Blue -> blue_score := !blue_score + 1
    end
  | _ -> print_endline "draw"

let score_updates_red_win_vert _ =
  reset_scores ();
  assert (get_status vertical_win_red_board = Won Red);
  pattern_matching_status vertical_win_red_board;
  assert (!red_score = 1);
  reset_scores ()

let score_updates_blue_win_vert _ =
  reset_scores ();
  assert (get_status vertical_win_blue_board = Won Blue);
  pattern_matching_status vertical_win_blue_board;
  assert (!blue_score = 1);
  reset_scores ()

let score_multiple_blue_wins_diagonal _ =
  reset_scores ();
  assert (get_status diagonal_win_board_blue = Won Blue);
  pattern_matching_status diagonal_win_board_blue;
  assert (!blue_score = 1);
  assert (!red_score = 0);
  assert (get_status diagonal_win_board_blue = Won Blue);
  pattern_matching_status diagonal_win_board_blue;
  assert (!blue_score = 2);
  assert (!red_score = 0);
  assert (get_status diagonal_win_board_blue = Won Blue);
  pattern_matching_status diagonal_win_board_blue;
  assert (!blue_score = 3);
  assert (!red_score = 0);
  assert (get_status diagonal_win_board_blue = Won Blue);
  pattern_matching_status diagonal_win_board_blue;
  assert (!blue_score = 4);
  assert (!red_score = 0);
  reset_scores ()

let score_multiple_red_diag _ =
  reset_scores ();
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 1);
  assert (!blue_score = 0);
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 2);
  assert (!blue_score = 0);
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 3);
  assert (!blue_score = 0);
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 4);
  assert (!blue_score = 0);
  reset_scores ()

let score_updates_red_win_diagonal _ =
  reset_scores ();
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 1);
  assert (!blue_score = 0);
  reset_scores ()

let test_start_time _ =
  let t1 = time () in
  let start = start_time () in
  let t2 = time () in
  assert (start >= t1 && start <= t2)

let test_duration _ =
  let start = start_time () in
  sleep 2;
  let elapsed = duration start in
  assert (elapsed > 1.8 && elapsed < 2.2)

let score_updates_blue_win_diagonal _ =
  reset_scores ();
  assert (get_status diagonal_win_board_blue = Won Blue);
  pattern_matching_status diagonal_win_board_blue;
  assert (!blue_score = 1);
  assert (!red_score = 0);
  reset_scores ()

let test_rapid_sequential_calls _ =
  let rec loop last_time n =
    if n > 0 then (
      let current_time = start_time () in
      assert (current_time >= last_time);
      loop current_time (n - 1))
    else ()
  in
  loop (start_time ()) 100

let test_long_duration _ =
  let start = start_time () in
  sleep 10;
  let elapsed = duration start in
  assert (elapsed > 9.8 && elapsed < 10.2)

let test_high_frequency_checking _ =
  for _ = 1 to 1000 do
    let start = start_time () in
    let d = duration start in
    assert (d >= 0.)
  done

let score_multiple_red_wins_diagonal _ =
  reset_scores ();
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 1);
  assert (!blue_score = 0);
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 2);
  assert (!blue_score = 0);
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 3);
  assert (!blue_score = 0);
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 4);
  assert (!blue_score = 0);
  reset_scores ()

let score_combination_wins_diagonal _ =
  reset_scores ();
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 1);
  assert (!blue_score = 0);
  assert (get_status diagonal_win_board_blue = Won Blue);
  pattern_matching_status diagonal_win_board_blue;
  assert (!red_score = 1);
  assert (!blue_score = 1);
  assert (get_status diagonal_win_board_blue = Won Blue);
  pattern_matching_status diagonal_win_board_blue;
  assert (!red_score = 1);
  assert (!blue_score = 2);
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 2);
  assert (!blue_score = 2);
  reset_scores ()

let score_updates_blue_win_horizontal _ =
  reset_scores ();
  assert (get_status horizontal_win_board_blue = Won Blue);
  pattern_matching_status horizontal_win_board_blue;
  assert (!blue_score = 1);
  assert (!red_score = 0);
  reset_scores ()

let score_updates_red_win_horizontal _ =
  reset_scores ();
  assert (get_status horizontal_win_board_red = Won Red);
  pattern_matching_status horizontal_win_board_red;
  assert (!blue_score = 0);
  assert (!red_score = 1);
  reset_scores ()

let score_multiple_red_vert _ =
  reset_scores ();
  assert (get_status vertical_win_red_board = Won Red);
  pattern_matching_status vertical_win_red_board;
  assert (!red_score = 1);
  assert (!blue_score = 0);
  assert (get_status vertical_win_red_board = Won Red);
  pattern_matching_status vertical_win_red_board;
  assert (!red_score = 2);
  assert (!blue_score = 0);
  assert (get_status vertical_win_red_board = Won Red);
  pattern_matching_status vertical_win_red_board;
  assert (!red_score = 3);
  assert (!blue_score = 0);
  reset_scores ()

let score_multiple_blue_vert _ =
  reset_scores ();
  assert (get_status vertical_win_blue_board = Won Blue);
  pattern_matching_status vertical_win_blue_board;
  assert (!blue_score = 1);
  assert (!red_score = 0);
  assert (get_status vertical_win_blue_board = Won Blue);
  pattern_matching_status vertical_win_blue_board;
  assert (!blue_score = 2);
  assert (!red_score = 0);
  assert (get_status vertical_win_blue_board = Won Blue);
  pattern_matching_status vertical_win_blue_board;
  assert (!blue_score = 3);
  assert (!red_score = 0);
  reset_scores ()

let score_multiple_blue_horizontal _ =
  reset_scores ();
  assert (get_status horizontal_win_board_blue = Won Blue);
  pattern_matching_status horizontal_win_board_blue;
  assert (!blue_score = 1);
  assert (!red_score = 0);
  assert (get_status horizontal_win_board_blue = Won Blue);
  pattern_matching_status horizontal_win_board_blue;
  assert (!blue_score = 2);
  assert (!red_score = 0);
  assert (get_status horizontal_win_board_blue = Won Blue);
  pattern_matching_status horizontal_win_board_blue;
  assert (!blue_score = 3);
  assert (!red_score = 0);
  reset_scores ()

let score_multiple_red_horizontal _ =
  reset_scores ();
  assert (get_status horizontal_win_board_red = Won Red);
  pattern_matching_status horizontal_win_board_red;
  assert (!blue_score = 0);
  assert (!red_score = 1);
  assert (get_status horizontal_win_board_red = Won Red);
  pattern_matching_status horizontal_win_board_red;
  assert (!blue_score = 0);
  assert (!red_score = 2);
  assert (get_status horizontal_win_board_red = Won Red);
  pattern_matching_status horizontal_win_board_red;
  assert (!blue_score = 0);
  assert (!red_score = 3);
  reset_scores ()

let score_draw _ =
  reset_scores ();
  assert (get_status draw_results_board_full_game = Draw);
  pattern_matching_status draw_results_board_full_game;
  assert (!red_score = 0);
  assert (!blue_score = 0);
  reset_scores ()

let score_multiple_blue_red_hor _ =
  reset_scores ();
  assert (get_status horizontal_win_board_red = Won Red);
  pattern_matching_status horizontal_win_board_red;
  assert (!red_score = 1);
  assert (!blue_score = 0);
  assert (get_status horizontal_win_board_blue = Won Blue);
  pattern_matching_status horizontal_win_board_blue;
  assert (!red_score = 1);
  assert (!blue_score = 1);
  assert (get_status horizontal_win_board_red = Won Red);
  pattern_matching_status horizontal_win_board_red;
  assert (!red_score = 2);
  assert (!blue_score = 1);
  assert (get_status horizontal_win_board_blue = Won Blue);
  pattern_matching_status horizontal_win_board_blue;
  assert (!red_score = 2);
  assert (!blue_score = 2);
  reset_scores ()

let score_multiple_blue_red_comb _ =
  reset_scores ();
  assert (get_status horizontal_win_board_red = Won Red);
  pattern_matching_status horizontal_win_board_red;
  assert (!red_score = 1);
  assert (!blue_score = 0);
  assert (get_status vertical_win_blue_board = Won Blue);
  pattern_matching_status vertical_win_blue_board;
  assert (!red_score = 1);
  assert (!blue_score = 1);
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 2);
  assert (!blue_score = 1);
  assert (get_status diagonal_win_board_red = Won Red);
  pattern_matching_status diagonal_win_board_red;
  assert (!red_score = 3);
  assert (!blue_score = 1);
  reset_scores ()

let test_suite =
  "test suite"
  >::: [
         "status in progress" >:: in_prog_status;
         "winning vertically red" >:: vertical_win_red;
         "winning vertically blue" >:: vertical_win_blue;
         "winning diagonally blue" >:: diagonal_win_blue;
         "winning diagonally red" >:: diagonal_win_red;
         "empty board" >:: empty_board;
         "player draw" >:: draw;
         "string of empty board" >:: string_board_empty;
         "string of vertical win board" >:: string_vertical_board_red;
         "string of diagonal win board" >:: string_diagonal_board_blue;
         "invalid move" >:: invalid_move;
         "winning horizontally red" >:: horizontal_win_red;
         "winning horizontally blue" >:: horizontal_win_blue;
         "valid move is true" >:: is_valid_move_true;
         "invalid vertical move is false" >:: is_valid_vertical_false;
         "invalid vertical move is false in the last column"
         >:: is_valid_vertical_end_false;
         "invalid horizontal move is false" >:: is_valid_horizontal_false;
         "testing copy of board" >:: test_copy_board;
         "horizontal string of board" >:: string_horizontal_board_red;
         "draw game score 0" >:: score_draw_game;
         "red player string" >:: string_player_red;
         "blue player string" >:: string_player_blue;
         "depth 1" >:: easy_best_move_depth_1;
         "depth 2" >:: easy_best_move_depth_2;
         "depth 1" >:: medium_best_move_depth_1;
         "depth 2" >:: medium_best_move_depth_2;
         "test_empty_board_score" >:: test_empty_board_score;
         "test_red_win_board_score" >:: test_red_win_board_score;
         "test_blue_win_board_score" >:: test_blue_win_board_score;
         "test_draw_board_score" >:: test_draw_board_score;
         "test_in_progress_board_score" >:: test_in_progress_board_score;
         "test_mixed_board_score" >:: test_mixed_board_score;
         "empty board score" >:: score_empty_board;
         "not same copy board" >:: copy_board_diff;
         "valid move for a partial column" >:: valid_move_partial_column;
         "random move empty" >:: test_random_move_empty_board;
         "random move partial" >:: test_random_move_partial_board;
         "best move" >:: best_move;
         "copying a random board" >:: copy_board_random_placement;
         "random move for medium level empty"
         >:: test_medium_random_move_empty_board;
         "random move partial board in medium level"
         >:: test_medium_random_move_partial_board;
         "best move for medium level" >:: medium_best_move;
         "in progress in between game" >:: in_prog_after_moves;
         "basic score updating" >:: score_updates_blue_lose;
         "score after single red win vertical" >:: score_updates_red_win_vert;
         "score after single blue win vertical" >:: score_updates_blue_win_vert;
         "score after single blue win horizontal"
         >:: score_updates_blue_win_horizontal;
         "score after single red win horizontal"
         >:: score_updates_red_win_horizontal;
         "score after single blue win diagonal board"
         >:: score_updates_blue_win_diagonal;
         "score after single red win diagonal board"
         >:: score_updates_red_win_diagonal;
         "score after multiple blue wins diagonal board"
         >:: score_multiple_blue_wins_diagonal;
         "score after multuple red wins diagonal board"
         >:: score_multiple_red_wins_diagonal;
         "score after multiple red wins vertical" >:: score_multiple_red_vert;
         "score after multiple blue wins vertical" >:: score_multiple_blue_vert;
         "score after combo of wins horizontal" >:: score_multiple_blue_red_hor;
         "score after combo of wins diagonal"
         >:: score_combination_wins_diagonal;
         "score after combo of wins in horizontal, vertical, and diagonal"
         >:: score_multiple_blue_red_comb;
         "score draw" >:: score_draw;
         "score multiple blue win horizontal" >:: score_multiple_blue_horizontal;
         "score multiple red win horizontal" >:: score_multiple_red_horizontal;
         "float type of time" >:: test_start_time;
         "tests if duration increases" >:: test_duration;
         "rapid tests for duration" >:: test_rapid_sequential_calls;
         "testing for long durations" >:: test_long_duration;
         "testing for high frequency" >:: test_high_frequency_checking;
         "score checking for multiple red win diag" >:: score_multiple_red_diag;
         "ai algorithm valid" >:: ai_bigger_depth_valid;
       ]

let _ = run_test_tt_main test_suite
