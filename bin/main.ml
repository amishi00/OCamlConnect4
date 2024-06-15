(* @author: Ravnoor Bedi (rb895), Amishi Gupta (ag2424), Alisha Varma (av523) *)

open Final_3110.State
open Final_3110

let move_durations = ref []

let print_average_time () =
  if List.length !move_durations > 0 then
    let total_time = List.fold_left ( +. ) 0. !move_durations in
    let average_time =
      total_time /. float_of_int (List.length !move_durations)
    in
    Printf.printf "Average move time: %fs\n" average_time
  else Printf.printf "No moves were made.\n"

let rec play_game_ai board player =
  let start = start_time () in
  print_string "\nCurrent Board:\n";
  print_board board;
  print_newline ();
  match get_status board with
  | Won p ->
      let time_passed = duration start in
      move_durations := time_passed :: !move_durations;
      begin
        match p with
        | Red -> red_score := !red_score + 1
        | Blue -> blue_score := !blue_score + 1
      end;
      Printf.printf "Player %s Wins!\n" (string_of_player p);
      print_average_time ()
  | Draw ->
      Printf.printf "The game is a draw!\n";
      print_average_time ()
  | InProgress -> (
      match player with
      | Red ->
          Printf.printf "Player %s's turn. Enter column (0-8): "
            (string_of_player player);
          begin
            match read_line () with
            | exception End_of_file -> ()
            | input -> begin
                match int_of_string_opt input with
                | Some col when col >= 0 && col < 9 -> begin
                    try
                      let time_passed = duration start in
                      move_durations := time_passed :: !move_durations;
                      let updated_board = make_move board player col in
                      play_game_ai updated_board Blue
                    with InvalidMove msg ->
                      Printf.printf "Invalid move: %s\n" msg;
                      play_game_ai board player
                  end
                | _ ->
                    Printf.printf "Please enter a valid column number.\n";
                    play_game_ai board player
              end
          end
      | Blue ->
          let col = Minimax.best_move board player 4 in
          Printf.printf "AI (Blue) chooses column %d\n" col;
          let updated_board = make_move board player col in
          play_game_ai updated_board Red)

let update_scores_and_print_result player =
  match player with
  | Red -> red_score := !red_score + 1
  | Blue ->
      blue_score := !blue_score + 1;
      Printf.printf "Player %s Wins!\n" (string_of_player player);
      print_average_time ()

let handle_draw () =
  Printf.printf "The game is a draw!\n";
  print_average_time ()

let player_turn board player move_count start_time play_game_fn =
  Printf.printf "Player %s's turn. Enter column (0-8): "
    (string_of_player player);
  match read_line () with
  | exception End_of_file -> ()
  | input -> (
      match int_of_string_opt input with
      | Some col when col >= 0 && col < 9 -> (
          try
            let time_passed = duration start_time in
            move_durations := time_passed :: !move_durations;
            let updated_board = make_move board player col in
            play_game_fn updated_board
              (if player = Red then Blue else Red)
              (move_count + 1)
          with InvalidMove msg ->
            Printf.printf "Invalid move: %s\n" msg;
            play_game_fn board player move_count)
      | _ ->
          Printf.printf "Please enter a valid column number.\n";
          play_game_fn board player move_count)

let ai_turn board player move_count start_time depth best_move_fn play_game_fn =
  let col = best_move_fn board player depth move_count in
  Printf.printf "AI (Blue) chooses column %d\n" col;
  let time_passed = duration start_time in
  move_durations := time_passed :: !move_durations;
  let updated_board = make_move board player col in
  play_game_fn updated_board Red (move_count + 1)

let rec play_game_easy board player move_count =
  print_string "\nCurrent Board:\n";
  print_board board;
  print_newline ();
  let start = start_time () in
  match get_status board with
  | Won p -> update_scores_and_print_result p
  | Draw -> handle_draw ()
  | InProgress -> (
      match player with
      | Red -> player_turn board player move_count start play_game_easy
      | Blue ->
          ai_turn board player move_count start 4 Easy.best_move play_game_easy)

let rec play_game_medium board player move_count =
  print_string "\nCurrent Board:\n";
  print_board board;
  print_newline ();
  let start = start_time () in
  match get_status board with
  | Won p -> update_scores_and_print_result p
  | Draw -> handle_draw ()
  | InProgress -> (
      match player with
      | Red -> player_turn board player move_count start play_game_medium
      | Blue ->
          ai_turn board player move_count start 4 Medium.best_move
            play_game_medium)

let rec play_game_person board player =
  print_string "\nCurrent Board:\n";
  print_board board;
  print_newline ();
  let start = start_time () in
  match get_status board with
  | Won p ->
      begin
        match p with
        | Red -> red_score := !red_score + 1
        | Blue -> blue_score := !blue_score + 1
      end;
      Printf.printf "Player %s Wins!\n" (string_of_player p);
      print_average_time ()
  | Draw ->
      Printf.printf "The game is a draw!\n";
      print_average_time ()
  | InProgress ->
      Printf.printf "Player %s's turn. Enter column (0-8): "
        (string_of_player player);
      begin
        match read_line () with
        | exception End_of_file -> ()
        | input -> begin
            match int_of_string_opt input with
            | Some col when col >= 0 && col < 9 -> begin
                try
                  let time_passed = duration start in

                  move_durations := time_passed :: !move_durations;
                  let updated_board = make_move board player col in
                  play_game_person updated_board
                    (match player with
                    | Red -> Blue
                    | Blue -> Red)
                with InvalidMove msg ->
                  Printf.printf "Invalid move: %s\n" msg;
                  play_game_person board player
              end
            | _ ->
                Printf.printf "Please enter a valid column number.\n";
                play_game_person board player
          end
      end

let () =
  print_string "Welcome to Connect Four!\n\nThis is your current board:\n"

let display_string_1 =
  "\n\
  \ Type the specific number to either play the game or ask for help:\n\
  \ 1 for Easy Mode vs Computer\n\
  \ 2 for Medium Mode vs Computer\n\
  \ 3 for Hard Mode vs Computer\n\
  \ 4 for Multiplayer Mode\n\
  \ 5 to End Game\n\n\
  \ Type your choice: "

let display_string_2 =
  "Type the specific number to either play the game or ask for help to learn \
   how to play and learn about the specific modes:\n\
  \ 1 for Easy Mode vs Computer\n\
  \ 2 for Medium Mode vs Computer\n\
  \ 3 for Hard Mode vs Computer\n\
  \ 4 for Multiplayer Mode\n\
  \ 5 for Game Instructions\n\n\
   Type your choice: "

let option_1 board =
  play_game_easy board Red 0;
  Printf.printf "Red Player Score: %d\n" !red_score;
  Printf.printf "Blue Player Score: %d\n" !blue_score

let option_2 board =
  play_game_medium board Red 0;
  Printf.printf "Red Player Score: %d\n" !red_score;
  Printf.printf "Blue Player Score: %d\n" !blue_score

let option_3 board =
  play_game_ai board Red;
  Printf.printf "Red Player Score: %d\n" !red_score;
  Printf.printf "Blue Player Score: %d\n" !blue_score

let option_4 board =
  play_game_person board Red;
  Printf.printf "Red Player Score: %d\n" !red_score;
  Printf.printf "Blue Player Score: %d\n" !blue_score

let rec play_again () =
  let board = create_board () in
  print_string display_string_1;
  match read_line () with
  | "1" ->
      option_1 board;
      play_again ()
  | "2" ->
      option_2 board;
      play_again ()
  | "3" ->
      option_3 board;
      play_again ()
  | "4" ->
      option_4 board;
      play_again ()
  | "5" ->
      Printf.printf "You are done with the game. Congrats!\n";
      exit 0
  | _ ->
      Printf.printf "Invalid choice. Select again: \n";
      play_again ()

let () =
  let board = create_board () in
  print_endline (string_of_board board)

let info_string =
  "\n\
   To play Connect4, you are essentially trying to connect 4 of your\n\
   tokens to make a line of 4 tokens. This line can be made vertically,\n\
   horizontally, or diagonally. Upon each alternating turn between\n\
   players, the other player may try to block your connection of 4\n\
   tokens. You must try to also block the other player's connection\n\
   to prevent them from winning. The first player who makes a\n\
   connection of 4 tokens wins the round. In easy, medium, and hard \n\
   mode, you are playing against an AI. In easy mode, which is aimed for\n\
   beginner players, the hypothetical best move will be given to the\n\
   player. In multiplayer mode, you can play against another person on \n\
   the same computer, taking alternating turns. In every mode, you can \n\
   see the score of your player and your opponent. The score increments \n\
   by 1 for the winner of each round. You can play another round by \n\
   selecting a playing option when prompted to enter a number option at \n\
   the end of a round. The scores for the rounds will tally up after \n\
   each round, and be displayed after each game is completed.\n\n\
   Now select a choice again. Try playing the game!\n\n"

let rec main () =
  let board = create_board () in
  print_string display_string_2;
  match read_line () with
  | "1" ->
      option_1 board;
      play_again ()
  | "2" ->
      option_2 board;
      play_again ()
  | "3" ->
      option_3 board;
      play_again ()
  | "4" ->
      option_4 board;
      play_again ()
  | "5" ->
      print_string info_string;
      main ()
  | _ ->
      Printf.printf "Invalid choice. Select again: \n";
      main ()

let () = main ()
