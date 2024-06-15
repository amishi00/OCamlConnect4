(*@author Ravnoor Bedi (rb895), Alisha Varma (av523), Amishi Gupta (ag2424)*)
open Final_3110.State

let rec play_game board player =
  Printf.printf "\nCurrent Board:\n%s\n" (string_of_board board);
  match get_status board with
  | Won p -> Printf.printf "Player %s Wins!\n" (string_of_player p)
  | Draw -> Printf.printf "The game is a draw!\n"
  | InProgress -> (
      Printf.printf "Player %s's turn. Enter column (0-6): "
        (string_of_player player);
      match read_line () with
      | exception End_of_file -> ()
      | input -> (
          match int_of_string_opt input with
          | Some col when col >= 0 && col < 7 -> (
              try
                let updated_board = make_move board player col in
                play_game updated_board (if player = Red then Blue else Red)
              with InvalidMove msg ->
                Printf.printf "Invalid move: %s\n" msg;
                play_game board player)
          | _ ->
              Printf.printf "Please enter a valid column number.\n";
              play_game board player))

let main () =
  let board = create_board () in
  play_game board Red

let () = main ()
