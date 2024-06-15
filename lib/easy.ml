open State
open Minimax
open Medium

let () = Random.self_init ()

let random_move board =
  let rec find_valid_column () =
    let col = Random.int cols in
    if is_valid_move board col then col else find_valid_column ()
  in
  find_valid_column ()

let is_valid_move board col =
  if col < 0 || col >= cols then false
  else
    let rec check_column row =
      if row >= rows then false
      else
        match board.(row).(col) with
        | None -> true
        | Some _ -> check_column (row + 1)
    in
    check_column 0

let move_valuation player eval best_val best_move_curr col =
  if (player = Red && eval > !best_val) || (player = Blue && eval < !best_val)
  then (
    best_val := eval;
    best_move_curr := col)

let best_move board player depth move_count =
  if move_count mod 3 = 0 then random_move board
  else
    let alpha = ref min_int in
    let beta = ref max_int in
    let best_val = if player = Red then ref min_int else ref max_int in
    let best_move_curr = ref (cols - 1) in
    for col = 0 to cols - 1 do
      try
        let new_board = copy_board board in
        let eval =
          minimax
            (make_move new_board player col)
            (if player = Red then Blue else Red)
            depth alpha beta (player = Blue)
        in
        move_valuation player eval best_val best_move_curr col
      with InvalidMove _ -> ()
    done;
    !best_move_curr
