(* Citation:
   https://www.hackerearth.com/blog/developers/minimax-algorithm-alpha-beta-pruning/*)

open State

let cols = 9
let rows = 6

let score board =
  match get_status board with
  | Won Red -> max_int
  | Won Blue -> min_int
  | Draw -> 0
  | InProgress -> 0

let copy_board board =
  let new_board = Array.make_matrix rows cols None in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      new_board.(r).(c) <- board.(r).(c)
    done
  done;
  new_board

let rec minimax board player depth alpha beta maxing =
  let maximize_ai board player depth alpha beta =
    let best_score = ref min_int in
    for col = 0 to cols - 1 do
      try
        let new_board = copy_board board in
        let eval =
          minimax
            (make_move new_board player col)
            (if player = Red then Blue else Red)
            (depth - 1) alpha beta false
        in
        best_score := max !best_score eval;
        if !best_score >= !beta then raise Exit;
        alpha := max !alpha !best_score
      with InvalidMove _ | Exit -> ()
    done;
    !best_score
  in

  let minimize_player board player depth alpha beta =
    let best_score = ref max_int in
    for col = 0 to cols - 1 do
      try
        let new_board = copy_board board in
        let eval =
          minimax
            (make_move new_board player col)
            (if player = Red then Blue else Red)
            (depth - 1) alpha beta true
        in
        best_score := min !best_score eval;
        if !best_score <= !alpha then raise Exit;
        beta := min !beta !best_score
      with InvalidMove _ | Exit -> ()
    done;
    !best_score
  in

  match get_status board with
  | Won _ | Draw -> score board
  | InProgress ->
      if depth = 0 then score board
      else if maxing then maximize_ai board player depth alpha beta
      else minimize_player board player depth alpha beta

let best_move board player depth =
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
      if
        (player = Red && eval > !best_val) || (player = Blue && eval < !best_val)
      then (
        best_val := eval;
        best_move_curr := col)
    with InvalidMove _ -> ()
  done;
  !best_move_curr
