type player =
  | Red
  | Blue

type cell = player option
type board = cell array array

type status =
  | InProgress
  | Won of player
  | Draw

exception InvalidMove of string

let rows = 6
let cols = 7
let create_board () = Array.make_matrix rows cols None

let check_winner board player =
  let check_direction (r_increment, c_increment) r_curr c_curr =
    let rec aux r c count =
      if r >= 0 && r < rows && c >= 0 && c < cols && count < 4 then
        match board.(r).(c) with
        | Some p when p = player ->
            aux (r + r_increment) (c + c_increment) (count + 1)
        | _ -> count
      else count
    in
    aux r_curr c_curr 0 >= 4
  in
  let directions =
    [ (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (1, 1); (-1, 1); (1, -1) ]
  in
  let rec check_cells r =
    if r >= rows then false
    else
      let rec check_row c =
        if c >= cols then false
        else
          List.exists (fun dir -> check_direction dir r c) directions
          || check_row (c + 1)
      in
      check_row 0 || check_cells (r + 1)
  in
  check_cells 0

let get_status board =
  if check_winner board Red then Won Red
  else if check_winner board Blue then Won Blue
  else if Array.for_all (fun row -> Array.for_all Option.is_some row) board then
    Draw
  else InProgress

let make_move board player col =
  if col < 0 || col >= cols then raise (InvalidMove "Column out of bounds");
  let rec find_open_row r =
    if r >= rows then raise (InvalidMove "Column is full")
    else
      match board.(r).(col) with
      | None ->
          board.(r).(col) <- Some player;
          board
      | Some _ -> find_open_row (r + 1)
  in
  find_open_row 0

let string_of_player = function
  | Red -> "R"
  | Blue -> "B"

let string_of_cell = function
  | None -> "."
  | Some player -> string_of_player player

let string_of_board board =
  Array.map
    (fun row ->
      Array.map string_of_cell row |> Array.to_list |> String.concat "")
    board
  |> Array.to_list |> List.rev |> String.concat "\n"
