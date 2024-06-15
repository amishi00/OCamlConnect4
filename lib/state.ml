open Unix

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

let start_time () = time ()
let duration start = time () -. start


let rows = 6
let cols = 9
let create_board () = Array.make_matrix rows cols None
let red_score = ref 0
let blue_score = ref 0

let direction_check c r player board count aux r_increment c_increment =
  if r >= 0 && r < rows && c >= 0 && c < cols && count < 4 then
    match board.(r).(c) with
    | Some p when p = player ->
        aux (r + r_increment) (c + c_increment) (count + 1)
    | _ -> count
  else count

let directions =
  [ (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (1, 1); (-1, 1); (1, -1) ]

let check_direction board player (r_increment, c_increment) r_curr c_curr =
  let rec aux r c count =
    direction_check c r player board count aux r_increment c_increment
  in
  aux r_curr c_curr 0 >= 4

let rec check_cells board player r =
  if r >= rows then false
  else
    let rec check_row c =
      if c >= cols then false
      else if check_directions board player r c then true
      else check_row (c + 1)
    and check_directions board player r c =
      let rec check_dir_list dirs =
        match dirs with
        | [] -> false
        | dir :: rest ->
            if check_direction board player dir r c then true
            else check_dir_list rest
      in
      check_dir_list directions
    in
    if check_row 0 then true else check_cells board player (r + 1)

let check_winner board player = check_cells board player 0

let rec check_full board row =
  if row >= Array.length board then true
  else if Array.exists Option.is_none board.(row) then false
  else check_full board (row + 1)

let get_status board =
  if check_winner board Red then Won Red
  else if check_winner board Blue then Won Blue
  else if check_full board 0 then Draw
  else InProgress

let rec find_open_row board player col r =
  if r >= rows then raise (InvalidMove "Column is full")
  else
    match board.(r).(col) with
    | None ->
        board.(r).(col) <- Some player;
        board
    | Some _ -> find_open_row board player col (r + 1)

let make_move board player col =
  if col < 0 || col >= cols then raise (InvalidMove "Column out of bounds");
  find_open_row board player col 0

let string_of_player = function
  | Red -> "R"
  | Blue -> "B"

let string_of_cell = function
  | None -> ANSITerminal.print_string [ ANSITerminal.green ] "."
  | Some player -> (
      match player with
      | Blue ->
          ANSITerminal.print_string [ ANSITerminal.blue ]
            (string_of_player player)
      | Red ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            (string_of_player player))

let print_board board =
  for i = Array.length board - 1 downto 0 do
    let row = board.(i) in
    let row_str = ref ("" ^ string_of_int i ^ "||") in
    print_string !row_str;
    for j = 0 to Array.length row - 1 do
      string_of_cell row.(j);
      print_string "|"
    done;
    print_endline "|"
  done;
  print_string "   ";
  for j = 0 to 8 do
    print_int j;
    print_string " "
  done;
  ()

let string_of_board board =
  let str_builder = ref "" in
  for i = Array.length board - 1 downto 0 do
    let row = board.(i) in
    for j = 0 to Array.length row - 1 do
      match row.(j) with
      | None -> str_builder := !str_builder ^ "."
      | Some player -> str_builder := !str_builder ^ string_of_player player
    done;
    str_builder := !str_builder ^ "\n"
  done;
  !str_builder
