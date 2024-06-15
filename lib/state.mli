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

val create_board : unit -> board
val get_status : board -> status
val make_move : board -> player -> int -> board
val string_of_board : board -> string
val string_of_player : player -> string
