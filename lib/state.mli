(** [player] is a variant representing the types of possible players*)
type player =
  | Red
  | Blue

type cell = player option
(** [cell] is a cell in the game board, which can contain a player or be empty
    (None)*)

type board = cell array array
(** [board] is the game board as a 2D array of cells. *)

(** [status] the status of the game: in progress, won by a player, or a draw.*)
type status =
  | InProgress
  | Won of player
  | Draw

exception InvalidMove of string
(** Exception raised when an invalid move is made. *)

val create_board : unit -> board
(** [create_board] is a new empty board with the defined number of rows and
    columns*)

val get_status : board -> status
(** [get_status] is the current status of the game based on the board. Requires:
    [board] is of type board.*)

val make_move : board -> player -> int -> board
(** [make_move] is a board with a move made for the given player in the
    specified column. Requires: [board] is of type board; [player] is of type
    player; [col] is of type int*)

val print_board : board -> unit
(** [print_board] prints the current state of the game board to the terminal.
    Requires: [board] is of type board*)

val string_of_player : player -> string
(** [string_of_player] is a string representation of a player. Requires:
    [player] is of type player*)

val string_of_board : board -> string
(** [string_of_board] is a string representation of a board. Requires: [board]
    is of type board*)

val red_score : int ref
(** [red_score] is a score tracker for the score of the red player*)

val blue_score : int ref
(** [blue_score] is a score tracker for the score of the blue player*)

val check_winner : 'a option array array -> 'a -> bool
(** [check_winner] is whether a player has won a game. Requires: [board] is of
    type 'a option array array; [player] is of type 'a *)

val start_time : unit -> float
(** [start_time] is the start time of when the function is called.*)

val duration : float -> float
(** [duration] is the value of time elapsed since the timer begins. Requires:
    [start] is of type float*)
