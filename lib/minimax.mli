val cols : int
(** [cols] is the number of columns in the board*)

val rows : int
(** [rows] is the number of rows in the board*)

val score : State.board -> int
(** [score] is a score based on the current game state. Requires: [board] is of
    type State.board*)

val copy_board : 'a option array array -> 'a option array array
(** [copy_board] is a copy of a given board. Requires: [board] is of type 'a
    option array array*)

val minimax :
  State.board -> State.player -> int -> int ref -> int ref -> bool -> int
(** [minimax] is a value representing the new board state after running the
    minimax algorithm on it. Requires: [board] is of type State.board; [player]
    is of type State.player; [depth] is of type int; [alpha] is of type int ref;
    [beta] is of type bool; [maxing] is of type int*)

val best_move : State.player option array array -> State.player -> int -> int
(** [best_move] is the column index of the best move for the player using the
    minimax algorithm. Requires: [board] is of type State.player option array
    array; [player] is of type State.player; [depth] is of type int*)
