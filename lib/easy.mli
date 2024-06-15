val random_move : 'a option array array -> int
(** [random_move] is a randomly selected valid column on the board. Requires:
    [board] is of type 'a option array array*)

val is_valid_move : 'a option array array -> int -> bool
(** [is_valid_move] is whether a move is valid for a column on the board.
    Requires: [board] is of type 'a option array array; [col] is of type int*)

val best_move :
  State.player option array array -> State.player -> int -> int -> int
(** [best_move] is the best move for a player in the easy level. Requires:
    [board] is of type State.player option array array; [player] is of type
    State.player; [depth] is of type int; [move_count] is of type int*)
