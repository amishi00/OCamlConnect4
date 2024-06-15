val is_valid_move : 'a option array array -> int -> bool
(** [is_valid_move] is whether a move is valid for a given column on the board.
    Requires: [board] is of type 'a option array array; [col] is of type int*)

val random_move : 'a option array array -> int
(** [random_move] is a random selected valid column on the baord. Requires:
    [board] is of type 'a option array array*)

val best_move :
  State.player option array array -> State.player -> int -> int -> int
(** [best_move] is the best move for a player in the medium level. Requires:
    [board] is of type State.player option array array; [player] is of type
    State.player; [depth] is of type int; [move_count] is of type int*)
