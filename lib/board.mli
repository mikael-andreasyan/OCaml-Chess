open Piece

type t
(**A mutable type that represents a piece*)

(**A type that represents which color is currently moving*)
type turn =
  | White
  | Black

val make_move : t -> int * int -> int * int -> bool
(**[make_move board (file_st, rank_st) (file_end, rank_end)] attempts to move
   the piece at [file_st, rank_st] to [file_end, rank_end]. If the movement is
   illegal, then the function returns false and makes no changes to the board.
   Otherwise, it mutates the current board to represent the move, changes board
   state, and advances the turn.*)

val make_board : unit -> t
(**Creates a fresh instance of the board*)

val current_turn : t -> turn
(**Returns which color's turn it currently is*)

val get_piece : int * int -> Piece.t option
(**[get_piece (file, rank)] returns [Some piece] if there is a piece present at
   (file, rank). Otherwise, return [None]*)
