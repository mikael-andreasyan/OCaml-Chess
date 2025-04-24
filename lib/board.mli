open Piece

type t
(**A persistent type that represents a board*)

val make_move : t -> int * int -> int * int -> t * bool
(**[make_move board (file_st, rank_st) (file_end, rank_end)] attempts to move
   the piece at [file_st, rank_st] to [file_end, rank_end]. If the movement is
   illegal, then the function returns false and makes no changes to the board.
   Otherwise, it mutates the current board to represent the move, changes board
   state, and advances the turn. Requires: [file_st, rank_st] and
   [file_end, rank_end] are valid positions on a chess board. For this function,
   the numbers 0-7 represent the letters a-h. Returns: a tuple with the first
   entry as the board (unchanged if move was invalid) and a boolean that says if
   the move was invalid*)

val make_board : unit -> t
(**Creates a fresh instance of the board*)

val current_turn : t -> Piece.color
(**Returns which color's turn it currently is*)

val get_piece : t -> int * int -> Piece.t option
(**[get_piece (file, rank)] returns [Some piece] if there is a piece present at
   (file, rank). Otherwise, return [None]*)

val legal_moves : t -> ((int * int) * (int * int)) list
(**[legal_moves board] returns a list of all the legal moves possible in the
   board. In each given entry, the first tuple represents starting position and
   the second one represents the ending position of a piece*)

val total_moves : t -> int
(**A move is when any player white or black moves their piece. Returns total
   number of moves played*)
