open Piece

type t
(**A persistent type that represents a board. It contains the board
   representation, current player color, and number of moves played.*)

val current_turn : t -> int
(**[current_turn board] returns the color's turn it currently is. 0 stands for
   white and 1 stands for black. *)

val total_moves : t -> int
(**[total_moves board] is the total number of moves completed on a board. A move
   is when any player white or black moves their piece. Returns total number of
   moves played*)

(* val make_board : string -> int -> int -> t (**[make_board fen color moves] is
   a new board with a the given fen string applied to the posistion.

   Requires: that [fen] is a valid fen string. *)

   val legal_moves : t -> t Dynarray.t (**[legal_moves board] is a list of all
   legal moves that can be done by a given board. *)

   val make_move : t -> (int * int) * (int * int) -> t * bool (**[make_move
   board (file_st, rank_st) (file_end, rank_end)] attempts to move the piece at
   [file_st, rank_st] to [file_end, rank_end]. If the movement is illegal, then
   the function returns false and makes no changes to the board. Otherwise, it
   mutates the current board to represent the move, changes board state, and
   advances the turn. Requires: [file_st, rank_st] and [file_end, rank_end] are
   valid positions on a chess board. For this function, the numbers 0-7
   represent the letters a-h. Returns: a tuple with the first entry as the board
   (unchanged if move was invalid) and a boolean that says if the move was
   invalid*) *)
