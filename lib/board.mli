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

val make_board : int64 array array -> int -> int -> int -> int64 -> t
(**[make_board1 board turn moves castlingRights enPassant] is a new board with
   the given values.*)

val legal_moves : t -> ((int * int) * (int * int)) Base.Queue.t
(**[legal_moves board] is a list of all legal moves that can be done by a given
   board. *)

val legal_moves_bishop : t -> ((int * int) * (int * int)) Base.Queue.t
val legal_moves_rook : t -> ((int * int) * (int * int)) Base.Queue.t
val legal_moves_queen : t -> ((int * int) * (int * int)) Base.Queue.t
val legal_moves_king : t -> ((int * int) * (int * int)) Base.Queue.t
val printerMoveList : ((int * int) * (int * int)) Base.Queue.t -> unit

val make_move : t -> (int * int) * (int * int) -> bool * t
(**[make_move board (file_st, rank_st) (file_end, rank_end)] attempts to move
   the piece at [file_st, rank_st] to [file_end, rank_end]. If the movement is
   illegal, then the function returns false and makes no changes to the board.
   Otherwise, it mutates the current board to represent the move, changes board
   state, and advances the turn. Requires: [file_st, rank_st] and
   [file_end, rank_end] are valid positions on a chess board. For this function,
   the numbers 0-7 represent the letters a-h. Returns: a tuple with the first
   entry as the board (unchanged if move was invalid) and a boolean that says if
   the move was invalid*)

(* val unmake_move : t -> (int * int) * (int * int) -> bool * t *)

val printerBoard : t -> string
(**[printer board] prints out a string of the board.*)

val get_piece : t -> int * int -> int option
(**[get_piece board (rank, file)] gives the piece at the selected rank and file.
   It outputs an int where the 4th bit represents the color and the 1st to 3rd
   bit represent the piece type.

   Example: 1001 is a white pawn.

   Here is a key: Pawn = 001, Knight = 010, Bishop = 011, Rook = 100, Queen =
   101, King = 110.*)
