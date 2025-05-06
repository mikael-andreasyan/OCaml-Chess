type t
(**A ephemeral type that represents a board. It contains the board
   representation, current player color, and some other ueful information
   regarding castling, enpassant, etc.*)

type move
(**Type representation for a move. *)

(*Some useful constants*)
val pawn : int
val knight : int
val bishop : int
val rook : int
val queen : int
val king : int
val white : int
val black : int
val file1 : Base.Int64.t
val file8 : Base.Int64.t
val rankA : Base.Int64.t
val rankH : Base.Int64.t

val current_turn : t -> int
(**[current_turn board] returns the color's turn it currently is. 0 stands for
   white and 1 stands for black. *)

val make_board1 : int64 array array -> int -> int -> int64 -> t
(**[make_board1 board turn castlingRights enPassant] is a new board with the
   given values.*)

val make_board2 : string -> t
(**[make_board2 fen] is a new board with the given fen string.

   Requires: fen must be a valid fen string.*)

val legal_moves : t -> move Base.Array.t
(**[legal_moves board] is a list of all legal moves that can be done by a given
   board. *)

val legal_moves_pawn : t -> int * int -> move Base.Array.t
(**[legal_moves_pawn board (rank, file)] is a list of all legal moves that can
   be done by pawns on a given board and with a given pawn. *)

val legal_moves_knight : t -> int * int -> move Base.Array.t
(**[legal_moves_knight board (rank, file)] is a list of all legal moves that can
   be done by knights on a given board and with a given knight. *)

val legal_moves_bishop : t -> int * int -> move Base.Array.t
(**[legal_moves_bishop board (rank, file)] is a list of all legal moves that can
   be done by bishopes on a given board and with a given bishop. *)

val legal_moves_rook : t -> int * int -> move Base.Array.t
(**[legal_moves_rook board (rank, file)] is a list of all legal moves that can
   be done by rooks on a given board and with a given rook. *)

val legal_moves_queen : t -> int * int -> move Base.Array.t
(**[legal_moves_queen board (rank, file)] is a list of all legal moves that can
   be done by queens on a given board and with a given queen. *)

val legal_moves_king : t -> int * int -> move Base.Array.t
(**[legal_moves_king board (rank, file)] is a list of all legal moves that can
   be done by queens on a given board and with a given king. *)

val make_move : t -> move -> bool
(**[make_move board ((rank1, file1), (rank2, file2))] attempts to move the piece
   at [rank1, rank_st] to [rank2, file2]. If the movement is illegal, then the
   function returns false and makes no changes to the board. Otherwise, it
   mutates the current board to represent the move, changes board state, and
   advances the turn. Requires: [rank1, file1] and [rank2, file2] are valid
   positions on a chess board. For this function, the numbers 0-7 represent the
   letters a-h. Returns: a tuple with the first entry as the board (unchanged if
   move was invalid) and a boolean that says if the move was invalid*)

val unmake_move : t -> unit
(**[make_move board ((rank1, file1), (rank2, file2))] attempts to unmake a move.
   Requires: [rank1, file1] and [rank2, file2] are valid positions on a chess
   board. For this function, the numbers 0-7 represent the letters a-h. Returns:
   a tuple with the first entry as the board (unchanged if move was invalid) and
   a boolean that says if the move was invalid*)

val get_piece : t -> int * int -> int option
(**[get_piece board (rank, file)] gives the piece at the selected rank and file.
   It outputs an int where the 4th bit represents the color and the 1st to 3rd
   bit represent the piece type.

   Example: 1001 is a white pawn.

   Here is a key: Pawn = 001, Knight = 010, Bishop = 011, Rook = 100, Queen =
   101, King = 110.*)

val get_piece_bitBoard : t -> int -> int -> Base.Int64.t
(**[get_piece_bitBoard board pieceType color] gets the bit board of a give
   pieceType and color. *)

val bit_to_tuple : Base.Int64.t -> int * int
(**[bit_to_tuple bit] outputs the tuple of the chess posistion in the format
   (rank, file).*)

val tuple_to_bit : int * int -> Base.Int64.t
(**[tuple_to_bit (rank, file)] outputs the bit of the chess posistion with the
   given (rank, file).*)

val player_check : t -> int -> bool
(**[player_check board color] checks if the player of the givne color on the
   board is in check.*)

val printerMoveList : move Base.Array.t -> unit
(**[printerMoveList moveList] is a printed version of the entire board.*)

val printerBoard : t -> unit
(**[printer board] prints out a string of the board.*)
