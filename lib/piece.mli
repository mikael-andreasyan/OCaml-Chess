type piece =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | King
  | Queen  (**Represents the type of a piece*)

type color =
  | White
  | Black  (**Represents the color of the piece*)

type t
(**An abstract type representing a piece*)

val valid_pattern : int * int -> int * int -> t -> bool
(**[valid_move (file_st, rank_st) (file_end, rank_end) piece] returns if a given
   move starting from [file_st, rank_st] to [file_end,rank_end] matches the
   movement pattern of the specified piece. For pawns, this function assumes
   diagonal movements are a valid pattern, so checking for if the move is
   possible will fall on the board.ml adjusted. This function is not able to
   check if a certain move is legal however (for example other pieces blocking
   the path or a piece not being able to move because it would put the king in
   check.). Note: The [file] is the letter associated with the column that the
   piece is on. For this function, the numbers 0-7 represent the letters a-h.
   Requires: [file_st, rank_st] and [file_end, rank_end] are valid positions on
   a chess board*)

val get_color : t -> color
(**[get_color piece] returns the color of the piece*)

val get_type : t -> piece
(**[get_type piece] returns the piece type of [piece]*)

val make_piece : color -> piece -> t
(**[make_piece color type] makes a piece with the type [type] and the color
   [color]*)
