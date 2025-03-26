type piece =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | King
  | Queen

type color =
  | White
  | Black

type t = {
  piece : piece;
  color : color;
}

let valid_pattern _ _ _ = true
let get_color piece = piece.color
let get_type piece = piece.piece

let make_piece piece_type color_type =
  { piece = piece_type; color = color_type }
