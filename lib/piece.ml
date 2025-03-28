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

let valid_pattern (file_st, rank_st) (file_end, rank_end) piece =
  let rank_diff = rank_end - rank_st in
  let file_diff = file_end - file_st in
  match piece.piece with
  | Pawn -> (
      match piece.color with
      | White -> rank_diff = 1 || (rank_st = 1 && rank_diff = 2)
      | Black -> rank_diff = -1 || (rank_st = 6 && rank_diff = -2))
  | Knight ->
      (Int.abs rank_diff = 2 && Int.abs file_diff = 1)
      || (Int.abs rank_diff = 1 && Int.abs file_diff = 2)
  | Bishop -> Int.abs rank_diff = Int.abs file_diff
  | Rook ->
      (file_diff <> 0 && rank_diff = 0) || (rank_diff <> 0 && file_diff = 0)
  | King -> Int.abs rank_diff = 1 || Int.abs file_diff = 1
  | Queen ->
      (file_diff <> 0 && rank_diff = 0)
      || (rank_diff <> 0 && file_diff = 0)
      || Int.abs rank_diff = Int.abs file_diff

let get_color piece = piece.color
let get_type piece = piece.piece

let make_piece color_type piece_type =
  { piece = piece_type; color = color_type }
