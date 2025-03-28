type t = {
  mutable board : Piece.t option array array;
  mutable curr_turn : Piece.color;
}
(**AF: the board is represented by option Piece.t array array*)

(**returns if the piece tried to move to a tile with the same color on it*)
let same_color board piece (file_end, rank_end) =
  match board.board.(rank_end).(file_end) with
  | None -> false
  | Some other_piece -> Piece.get_color piece = Piece.get_color other_piece

let piece_exists board rank file =
  match board.board.(rank).(file) with
  | Some _ -> true
  | None -> false

(**Since the movement of pawns is such a special case, this function separately
   determines if a piece would be in the way of a pawn move*)
(* let valid_pawn_move board (file_st, rank_st) (file_end, rank_end) = m *)

(**[piece_in_way board (file_st, rank_st) (file_end, rank_end)] checks if a
   piece is in the way of the given move. Only works for moves that are diagonal
   or straight, otherwise assumes it is a knight move and the piece can jump
   over*)
let piece_in_way board (file_st, rank_st) (file_end, rank_end) =
  let valid = ref true in
  let file_diff = file_end - file_st in
  let rank_diff = rank_end - rank_st in
  if file_diff <> 0 && rank_diff = 0 then
    if file_diff < 0 then
      for x = file_end to file_st do
        if x <> file_st && x <> file_end then
          if piece_exists board rank_st x then valid := false else ()
        else ()
      done
    else if file_diff > 0 then
      for x = file_st to file_end do
        if x <> file_st && x <> file_end then
          if piece_exists board rank_st x then valid := false else ()
        else ()
      done
    else ()
  else if file_diff = 0 && rank_diff <> 0 then
    if rank_diff < 0 then
      for x = rank_end to rank_st do
        if x <> rank_st && x <> rank_end then
          if piece_exists board x file_st then valid := false else ()
        else ()
      done
    else if rank_diff > 0 then
      for x = rank_st to rank_end do
        if x <> rank_st && x <> rank_end then
          if piece_exists board x file_st then valid := false else ()
        else ()
      done
    else ()
  else if Int.abs file_diff = Int.abs rank_diff then
    let rank_mult = if rank_diff < 0 then -1 else 1 in
    let file_mult = if file_diff < 0 then -1 else 1 in
    for x = 1 to Int.abs file_diff - 1 do
      if
        piece_exists board
          (rank_st + (x * rank_mult))
          (file_st + (x * file_mult))
      then valid := false
    done
  else ();
  !valid

(**Only for checking validity of pawn moves. Returns true in all other cases*)
let valid_pawn_move board (file_st, rank_st) (file_end, rank_end) =
  let piece = board.board.(rank_st).(file_st) in
  match piece with
  | Some piece -> (
      match Piece.get_type piece with
      | Pawn ->
          let rank_diff = rank_end - rank_st in
          let file_diff = file_end - file_st in
          if (Int.abs rank_diff = 1 || Int.abs rank_diff = 2) && file_diff = 0
          then not (piece_exists board rank_end file_end)
          else if Int.abs file_diff = Int.abs rank_diff then
            piece_exists board rank_end file_end
          else true
      | _ -> true)
  | None -> true

let make_move board (file_st, rank_st) (file_end, rank_end) =
  let piece = board.board.(rank_st).(file_st) in
  match piece with
  | Some piece ->
      if
        Piece.valid_pattern (file_st, rank_st) (file_end, rank_end) piece
        && piece_in_way board (file_st, rank_st) (file_end, rank_end)
        && (not (same_color board piece (file_end, rank_end)))
        && valid_pawn_move board (file_st, rank_st) (file_end, rank_end)
      then (
        board.board.(rank_end).(file_end) <- Some piece;
        board.board.(rank_st).(file_st) <- None;
        (match board.curr_turn with
        | White -> board.curr_turn <- Black
        | Black -> board.curr_turn <- White);
        true)
      else false
  | None -> false

let setup array =
  array.(7).(0) <- Some Piece.(make_piece Black Rook);
  array.(7).(1) <- Some Piece.(make_piece Black Knight);
  array.(7).(2) <- Some Piece.(make_piece Black Bishop);
  array.(7).(3) <- Some Piece.(make_piece Black Queen);
  array.(7).(4) <- Some Piece.(make_piece Black King);
  array.(7).(5) <- Some Piece.(make_piece Black Bishop);
  array.(7).(6) <- Some Piece.(make_piece Black Knight);
  array.(7).(7) <- Some Piece.(make_piece Black Rook);
  for x = 0 to 7 do
    array.(6).(x) <- Some Piece.(make_piece Black Pawn)
  done;
  array.(0).(0) <- Some Piece.(make_piece White Rook);
  array.(0).(1) <- Some Piece.(make_piece White Knight);
  array.(0).(2) <- Some Piece.(make_piece White Bishop);
  array.(0).(3) <- Some Piece.(make_piece White Queen);
  array.(0).(4) <- Some Piece.(make_piece White King);
  array.(0).(5) <- Some Piece.(make_piece White Bishop);
  array.(0).(6) <- Some Piece.(make_piece White Knight);
  array.(0).(7) <- Some Piece.(make_piece White Rook);
  for x = 0 to 7 do
    array.(1).(x) <- Some Piece.(make_piece White Pawn)
  done;
  array

(**in this implementation the first array is the rank (rows) and the second
   array are the files (cols).*)
let make_board () =
  {
    board =
      setup
        (Array.init 8 (fun _ -> Array.init 8 (fun _ -> (None : Piece.t option))));
    curr_turn = White;
  }

(**Returns which color's turn it currently is*)
let current_turn board = board.curr_turn

let get_piece board (file, rank) = board.board.(rank).(file)
