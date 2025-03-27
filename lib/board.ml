type t = {
  mutable board : Piece.t option array array;
  mutable curr_turn : Piece.color;
}
(**AF: the board is represented by option Piece.t array array*)

let make_move board (file_st, rank_st) (file_end, rank_end) =
  let piece = board.board.(rank_st).(file_st) in
  match piece with
  | Some piece ->
      if Piece.valid_pattern (file_st, rank_st) (file_end, rank_end) piece then (
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
