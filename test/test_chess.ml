open Chess
open OUnit2

(**[acc_move_array csv iterations acc] takes in a [csv] list of lists with the
   header removed and an [acc] that represents the current array. When first
   passed, [acc] should be an array equal to the length of the list and *)

(**[cvs_to_move_array csv] takes in a csv in which every row represents a chess
   move and that follows the format
   [start_file, start_rank, end_file, end_rank, piece_promotion]. The file and
   rank numbers follow the 0-7 numbering format. [piece_promotion] must have a
   value of [n] for [None], [b] for a bishop, [kn] for a knight, [r] for a rook,
   [k] for a king, and [q] for a queen. This function is used in processing
   larger move data sets. If the csv has a header row, the header row should be
   passed in ignored*)
let csv_to_move_array csv = ()

(**Starting position in chess and all of it's legal moves. All valid moves for
   white this turn*)
let board1 =
  Board.make_board2 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let board1_moves : Board.move array =
  [|
    ((0, 1), (0, 2), None);
    ((0, 1), (0, 3), None);
    ((1, 1), (1, 2), None);
    ((1, 1), (1, 3), None);
    ((2, 1), (2, 2), None);
    ((2, 1), (2, 3), None);
    ((3, 1), (3, 2), None);
    ((3, 1), (3, 3), None);
    ((4, 1), (4, 2), None);
    ((4, 1), (4, 3), None);
    ((5, 1), (5, 2), None);
    ((5, 1), (5, 3), None);
    ((6, 1), (6, 2), None);
    ((6, 1), (6, 3), None);
    ((7, 1), (7, 2), None);
    ((7, 1), (7, 3), None);
    ((1, 0), (0, 2), None);
    ((1, 0), (3, 2), None);
    ((6, 0), (5, 2), None);
    ((6, 0), (7, 2), None);
  |]

(**Starting position in chess after white moves a pawn. All valid moves for
   black this turn*)
let board2 =
  Board.make_board2
    "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

let board2_moves : Board.move array =
  [|
    ((0, 6), (0, 5), None);
    ((0, 6), (0, 4), None);
    ((1, 6), (1, 5), None);
    ((1, 6), (1, 4), None);
    ((2, 6), (2, 5), None);
    ((2, 6), (2, 4), None);
    ((3, 6), (3, 5), None);
    ((3, 6), (3, 4), None);
    ((4, 6), (4, 5), None);
    ((4, 6), (4, 4), None);
    ((5, 6), (5, 5), None);
    ((5, 6), (5, 4), None);
    ((6, 6), (6, 5), None);
    ((6, 6), (6, 4), None);
    ((7, 6), (7, 5), None);
    ((7, 6), (7, 4), None);
    ((1, 7), (0, 5), None);
    ((1, 7), (3, 5), None);
    ((6, 7), (7, 5), None);
    ((6, 7), (5, 5), None);
  |]

(* Board 3: White starting position *)
let board1 =
  Board.make_board2 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let board1_moves : Board.move array =
  [|
    ((0, 1), (0, 2), None);
    ((0, 1), (0, 3), None);
    ((1, 1), (1, 2), None);
    ((1, 1), (1, 3), None);
    ((2, 1), (2, 2), None);
    ((2, 1), (2, 3), None);
    ((3, 1), (3, 2), None);
    ((3, 1), (3, 3), None);
    ((4, 1), (4, 2), None);
    ((4, 1), (4, 3), None);
    ((5, 1), (5, 2), None);
    ((5, 1), (5, 3), None);
    ((6, 1), (6, 2), None);
    ((6, 1), (6, 3), None);
    ((7, 1), (7, 2), None);
    ((7, 1), (7, 3), None);
    ((1, 0), (0, 2), None);
    ((1, 0), (2, 2), None);
    ((6, 0), (5, 2), None);
    ((6, 0), (7, 2), None);
  |]

(* Board 3: Castling rights intact *)
let board3 = Board.make_board2 "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"

let board3_moves : Board.move array =
  [|
    (* Kingside castling *)
    ((4, 0), (6, 0), None);
    (* Queenside castling *)
    ((4, 0), (2, 0), None);
    (* Rook moves *)
    ((0, 0), (1, 0), None);
    ((0, 0), (2, 0), None);
    ((0, 0), (3, 0), None);
    ((7, 0), (6, 0), None);
    ((7, 0), (5, 0), None);
    (* King standard moves *)
    ((4, 0), (3, 0), None);
    ((4, 0), (5, 0), None);
  |]

(* Board 4: En passant *)
let board4 =
  Board.make_board2
    "rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP2PPP/RNBQKBNR w KQkq e6 0 3"

let board4_moves : Board.move array =
  [|
    (* En passant capture *)
    ((3, 4), (4, 5), None);
    (* Standard pawn moves *)
    ((0, 1), (0, 2), None);
    ((0, 1), (0, 3), None);
    ((1, 1), (1, 2), None);
    ((1, 1), (1, 3), None);
    ((2, 1), (2, 2), None);
    ((2, 1), (2, 3), None);
    ((5, 1), (5, 2), None);
    ((5, 1), (5, 3), None);
    ((6, 1), (6, 2), None);
    ((6, 1), (6, 3), None);
    ((7, 1), (7, 2), None);
    ((7, 1), (7, 3), None);
    (* Knight moves *)
    ((1, 0), (0, 2), None);
    ((1, 0), (2, 2), None);
    ((6, 0), (5, 2), None);
    ((6, 0), (7, 2), None);
  |]

(* Board 5: Promotion opportunity *)
let board5 = Board.make_board2 "4k3/3P4/8/8/8/8/8/4K3 w - - 0 1"

let board5_moves : Board.move array =
  [|
    (* Promotions *)
    ((3, 6), (3, 7), None);
    ((3, 6), (3, 7), None);
    ((3, 6), (3, 7), None);
    ((3, 6), (3, 7), None);
    (* King move *)
    ((4, 0), (3, 0), None);
    ((4, 0), (5, 0), None);
    ((4, 0), (3, 1), None);
    ((4, 0), (4, 1), None);
    ((4, 0), (5, 1), None);
  |]

(**[compare_moves name board expected] creates a test case with the name [name]
   where it gets the legal moveset from [board] and makes sure that said board
   has the same elements as [expected]*)
let compare_moves name board expected =
  name >:: fun _ ->
  assert_equal (Board.legal_moves board) expected ~printer:Board.printerMoveList

(**[move_exists name board expected_move] creates a test with the name [name]
   and makes sure that [expected_move] exists within the output for valid moves
   in [board]*)
let move_exists name board expected_move =
  name >:: fun _ ->
  assert_equal (Array.mem expected_move board) true ~printer:string_of_bool
(**Position where the white king is under check. They can only take the enemy
   queen*)
(*rn2kbnr/ppp1pppp/8/3p4/8/5P2/PPPPq1PP/RNB1K2R w KQkq - 0 7*)

(**Position where the white kind is under check. They can either block the check
   with their knight or move behind the knight*)
(* "1K5r/1N5r/8/8/8/8/8/5k2 w - - 0 1" *)

(**Position where the black king is under check. They can only move their knight
   2 ways to defend from the check*)

(**Legal move test generation*)
(*tests would involve comparing the array output of board.mli to the expected
  output*)

(**Pawn moveset tests*)
(*Tests would involve making positions where a certain pawn move is expected and
  checking if said move exists in the array*)

(**Castling tests*)
(*Tests where we create positions where the king can castle and check if said
  castling moves exist*)

(**Checking tests*)
(*Tests where we see if certain positions where a piece should be checked
  properly recognize checking*)

(**Very basic engine tests that ensure the engine can at least give back basic
   moves*)
(*Tests would involve checking if the move that the engine returns is at least
  one of the expected legal moves for the board*)

(**Advanced engine tests that checks if it can notice positions where the queen
   is being blundered*)
(*Test would involve passing a board position and saying that move we expect
  back*)
