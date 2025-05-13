open Chess
open OUnit2
open Csv

(*==========================HELPER FUNCTIONS=================================*)

(**[acc_move_list csv iterations acc] takes in a [csv] list of lists with the
   header removed and an [acc] that represents the current array. When first
   passed, [acc] should be an array equal to the length of the list and be an
   array of moves*)
let rec acc_move_array csv array =
  match csv with
  | [] -> array
  | h :: t -> (
      match h with
      | [ file_start; rank_start; file_end; rank_end; promotion ] ->
          let curr_move =
            match promotion with
            | "n" ->
                ( (int_of_string file_start, int_of_string rank_start),
                  (int_of_string file_end, int_of_string rank_end),
                  None )
            | "kn" ->
                ( (int_of_string file_start, int_of_string rank_start),
                  (int_of_string file_end, int_of_string rank_end),
                  Some Board.knight )
            | "r" ->
                ( (int_of_string file_start, int_of_string rank_start),
                  (int_of_string file_end, int_of_string rank_end),
                  Some Board.rook )
            | "q" ->
                ( (int_of_string file_start, int_of_string rank_start),
                  (int_of_string file_end, int_of_string rank_end),
                  Some Board.queen )
            | "b" ->
                ( (int_of_string file_start, int_of_string rank_start),
                  (int_of_string file_end, int_of_string rank_end),
                  Some Board.bishop )
            | _ -> failwith "not a valid move"
          in
          let array_length = Array.length array in
          array.(array_length - List.length csv) <- curr_move;
          acc_move_array t array
      | _ ->
          failwith
            "The CSV doesn't follow the correct formatting of start_file, \
             start_rank, end_file, end_rank, piece_promotion")

(**[cvs_to_move_array csv] takes in a csv in which every row represents a chess
   move and that follows the format
   [start_file, start_rank, end_file, end_rank, piece_promotion]. The file and
   rank numbers follow the 0-7 numbering format. [piece_promotion] must have a
   value of [n] for [None], [b] for a bishop, [kn] for a knight, [r] for a rook,
   [k] for a king, and [q] for a queen. This function is used in processing
   larger move data sets. If the csv has a header row, the header row should be
   passed in ignored*)
let csv_to_move_array csv =
  match csv with
  | [] -> failwith "the testing csv had nothing"
  | h :: t ->
      acc_move_array t (Array.make (List.length t) ((0, 0), (0, 0), None))

let rec compare_lists_as_sets lst1 lst2 =
  match lst1 with
  | [] -> true
  | h :: t -> if List.mem h lst2 then compare_lists_as_sets t lst2 else false

(**[compare_move_arrays expected real] compares two move arrays.*)
let compare_move_arrays expected real =
  let expected_list = Array.to_list expected in
  let real_list =
    List.filter (fun (st, _, _) -> st <> (-1, -1)) (Array.to_list real)
  in
  if List.length expected_list = List.length real_list then
    compare_lists_as_sets expected_list real_list
  else false
(*note: the conversion to lists + filtering is necessary because the board
  output array has padded entries*)

(**[compare_moves name board expected] creates a test case with the name [name]
   where it gets the legal moveset from [board] and makes sure that said board
   has the same elements as [expected]*)
let make_compare_moves name board expected =
  name >:: fun _ ->
  assert_equal ~cmp:compare_move_arrays expected (Board.legal_moves board)
    ~printer:Board.printerMoveList

(**[make_move_exists name board expected_move] creates a test with the name
   [name] and makes sure that [expected_move] exists within the output for valid
   moves in [board]*)
let make_move_exists name board expected_move =
  name >:: fun _ ->
  assert_equal
    (Array.mem expected_move (Board.legal_moves board))
    true ~printer:string_of_bool

(**[make_engine_move_valid name board expected_moves] checks if the engine, when
   taking in the current [board], is able to make a move that is considered
   valid*)
let make_engine_move_valid name board expected_moves =
  name >:: fun _ ->
  assert_equal
    (Array.mem (Engine.get_move board) (Board.legal_moves board))
    true

(**[make_valid_move name board move] attempts to make the [move] on the [board].
   It is expected that this move is a valid move, so the function checks if the
   original piece moved to the new location on the board and that the turn has
   changed over to the other player*)
let make_valid_move name board move =
  match move with
  | (f_st, r_st), (f_end, r_end), _ ->
      let start_turn = Board.current_turn board in
      let start_piece = Board.get_piece board (f_st, r_st) in
      let boolean = Board.make_move board move in
      let end_turn = Board.current_turn board in
      let end_piece = Board.get_piece board (f_end, r_end) in
      Board.unmake_move board;
      name >:: fun _ ->
      assert_equal
        (start_turn <> end_turn && start_piece = end_piece && boolean)
        true

(**[make_invalid_move name board move] attempts to make the [move] on the
   [board]. It is expected that this is not a valid move, so the original square
   should stay unchanged and turn order should not change*)
let make_invalid_move name board move =
  match move with
  | (f_st, r_st), (f_end, r_end), _ ->
      let start_turn = Board.current_turn board in
      let start_piece = Board.get_piece board (f_st, r_st) in
      let boolean = Board.make_move board move in
      let end_turn = Board.current_turn board in
      let end_piece = Board.get_piece board (f_st, r_st) in
      Board.unmake_move board;
      name >:: fun _ ->
      assert_equal
        (start_turn = end_turn && start_piece = end_piece && boolean)
        true

(**[make_king_replace_test name board eat_move expected_king_pos] makes a test
   for the primary rule of bloody chess: king replacement of higher value
   pieces. [eat_move] is a valid move, that when done, eats the king.
   [expected_king_pos] is where the king should end up after being eaten*)
let make_king_replace_test name board eat_move expected_king_pos = ()

(**[make_game_over_test name board finishing_move] makes a test for the main
   game-over condition of bloody chess: when the king is the only piece left
   standing*)
let make_game_over_test name board eat_move = ()

(*========================BOARDS AND EXPECTED MOVES =====================*)

(**Starting position in chess and all of it's legal moves. All valid moves for
   white this turn*)
let board1 =
  Board.make_board2 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let board1_moves = csv_to_move_array (Csv.load "../data/board1moves.csv")

(**Starting position in chess after white moves a pawn. All valid moves for
   black this turn*)
let board2 =
  Board.make_board2
    "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

let board2_moves = csv_to_move_array (Csv.load "../data/board2moves.csv")

(**Position where the black king is able to castle and also black has a
   checkmate in one move.*)
let board3 = Board.make_board2 "4k2r/8/8/8/8/8/3PPP2/4K3 b k - 1 1"

let board3_moves = csv_to_move_array (Csv.load "../data/board3moves.csv")

(**Position where white pawn is able to capture a black pawn and white king can
   move all directions*)
let board4 = Board.make_board2 "8/5k2/8/3p4/4P3/8/3K4/8 w - - 2 2"

let board4_moves = csv_to_move_array (Csv.load "../data/board4moves.csv")

(**Position where white is able to take through en-passante*)
let board5 = Board.make_board2 "5k2/8/8/3pP3/8/8/8/5K2 w - d6 0 3"

let board5_moves = csv_to_move_array (Csv.load "../data/board5moves.csv")

(**Position where the white king can castle.*)
let board6 = Board.make_board2 "3b4/4k3/8/8/8/8/8/R3K3 w Q - 0 1"

let board6_moves = csv_to_move_array (Csv.load "..data/board6moves.csv")

(**Position where black has a bishop it can move around*)
let board7 = Board.make_board2 "3b4/4k3/8/8/8/8/8/R3K3 b Q - 0 1"

let board7_moves = csv_to_move_array (Csv.load "..data/board7moves.csv")

(**Position where black can move one of their rooks, both of their knights, and
   most of their pawns*)
let board8 =
  Board.make_board2 "rn4nr/1p2kppp/3p4/p3p3/P3P3/1PP2P2/6PP/RN2K1NR b KQ - 0 11"

let board8_moves = csv_to_move_array (Csv.load "..data/board8moves.csv")

(**A position where the black king can be taken in one turn and has one queen
   and one pawn left. White turn*)
let board9 = Board.make_board2 "2k4q/8/p7/8/8/8/8/2R1K3 w - - 0 1"

let board9_moves = csv_to_move_array (Csv.load "..data/board9moves.csv")

(**A position where the white king can be taken in one turn and only has a
   bishop left. Black has a rook and a pawn they can promote. Black turn*)

let board10 = Board.make_board2 "2k1r3/8/8/8/8/8/p7/2B1K3 b - - 0 1"
let board10_moves = csv_to_move_array (Csv.load "..data/board10moves.csv")

(*=======================TESTS ON VALID MOVE GENERATION====================*)

(*Below is a collection of testing moves for specific piece types and specific
  cases of movement*)
let castling_testing =
  "test suite for castling"
  >::: [
         make_move_exists "testing for right-side castling for black on board 4"
           board4
           ((4, 7), (7, 7), None);
       ]

let pawn_movement_testing =
  "test suite for valid pawn movements"
  >::: [
         make_move_exists
           "testing for pawn taking another pawn the normal way board 4" board4
           ((4, 3), (3, 4), None);
         make_move_exists
           "testing for double move for one of the pawns in board1" board1
           ((0, 1), (0, 3), None);
         make_move_exists
           "testing for another double move for one of the pawns in board1"
           board1
           ((0, 4), (4, 3), None);
         make_move_exists
           "testing for double move for one of the black pawns in board2" board2
           ((0, 6), (0, 4), None);
         make_move_exists
           "testing for another double move for one of the black pawns in \
            board2"
           board2
           ((4, 6), (4, 4), None);
         make_move_exists
           "testing for a promotion of the pawn to a queen in board 10" board10
           ((0, 1), (0, 0), Some Board.queen);
         make_move_exists
           "testing for a promotion of the pawn to a knight in board 10" board10
           ((0, 1), (0, 0), Some Board.knight);
         make_move_exists
           "testing for a promotion of the pawn to a queen in board 10" board10
           ((0, 1), (0, 0), Some Board.rook);
       ]

let rook_movement_testing =
  "test suite for valid rook movements"
  >::: [
         make_move_exists "testing for one of the basic rook moves in board 4"
           board4
           ((4, 4), (3, 5), None);
         make_move_exists "testing for one of the basic rook moves in board 6"
           board6
           ((0, 0), (1, 1), None);
       ]

(**A set of tests that makes sure that knights can jump over other pieces*)
let knight_jump_tests =
  "test suite for knights jumping over other pieces" >::: []

(**A set of tests to check that non-knight pieces are not able to phase through
   other pieces*)
let other_phase_tests =
  "test suite that ensures that other pieces cannot phase through other pieces"
  >::: [
         make_invalid_move
           "make sure rook phasing through pawns in board1 is not valid" board1
           ((0, 0), (0, 3), None);
         make_invalid_move
           "make sure bishop phasing through pawns in board1 is not valid"
           board1
           ((5, 0), (2, 3), None);
         make_invalid_move
           "make sure queen phasing through pawns diagonally is not valid"
           board1
           ((3, 0), (3, 5), None);
         make_invalid_move
           "make sure queen phasing through pawns horizontally is not valid"
           board1
           ((3, 0), (1, 2), None);
       ]

let take_same_color_tests =
  "test suite that ensures that pieces of the same color cannot take each other"
  >::: [
         make_invalid_move
           "make sure rook eating a white pawn in board1 is not considered one \
            of the valid moves"
           board1
           ((0, 0), (0, 1), None);
         make_invalid_move
           "make sure bishop eating a white pawn in board1 is not considered \
            one of the valid moves"
           board1
           ((5, 0), (4, 1), None);
         make_invalid_move
           "make sure queen eating white pawn is not considered valid board1"
           board1
           ((3, 0), (3, 1), None);
         make_invalid_move "make sure rook cannot eat knight next to him board1"
           board1
           ((0, 0), (1, 0), None);
       ]

let invalid_promotions =
  "test suite that ensures promotion logic isn't invalid"
  >::: [
         make_invalid_move
           "testing for pawn not being able to promote to queen after double \
            step board1"
           board1
           ((0, 1), (0, 3), Some Board.queen);
         make_invalid_move
           "testing for pawn not being able to promote to rook after single \
            step board1"
           board1
           ((0, 1), (0, 2), Some Board.rook);
         make_invalid_move
           "testing for pawn not being able to promote to queen after double \
            step board1"
           board1
           ((4, 1), (4, 3), Some Board.bishop);
         make_invalid_move
           "testing for pawn not being able to promote to rook after single \
            step board1"
           board1
           ((4, 1), (4, 2), Some Board.knight);
       ]

let invalid_movement_patters =
  "test suite that ensures that pieces cannot move in invalid movement patterns"
  >::: []

(**General legal move test generation, checking that all expected moves exist*)
let legal_move_testing =
  "test suite for legal move generarion"
  >::: [
         make_compare_moves "legal moves for starting position white" board1
           board1_moves;
         make_compare_moves "legal moves for starting position black" board2
           board2_moves;
         make_compare_moves "legal moves in board 3" board3 board3_moves;
         make_compare_moves "legal moves in board 4" board4 board4_moves;
         make_compare_moves "legal moves in board 5" board5 board5_moves;
         make_compare_moves "legal moves in board 6" board6 board6_moves;
         make_compare_moves "legal moves in board 7" board7 board7_moves;
       ]

(*==========================TESTS ON MAKE_MOVE FUNCTION IN BOARD==============*)
(*Ensures that the board mutates correctly and is able to properly recognize 
incorrect moves within the make_move function*)

let bloody_chess_tests = "test suite for bloody chess specific moves" >::: []
let _ = run_test_tt_main pawn_movement_testing
let _ = run_test_tt_main rook_movement_testing
let _ = run_test_tt_main castling_testing
let _ = run_test_tt_main legal_move_testing
