open Chess
open OUnit2
open Csv

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

(**Position where white pawn is able to capture and white king can move all
   directions*)
let board4 = Board.make_board2 "8/5k2/8/3p4/4P3/8/3K4/8 w - - 2 2"

let board4_moves = csv_to_move_array (Csv.load "../data/board4moves.csv")

(**Position where white is able to take through en-passante*)
let board5 = Board.make_board2 "5k2/8/8/3pP3/8/8/8/5K2 w - d6 0 3"

let board5_moves = csv_to_move_array (Csv.load "../data/board5moves.csv")

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
  name >:: fun _ -> assert_equal true true

(*Below is a collection of testing moves for specific piece types and specific
  cases of movement*)
let castling_testing = "test suite for castling" >::: []

let pawn_movement_testing =
  "test suite for pawn movements"
  >::: [
         make_move_exists "testing for pawn en-passante move in board 5" board5
           ((4, 4), (3, 5), None);
       ]

(**General legal move test generation*)
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
       ]

(**Position where the white king is under check. They can only take the enemy
   queen*)
(*rn2kbnr/ppp1pppp/8/3p4/8/5P2/PPPPq1PP/RNB1K2R w KQkq - 0 7*)

(**Position where the white kind is under check. They can either block the check
   with their knight or move behind the knight*)
(* "1K5r/1N5r/8/8/8/8/8/5k2 w - - 0 1" *)

(**Position where the black king is under check. They can only move their knight
   2 ways to defend from the check*)

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

let _ = run_test_tt_main legal_move_testing
