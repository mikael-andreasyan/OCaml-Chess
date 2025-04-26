open Base

type t = {
  board : Int64.t array array;
  mutable turn : int;
  mutable moves : int;
  mutable castlingRights : int;
  mutable enPassant : int;
}
(**AF: the board is represented by 6 two dimensional arrays of 64 bit ints from
   Jane Street's base library. The first index associates itself with a piece
   type such as pawn and the second index associates itself with a color type.
   Lastly, each index in the 64 bit is one square on the chess board.

   Ex) board.(0).(1) contains the posistions for all black pawns. *)

(**Somce constants for convience*)
let sliding_compass = [| 1; 8; 9; 7 |]

let knight_compass = [| 6; 15; 17; 10 |]

let bottomRow =
  Int64.of_int64 0b100000001000000010000000100000001000000010000000100000001L

let topRow =
  Int64.of_int64
    0b1000000010000000100000001000000010000000100000001000000010000000L

let leftCol = Int64.of_int64 0b1111111L

let rightCol =
  Int64.of_int64
    0b1111111000000000000000000000000000000000000000000000000000000000L

let castle =
  [|
    [|
      Int64.of_int64 0b1000000010000000000000000000000000000000000000000L;
      Int64.of_int64 0b1000000010000000100000000L;
    |];
    [|
      Int64.of_int64 0b10000000100000000000000000000000000000000000000000000000L;
      Int64.of_int64 0b110000000100000001000000000000000L;
    |];
  |]

let pawn = 0
let knight = 1
let bishop = 2
let rook = 3
let queen = 4
let king = 5
let current_turn board = board.turn
let total_moves board = board.moves
let make_board fen color moves = failwith "incomplete"

(**Outputs the tuple of the chess posistion (rank, file)*)
let bit_to_tuple bit =
  let squareIndex = Int64.(of_int (ceil_log2 bit)) in
  ( Int64.(to_int_exn (shift_right squareIndex 3)),
    Int64.(to_int_exn (bit_and squareIndex (of_int 7))) )

let check_spot board bit color =
  let present = ref false in
  for piece = pawn to king do
    if Int64.(equal (bit_and board.board.(piece).(color) bit) Int64.zero) then
      ()
    else present := true
  done;
  !present

let legal_moves board =
  let ans = Queue.create () in
  for piece = pawn to king do
    (*Getting bit board for chosen piece*)
    let pieceBitBoard = ref board.board.(piece).(board.turn) in
    (*Special bitshifting operations for "sliding pieces" such as the bishop,
      queen, and rook.*)
    if piece >= bishop && piece <= queen then
      (*Selecting start and finish indices for the compass array*)
      let start = if piece = rook || piece = queen then 0 else 2 in
      let finish = if piece = bishop || piece = queen then 3 else 1 in
      for index = start to finish do
        (*Obtaining the least significant bit of the bit board.*)
        let lsb = Int64.(bit_and !pieceBitBoard (neg !pieceBitBoard)) in
        (*Obtain bit shift value from compass*)
        let shiftBit = sliding_compass.(index) in
        (*We have to move all pieces on the bit board so this is the requirement
          in the while loop.*)
        while Int64.equal !pieceBitBoard Int64.zero do
          (*Stop condition for when we hit the border or we hit a friendly piece
            or we capture an enemy piece.*)
          let stop = ref false in
          let prev = ref lsb in
          while not !stop do
            (*Getting new move of selected piece*)
            let move = Int64.shift_left !prev shiftBit in
            (*Getting border int.*)
            let borderBit =
              if shiftBit = 9 || shiftBit = 1 then bottomRow
              else if shiftBit = 7 then topRow
              else Int64.zero
            in
            (*Checking if hitting border or hitting friendly piece then adding
              if not.*)
            if
              Int64.(equal (bit_and move borderBit) Int64.zero)
              || check_spot board move board.turn
            then stop := true
            else Queue.enqueue ans (bit_to_tuple lsb, bit_to_tuple move);
            (*If capturing should stop.*)
            if check_spot board move (board.turn lxor 1) then stop := true
            else ();
            (*Updating prev value.*)
            prev := move
          done;
          (*Same thing but we shift to the right.*)
          stop := false;
          while not !stop do
            let move = Int64.shift_right_logical !prev shiftBit in
            let borderBit =
              if shiftBit = 9 || shiftBit = 1 then topRow
              else if shiftBit = 7 then bottomRow
              else Int64.zero
            in
            if
              Int64.(equal (bit_and move borderBit) Int64.zero)
              || check_spot board move board.turn
            then stop := true
            else Queue.enqueue ans (bit_to_tuple lsb, bit_to_tuple move);
            if check_spot board move (board.turn lxor 1) then stop := true
            else ();
            prev := move
          done;
          (*Updating pieceBitBoard to remove the previous lsb.*)
          pieceBitBoard :=
            Int64.(bit_and !pieceBitBoard (!pieceBitBoard - Int64.one))
        done
      done
    else if piece = pawn then
      (*File data for double pawn pushing*)
      let fileForDouble = if board.turn = 1 then 1 else 6 in
      (*Obtaining the least significant bit of the bit board.*)
      let lsb = Int64.(bit_and !pieceBitBoard (neg !pieceBitBoard)) in
      (*Getting tuple of the start posistion.*)
      let rank, file = bit_to_tuple lsb in
      while Int64.equal !pieceBitBoard Int64.zero do
        let move =
          if board.turn = 1 then Int64.shift_left lsb 1
          else Int64.shift_right_logical lsb 1
        in
        let borderBit = if board.turn = 1 then bottomRow else topRow in
        if
          Int64.(equal (bit_and move borderBit) Int64.zero)
          || check_spot board move board.turn
        then ()
        else Queue.enqueue ans ((rank, file), bit_to_tuple move);
        (*Double pawn pushing*)
        if file = fileForDouble then
          let move =
            if board.turn = 1 then Int64.shift_left lsb 2
            else Int64.shift_right_logical lsb 2
          in
          let borderBit = if board.turn = 1 then bottomRow else topRow in
          if
            Int64.(equal (bit_and move borderBit) Int64.zero)
            || check_spot board move board.turn
          then ()
          else Queue.enqueue ans ((rank, file), bit_to_tuple move)
        else ();
        (*Updating pieceBitBoard to remove the previous lsb.*)
        pieceBitBoard :=
          Int64.(bit_and !pieceBitBoard (!pieceBitBoard - Int64.one))
      done
      (*Piece must be a king in which case we can just apply the bit shifts.*)
    else if piece = king then (
      let lsb = Int64.(bit_and !pieceBitBoard (neg !pieceBitBoard)) in
      for index = 0 to 3 do
        let shiftBit = sliding_compass.(index) in
        let move = Int64.shift_left lsb shiftBit in
        let borderBit =
          if shiftBit = 9 || shiftBit = 1 then bottomRow
          else if shiftBit = 7 then topRow
          else Int64.zero
        in
        if
          Int64.(equal (bit_and move borderBit) Int64.zero)
          || check_spot board move board.turn
        then ()
        else Queue.enqueue ans (bit_to_tuple lsb, bit_to_tuple move)
      done;
      for index = 0 to 3 do
        let shiftBit = sliding_compass.(index) in
        let move = Int64.shift_right_logical lsb shiftBit in
        let borderBit =
          if shiftBit = 9 || shiftBit = 1 then topRow
          else if shiftBit = 7 then bottomRow
          else Int64.zero
        in
        if
          Int64.(equal (bit_and move borderBit) Int64.zero)
          || check_spot board move board.turn
        then ()
        else Queue.enqueue ans (bit_to_tuple lsb, bit_to_tuple move)
      done
      (*Piece must be a knight, we have special compass for this.*))
    else
      let lsb = Int64.(bit_and !pieceBitBoard (neg !pieceBitBoard)) in
      for index = 0 to 3 do
        let shiftBit = knight_compass.(index) in
        let move = Int64.shift_left lsb shiftBit in
        let borderBit =
          if shiftBit = 9 || shiftBit = 1 then bottomRow
          else if shiftBit = 7 then topRow
          else Int64.zero
        in
        if
          Int64.(equal (bit_and move borderBit) Int64.zero)
          || check_spot board move board.turn
        then ()
        else Queue.enqueue ans (bit_to_tuple lsb, bit_to_tuple move)
      done;
      for index = 0 to 3 do
        let shiftBit = knight_compass.(index) in
        let move = Int64.shift_right_logical lsb shiftBit in
        let borderBit =
          if shiftBit = 9 || shiftBit = 1 then topRow
          else if shiftBit = 7 then bottomRow
          else Int64.zero
        in
        if
          Int64.(equal (bit_and move borderBit) Int64.zero)
          || check_spot board move board.turn
        then ()
        else Queue.enqueue ans (bit_to_tuple lsb, bit_to_tuple move)
      done;
      pieceBitBoard :=
        Int64.(bit_and !pieceBitBoard (!pieceBitBoard - Int64.one))
  done;
  (*Castling*)
  ();
  (*En Passant*)
  ();
  ans
