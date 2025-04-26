open Base

type t = {
  board : Int64.t array array;
  mutable turn : int;
  mutable moves : int;
  mutable castlingRights : int;
  mutable enPassant : int64;
}
(**AF: the board is represented by 6 two dimensional arrays of 64 bit ints from
   Jane Street's base library. The first index associates itself with a piece
   type such as pawn and the second index associates itself with a color type.
   Lastly, each index in the 64 bit is one square on the chess board.

   Ex) board.(0).(1) contains the posistions for all black pawns. *)

(*Some constants for convience.*)
(*Compasses for bit shifts.*)
let sliding_compass = [| 1; 8; 9; 7 |]
let knight_compass = [| 6; 15; 17; 10 |]

(*Bit strings for the top row, bottom row, left column, and right column.*)
let bottomRow =
  Int64.of_int64 0b100000001000000010000000100000001000000010000000100000001L

let topRow =
  Int64.of_int64
    0b1000000010000000100000001000000010000000100000001000000010000000L

let leftCol = Int64.of_int64 0b1111111L

let rightCol =
  Int64.of_int64
    0b1111111000000000000000000000000000000000000000000000000000000000L

(*Bit strings for castling rights. *)
let castleFree =
  [|
    [|
      (*king*)
      Int64.of_int64 0b10000000100000000000000000000000000000000000000000L;
      (*queen*)
      Int64.of_int64 0b1000000010000000100000000L;
    |];
    [|
      (*king*)
      Int64.of_int64 0b10000000100000000000000000000000000000000000000000000000L;
      (*queen*)
      Int64.of_int64 0b10000000100000001000000000000000L;
    |];
  |]

let castleMove =
  [|
    [|
      (*king*)
      Int64.of_int64 0b0L;
      (*queen*)
      Int64.of_int64 0b0L;
    |];
    [|
      (*king*)
      Int64.of_int64 0b0L;
      (*queen*)
      Int64.of_int64 0b0L;
    |];
  |]

let castleKing =
  [|
    [|
      (*king*)
      Int64.of_int64 0b0L;
      (*queen*)
      Int64.of_int64 0b0L;
    |];
    [|
      (*king*)
      Int64.of_int64 0b0L;
      (*queen*)
      Int64.of_int64 0b0L;
    |];
  |]

let castleRook =
  [|
    [|
      (*king*)
      Int64.of_int64 0b0L;
      (*queen*)
      Int64.of_int64 0b0L;
    |];
    [|
      (*king*)
      Int64.of_int64 0b0L;
      (*queen*)
      Int64.of_int64 0b0L;
    |];
  |]

(*Indices for the piece and colortypes.*)
let pawn = 0
let knight = 1
let bishop = 2
let rook = 3
let queen = 4
let king = 5
let colorsArray = [| 8; 0 |]

let printer =
  [| [| "p"; "h"; "b"; "r"; "q"; "k" |]; [| "P"; "H"; "B"; "R"; "Q"; "K" |] |]

(**[bit_to_tuple bit] outputs the tuple of the chess posistion in the format
   (rank, file).*)
let bit_to_tuple bit =
  let squareIndex = Int64.(of_int (ceil_log2 bit)) in
  ( Int64.(to_int_exn (shift_right squareIndex 3)),
    Int64.(to_int_exn (bit_and squareIndex (of_int 7))) )

(**[tuple_to_bit (rank, file)] outputs the bit of the chess posistion with the
   given (rank, file).*)
let tuple_to_bit (rank, file) =
  let index = (8 * rank) + file in
  Int64.shift_left Int64.one index

(**[check_spot board bit color] checks if a piece of a certain color is on the
   given piece.*)
let check_spot board bit color =
  let present = ref false in
  try
    for piece = pawn to king do
      if Int64.(equal (bit_and board.board.(piece).(color) bit) Int64.zero) then
        ()
      else present := true
    done;
    !present
  with _ -> !present

(**[boardCopy board] copies the board representation into a new board. *)
let boardCopy board =
  [|
    [| board.(0).(0); board.(0).(1) |];
    [| board.(0).(0); board.(0).(1) |];
    [| board.(1).(0); board.(1).(1) |];
    [| board.(1).(0); board.(1).(1) |];
    [| board.(2).(0); board.(2).(1) |];
    [| board.(2).(0); board.(2).(1) |];
    [| board.(3).(0); board.(3).(1) |];
    [| board.(3).(0); board.(3).(1) |];
    [| board.(4).(0); board.(4).(1) |];
    [| board.(4).(0); board.(4).(1) |];
    [| board.(5).(0); board.(5).(1) |];
    [| board.(5).(0); board.(5).(1) |];
  |]

let current_turn board = board.turn
let total_moves board = board.moves

let make_board1 board turn moves castlingRights enPassant =
  { board; turn; moves; castlingRights; enPassant }

let make_board2 fen = failwith "incomplete"

let get_piece board (rank, file) =
  let bit = tuple_to_bit (rank, file) in
  let return = ref None in
  for piece = pawn to king do
    for color = 0 to 1 do
      if Int64.(equal (bit_and board.board.(piece).(color) bit) Int64.zero) then
        ()
      else return := Some (piece + 1 + colorsArray.(color))
    done
  done;
  !return

let legal_moves_pawn (board : t) : ((int * int) * (int * int)) list =
  failwith "fuck"

let legal_moves_knight board = failwith "fuck"
let legal_moves_bishop board = failwith "fuck"
let legal_moves_rook board = failwith "fuck"
let legal_moves_queen board = failwith "fuck"
let legal_moves_king board = failwith "fuck"

let movesArray =
  [|
    legal_moves_pawn;
    legal_moves_knight;
    legal_moves_bishop;
    legal_moves_rook;
    legal_moves_queen;
    legal_moves_king;
  |]

let make_move board ((rank1, file1), ((rank2 : int), (file2 : int))) =
  let piece = get_piece board (rank1, file1) in
  match piece with
  | None ->
      ( false,
        {
          board = boardCopy board.board;
          turn = board.turn;
          moves = board.moves;
          castlingRights = board.castlingRights;
          enPassant = board.enPassant;
        } )
  | Some integer ->
      let color = Int.shift_right (8 land integer) 3 in
      if color <> board.turn then
        ( false,
          {
            board = boardCopy board.board;
            turn = board.turn;
            moves = board.moves;
            castlingRights = board.castlingRights;
            enPassant = board.enPassant;
          } )
      else
        let pieceType = (7 land integer) - 1 in
        let func = movesArray.(pieceType) in
        let moveList = func board in
        if
          not
            (List.mem moveList
               ((rank1, file1), (rank2, file2))
               ~equal:(fun
                   ((rank1, file1), (rank2, file2))
                   ((rank3, file3), (rank4, file4))
                 ->
                 rank1 = rank3 && rank2 = rank4 && file1 = file3
                 && file2 = file4))
        then
          ( false,
            {
              board = boardCopy board.board;
              turn = board.turn;
              moves = board.moves;
              castlingRights = board.castlingRights;
              enPassant = board.enPassant;
            } )
        else
          let start = tuple_to_bit (rank1, file1) in
          let finish = tuple_to_bit (rank2, file2) in
          let bit = Int64.bit_and start finish in
          let boardClone = boardCopy board.board in
          if Int64.equal bit castleMove.(board.turn).(0) || pieceType = king
          then (
            boardClone.(king).(board.turn) <-
              Int64.bit_xor
                boardClone.(king).(board.turn)
                castleKing.(board.turn).(0);
            boardClone.(rook).(board.turn) <-
              Int64.bit_xor
                boardClone.(rook).(board.turn)
                castleRook.(board.turn).(0);
            ( true,
              {
                board = boardClone;
                turn = board.turn lxor 1;
                moves = board.moves + 1;
                castlingRights = board.castlingRights;
                enPassant = board.enPassant;
              } ))
          else if
            Int64.equal bit castleMove.(board.turn).(1) || pieceType = king
          then (
            boardClone.(king).(board.turn) <-
              Int64.bit_xor
                boardClone.(king).(board.turn)
                castleKing.(board.turn).(1);
            boardClone.(rook).(board.turn) <-
              Int64.bit_xor
                boardClone.(rook).(board.turn)
                castleRook.(board.turn).(1);
            ( true,
              {
                board = boardClone;
                turn = board.turn lxor 1;
                moves = board.moves + 1;
                castlingRights = board.castlingRights;
                enPassant = board.enPassant;
              } ))
          else if pieceType = pawn then (
            let remove = board.enPassant in
            boardClone.(pieceType).(board.turn) <-
              Int64.bit_xor bit board.board.(pieceType).(board.turn);
            for piece = pawn to queen do
              boardClone.(piece).(board.turn lxor 1) <-
                Int64.bit_xor start boardClone.(piece).(board.turn lxor 1)
            done;
            boardClone.(pawn).(board.turn lxor 1) <-
              Int64.bit_xor remove boardClone.(pawn).(board.turn lxor 1);
            ( true,
              {
                board = boardClone;
                turn = board.turn lxor 1;
                moves = board.moves + 1;
                castlingRights = board.castlingRights;
                enPassant = board.enPassant;
              } ))
          else (
            boardClone.(pieceType).(board.turn) <-
              Int64.bit_xor bit board.board.(pieceType).(board.turn);
            for piece = pawn to queen do
              boardClone.(piece).(board.turn lxor 1) <-
                Int64.bit_xor start boardClone.(piece).(board.turn lxor 1)
            done;
            ( true,
              {
                board = boardClone;
                turn = board.turn lxor 1;
                moves = board.moves + 1;
                castlingRights = board.castlingRights;
                enPassant = board.enPassant;
              } ))

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

        (*Pawn captures.*)

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

let printer board =
  let boardString = ref "" in
  for rank = 0 to 7 do
    for file = 0 to 7 do
      let piece = get_piece board (rank, file) in
      match piece with
      | None -> boardString := !boardString ^ "#"
      | Some integer ->
          let color = Int.shift_right (8 land integer) 3 in
          let piece = (7 land integer) - 1 in
          boardString := !boardString ^ printer.(piece).(color)
    done;
    boardString := !boardString ^ "\n"
  done;
  !boardString
