open Base

type t = {
  board : int64 array array;
  mutable turn : int;
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
let file1 =
  Int64.of_int64 0b100000001000000010000000100000001000000010000000100000001L

let file8 =
  Int64.of_int64
    0b1000000010000000100000001000000010000000100000001000000010000000L

let rankA = Int64.of_int64 0b1111111L

let rankH =
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
let white = 0
let black = 1

let printer =
  [| [| "p"; "h"; "b"; "r"; "q"; "k" |]; [| "P"; "H"; "B"; "R"; "Q"; "K" |] |]

let bit_to_tuple bit =
  let squareIndex = Int64.ctz bit in
  (Int.shift_right_logical squareIndex 3, squareIndex land 7)

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

let make_board board turn moves castlingRights enPassant =
  { board; turn; castlingRights; enPassant }

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

let get_piece_bitBoard board pieceType color = board.board.(pieceType).(color)

(**[legal_moves_pawn board] is a list of all legal pawn moves.*)
let legal_moves_pawn board =
  let ans = Queue.create () in
  let pieceBitBoard = ref board.board.(knight).(board.turn) in
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
    let borderBit = if board.turn = 1 then file1 else file8 in
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
      let borderBit = if board.turn = 1 then file1 else file8 in
      if
        Int64.(equal (bit_and move borderBit) Int64.zero)
        || check_spot board move board.turn
      then ()
      else Queue.enqueue ans ((rank, file), bit_to_tuple move)
    else ();

    (*Pawn captures.*)

    (*Updating pieceBitBoard to remove the previous lsb.*)
    pieceBitBoard := Int64.(bit_and !pieceBitBoard (!pieceBitBoard - Int64.one))
  done;
  ans

(**[legal_moves_knight board] is a list of all legal knight moves.*)
let legal_moves_knight board =
  let ans = Queue.create () in
  let pieceBitBoard = ref board.board.(knight).(board.turn) in
  let lsb = Int64.(!pieceBitBoard land neg !pieceBitBoard) in
  while not (Int64.equal !pieceBitBoard Int64.zero) do
    for index = 0 to 3 do
      let shiftBit = knight_compass.(index) in
      let move = Int64.shift_left lsb shiftBit in
      let borderBit =
        if shiftBit = 9 || shiftBit = 1 then file1
        else if shiftBit = 7 then file8
        else Int64.zero
      in
      if
        Int64.(equal (move land borderBit) Int64.zero)
        || check_spot board move board.turn
      then ()
      else Queue.enqueue ans (bit_to_tuple lsb, bit_to_tuple move)
    done;
    for index = 0 to 3 do
      let shiftBit = knight_compass.(index) in
      let move = Int64.shift_right_logical lsb shiftBit in
      let borderBit =
        if shiftBit = 9 || shiftBit = 1 then file8
        else if shiftBit = 7 then file1
        else Int64.zero
      in
      if
        Int64.(equal (move land borderBit) Int64.zero)
        || check_spot board move board.turn
      then ()
      else Queue.enqueue ans (bit_to_tuple lsb, bit_to_tuple move)
    done;
    pieceBitBoard := Int64.(!pieceBitBoard land (!pieceBitBoard - Int64.one))
  done;
  ans

(**[legal_moves_bishop board] is a list of all legal bishop moves.*)
let legal_moves_bishop board =
  let open Int64 in
  let ans = Queue.create () in
  let turn = board.turn in

  (* Precompute friendly and enemy occupancies *)
  let me_occ =
    board.board.(king).(turn)
    lor board.board.(queen).(turn)
    lor board.board.(rook).(turn)
    lor board.board.(bishop).(turn)
    lor board.board.(pawn).(turn)
    lor board.board.(knight).(turn)
  in
  let opp_occ =
    let o = Stdlib.( - ) 1 turn in
    board.board.(king).(o)
    lor board.board.(queen).(o)
    lor board.board.(rook).(o)
    lor board.board.(bishop).(o)
    lor board.board.(pawn).(o)
    lor board.board.(knight).(o)
  in

  let pb = ref board.board.(bishop).(turn) in

  let sb2 = sliding_compass.(2) in
  let sb3 = sliding_compass.(3) in

  while !pb <> zero do
    let from_bb = !pb land neg !pb in
    let from_sq = bit_to_tuple from_bb in
    let rec slide2_left pos =
      let mv = shift_left pos sb2 in
      if mv = zero || mv land file1 <> zero || mv land me_occ <> zero then ()
      else begin
        Queue.enqueue ans (from_sq, bit_to_tuple mv);
        if mv land opp_occ <> zero then () else slide2_left mv
      end
    in
    let rec slide2_right pos =
      let mv = shift_right_logical pos sb2 in
      if mv = zero || mv land file8 <> zero || mv land me_occ <> zero then ()
      else begin
        Queue.enqueue ans (from_sq, bit_to_tuple mv);
        if mv land opp_occ <> zero then () else slide2_right mv
      end
    in
    let rec slide3_left pos =
      let mv = shift_left pos sb3 in
      if mv = zero || mv land file8 <> zero || mv land me_occ <> zero then ()
      else begin
        Queue.enqueue ans (from_sq, bit_to_tuple mv);
        if mv land opp_occ <> zero then () else slide3_left mv
      end
    in
    let rec slide3_right pos =
      let mv = shift_right_logical pos sb3 in
      if mv = zero || mv land file1 <> zero || mv land me_occ <> zero then ()
      else begin
        Queue.enqueue ans (from_sq, bit_to_tuple mv);
        if mv land opp_occ <> zero then () else slide3_right mv
      end
    in

    slide2_left from_bb;
    slide2_right from_bb;
    slide3_left from_bb;
    slide3_right from_bb;

    pb := !pb land (!pb - one)
  done;
  ans

(**[legal_moves_rook board] is a list of all legal rook moves.*)
let legal_moves_rook board =
  let open Int64 in
  let ans = Queue.create () in
  let turn = board.turn in
  let me_occ =
    board.board.(king).(turn)
    lor board.board.(queen).(turn)
    lor board.board.(rook).(turn)
    lor board.board.(bishop).(turn)
    lor board.board.(pawn).(turn)
    lor board.board.(knight).(turn)
  in
  let opp_occ =
    let o = Stdlib.( - ) 1 turn in
    board.board.(king).(o)
    lor board.board.(queen).(o)
    lor board.board.(rook).(o)
    lor board.board.(bishop).(o)
    lor board.board.(pawn).(o)
    lor board.board.(knight).(o)
  in

  let pb = ref board.board.(rook).(turn) in

  let sb0 = sliding_compass.(0) and sb1 = sliding_compass.(1) in

  let rt0 = if Stdlib.( = ) sb0 1 then file1 else zero
  and lt0 = if Stdlib.( = ) sb0 1 then file8 else zero
  and rt1 = if Stdlib.( = ) sb1 1 then file1 else zero
  and lt1 = if Stdlib.( = ) sb1 1 then file8 else zero in
  while !pb <> zero do
    let from_bb = !pb land neg !pb in
    let from_sq = bit_to_tuple from_bb in
    let rec east pos =
      let mv = shift_left pos sb0 in
      if mv = zero || mv land rt0 <> zero || mv land me_occ <> zero then ()
      else begin
        Queue.enqueue ans (from_sq, bit_to_tuple mv);
        if mv land opp_occ <> zero then () else east mv
      end
    in
    let rec west pos =
      let mv = shift_right_logical pos sb0 in
      if mv = zero || mv land lt0 <> zero || mv land me_occ <> zero then ()
      else begin
        Queue.enqueue ans (from_sq, bit_to_tuple mv);
        if mv land opp_occ <> zero then () else west mv
      end
    in
    let rec south pos =
      let mv = shift_left pos sb1 in
      if mv = zero || mv land rt1 <> zero || mv land me_occ <> zero then ()
      else begin
        Queue.enqueue ans (from_sq, bit_to_tuple mv);
        if mv land opp_occ <> zero then () else south mv
      end
    in
    let rec north pos =
      let mv = shift_right_logical pos sb1 in
      if mv = zero || mv land lt1 <> zero || mv land me_occ <> zero then ()
      else begin
        Queue.enqueue ans (from_sq, bit_to_tuple mv);
        if mv land opp_occ <> zero then () else north mv
      end
    in
    east from_bb;
    west from_bb;
    south from_bb;
    north from_bb;
    pb := !pb land (!pb - one)
  done;

  ans

(**[legal_moves_queen board] is a list of all legal queen moves.*)
let legal_moves_queen board =
  let module I = Int64 in
  let open I in
  let ans = Queue.create () in
  let turn = board.turn in
  let me_occ =
    board.board.(king).(turn)
    lor board.board.(queen).(turn)
    lor board.board.(rook).(turn)
    lor board.board.(bishop).(turn)
    lor board.board.(pawn).(turn)
    lor board.board.(knight).(turn)
  in
  let opp_occ =
    let o = Stdlib.( - ) 1 turn in
    board.board.(king).(o)
    lor board.board.(queen).(o)
    lor board.board.(rook).(o)
    lor board.board.(bishop).(o)
    lor board.board.(pawn).(o)
    lor board.board.(knight).(o)
  in
  let pb = ref board.board.(queen).(turn) in
  while !pb <> zero do
    let from_bb = !pb land neg !pb in
    let from_sq = bit_to_tuple from_bb in
    for index = 0 to 3 do
      let shift = sliding_compass.(index) in
      let border_left =
        if Stdlib.( = ) shift 9 || Stdlib.( = ) shift 1 then file1
        else if Stdlib.( = ) shift 7 then file8
        else zero
      in
      let border_right =
        if Stdlib.( = ) shift 9 || Stdlib.( = ) shift 1 then file8
        else if Stdlib.( = ) shift 7 then file1
        else zero
      in
      let rec slide_left pos =
        let mv = shift_left pos shift in
        if mv = zero || mv land border_left <> zero || mv land me_occ <> zero
        then ()
        else begin
          Queue.enqueue ans (from_sq, bit_to_tuple mv);
          if mv land opp_occ <> zero then () else slide_left mv
        end
      in
      let rec slide_right pos =
        let mv = shift_right_logical pos shift in
        if mv = zero || mv land border_right <> zero || mv land me_occ <> zero
        then ()
        else begin
          Queue.enqueue ans (from_sq, bit_to_tuple mv);
          if mv land opp_occ <> zero then () else slide_right mv
        end
      in
      slide_left from_bb;
      slide_right from_bb
    done;
    pb := !pb land (!pb - one)
  done;
  ans

(**[legal_moves_king board] is a list of all legal king moves.*)
let legal_moves_king board =
  let ans = Queue.create () in
  let union =
    [|
      Int64.( lor )
        board.board.(king).(white)
        (Int64.( lor )
           board.board.(queen).(white)
           (Int64.( lor )
              board.board.(rook).(white)
              (Int64.( lor )
                 board.board.(bishop).(white)
                 (Int64.( lor )
                    board.board.(pawn).(white)
                    board.board.(knight).(white)))));
      Int64.( lor )
        board.board.(king).(black)
        (Int64.( lor )
           board.board.(queen).(black)
           (Int64.( lor )
              board.board.(rook).(black)
              (Int64.( lor )
                 board.board.(bishop).(black)
                 (Int64.( lor )
                    board.board.(pawn).(black)
                    board.board.(knight).(black)))));
    |]
  in
  let pieceBitBoard = ref board.board.(queen).(board.turn) in
  let lsb = Int64.(bit_and !pieceBitBoard (neg !pieceBitBoard)) in
  for index = 0 to 3 do
    let shiftBit = sliding_compass.(index) in
    let move = Int64.shift_left lsb shiftBit in
    let borderBit =
      if shiftBit = 9 || shiftBit = 1 then file1
      else if shiftBit = 7 then file8
      else Int64.zero
    in
    if
      Int64.(equal move Int64.zero)
      || (not Int64.(equal (move land borderBit) Int64.zero))
      || not Int64.(equal (move land union.(board.turn)) Int64.zero)
    then ()
    else Queue.enqueue ans (bit_to_tuple lsb, bit_to_tuple move)
  done;
  for index = 0 to 3 do
    let shiftBit = sliding_compass.(index) in
    let move = Int64.shift_right_logical lsb shiftBit in
    let borderBit =
      if shiftBit = 9 || shiftBit = 1 then file8
      else if shiftBit = 7 then file1
      else Int64.zero
    in
    if
      Int64.(equal (bit_and move borderBit) Int64.zero)
      || check_spot board move board.turn
    then ()
    else Queue.enqueue ans (bit_to_tuple lsb, bit_to_tuple move)
  done;
  ans

let movesArray =
  [|
    legal_moves_pawn;
    legal_moves_knight;
    legal_moves_bishop;
    legal_moves_rook;
    legal_moves_queen;
    legal_moves_king;
  |]

let make_move board ((rank1, file1), ((rank2 : int), (file2 : int))) = true
let unmake_move board ((rank1, file1), ((rank2 : int), (file2 : int))) = true
let legal_moves board = failwith ""
let player_check board = true

let printerBoard board =
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

let printerMoveList movelist =
  Base.Queue.iter movelist ~f:(fun ((x1, y1), (x2, y2)) ->
      Stdlib.Printf.printf "From (%d, %d) to (%d, %d)\n" x1 y1 x2 y2)
