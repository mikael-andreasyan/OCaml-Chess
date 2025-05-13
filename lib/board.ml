open Base

type move = (int * int) * (int * int) * int option

type t = {
  board : int64 array array;
  mutable turn : int;
  mutable castlingRights : int;
  mutable enPassant : int64;
  mutable moveList : move Stack.t;
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
let maxLegalMoves = 218

let printer =
  [| [| "p"; "h"; "b"; "r"; "q"; "k" |]; [| "P"; "H"; "B"; "R"; "Q"; "K" |] |]

let bit_to_tuple bit =
  let squareIndex = Int64.ctz bit in
  (Int.shift_right_logical squareIndex 3, squareIndex land 7)

let tuple_to_bit (rank, file) =
  let index = (8 * rank) + file in
  Int64.shift_left Int64.one index

let current_turn board = board.turn

let make_board1 board turn castlingRights enPassant =
  { board; turn; castlingRights; enPassant; moveList = Stack.create () }

let empty_board () : Int64.t array array =
  Array.init 6 ~f:(fun _ -> Array.create ~len:2 Int64.zero)

let piece_index = function
  | 'p' | 'P' -> 0
  | 'n' | 'N' -> 1
  | 'b' | 'B' -> 2
  | 'r' | 'R' -> 3
  | 'q' | 'Q' -> 4
  | 'k' | 'K' -> 5
  | _ -> -1

let color_index c = if Stdlib.(Char.uppercase_ascii c = c) then 0 else 1

let make_board2 fen : t =
  let board = empty_board () in
  let parts = String.split fen ~on:' ' in
  let rows = String.split (List.nth_exn parts 0) ~on:'/' in
  List.iteri rows ~f:(fun rank row ->
      let file = ref 7 in
      String.iter row ~f:(fun c ->
          if Char.is_digit c then
            file := !file - (Char.to_int c - Char.to_int '0')
          else
            let pi = piece_index c in
            let ci = color_index c in
            let bit = tuple_to_bit (7 - !file, 7 - rank) in
            board.(pi).(ci) <- Int64.( lor ) board.(pi).(ci) bit;
            Stdlib.decr file));
  let turn = if String.equal (List.nth_exn parts 1) "w" then 0 else 1 in
  let castlingRights =
    let cstr = List.nth_exn parts 2 in
    let bit = ref 0 in
    if String.contains cstr 'k' then bit := !bit lor 0b1000;
    if String.contains cstr 'q' then bit := !bit lor 0b0100;
    if String.contains cstr 'K' then bit := !bit lor 0b0010;
    if String.contains cstr 'Q' then bit := !bit lor 0b0001;
    !bit
  in
  let enPassant =
    match List.nth_exn parts 3 with
    | "-" -> Int64.zero
    | ep ->
        let file = Char.to_int ep.[0] - Char.to_int 'a' in
        let rank = Char.to_int ep.[1] - Char.to_int '1' in
        tuple_to_bit (rank, file)
  in
  { board; turn; castlingRights; enPassant; moveList = Stack.create () }

let get_piece board (rank, file) =
  let bit = tuple_to_bit (rank, file) in
  let return = ref None in
  try
    for piece = pawn to king do
      for color = 0 to 1 do
        if Int64.(equal (bit_and board.board.(piece).(color) bit) Int64.zero)
        then ()
        else (
          return := Some (piece + 1 + colorsArray.(color));
          failwith "fast exit")
      done
    done;
    !return
  with _ -> !return

let get_piece_bitBoard board pieceType color = board.board.(pieceType).(color)

let legal_moves_pawn board (rank, file) list index =
  let open Int64 in
  let ans = list in
  let index = ref index in
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
  let pawnBit = tuple_to_bit (rank, file) in
  if pawnBit land board.board.(pawn).(board.turn) = zero then
    failwith "Not valid pawn move"
  else ();
  (let newPos1 = shift_left pawnBit 1 in
   if newPos1 land (me_occ lor opp_occ) = zero then
     if Stdlib.( = ) file 6 then (
       Base.Array.set ans !index
         ((rank, file), bit_to_tuple newPos1, Some knight);
       index := Stdlib.( + ) !index 1;
       Base.Array.set ans !index
         ((rank, file), bit_to_tuple newPos1, Some bishop);
       index := Stdlib.( + ) !index 1;
       Base.Array.set ans !index ((rank, file), bit_to_tuple newPos1, Some rook);
       index := Stdlib.( + ) !index 1;
       Base.Array.set ans !index ((rank, file), bit_to_tuple newPos1, Some queen);
       index := Stdlib.( + ) !index 1)
     else (
       Base.Array.set ans !index ((rank, file), bit_to_tuple newPos1, None);
       index := Stdlib.( + ) !index 1)
   else ());
  (let newPos2 = shift_left pawnBit 2 in
   if Stdlib.( = ) file 1 && newPos2 land (me_occ lor opp_occ) = zero then (
     Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, None);
     index := Stdlib.( + ) !index 1));
  if Stdlib.( = ) board.turn white then (
    if shift_right_logical pawnBit 8 land board.enPassant <> zero then (
      Base.Array.set ans !index
        ((rank, file), bit_to_tuple (shift_right_logical pawnBit 7), None);
      index := Stdlib.( + ) !index 1)
    else ();
    if shift_left pawnBit 8 land board.enPassant <> zero then (
      Base.Array.set ans !index
        ((rank, file), bit_to_tuple (shift_left pawnBit 9), None);
      index := Stdlib.( + ) !index 1)
    else ())
  else if Stdlib.( = ) board.turn black then (
    if shift_right_logical pawnBit 8 land board.enPassant <> zero then (
      Base.Array.set ans !index
        ((rank, file), bit_to_tuple (shift_left pawnBit 7), None);
      index := Stdlib.( + ) !index 1)
    else ();
    if shift_left pawnBit 8 land board.enPassant <> zero then (
      Base.Array.set ans !index
        ((rank, file), bit_to_tuple (shift_right_logical pawnBit 9), None);
      index := Stdlib.( + ) !index 1)
    else ())
  else ();
  if Stdlib.( = ) board.turn white then
    let newPos2 = shift_right_logical pawnBit 7 in
    let newPos3 = shift_left pawnBit 9 in
    if Stdlib.( <> ) file 6 then
      if newPos2 land opp_occ <> zero then (
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, None);
        index := Stdlib.( + ) !index 1)
      else if newPos3 land opp_occ <> zero then (
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos3, None);
        index := Stdlib.( + ) !index 1)
      else ()
    else if newPos2 land opp_occ <> zero then (
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, Some knight);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, Some bishop);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, Some rook);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, Some queen);
      index := Stdlib.( + ) !index 1)
    else if newPos3 land opp_occ <> zero then (
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos3, Some knight);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos3, Some bishop);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos3, Some rook);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos3, Some queen);
      index := Stdlib.( + ) !index 1)
    else ()
  else if Stdlib.( = ) board.turn black then (
    let newPos2 = shift_left pawnBit 7 in
    let newPos3 = shift_right_logical pawnBit 9 in
    if Stdlib.( <> ) file 6 then
      if newPos2 land opp_occ <> zero then (
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, None);
        index := Stdlib.( + ) !index 1)
      else if newPos3 land opp_occ <> zero then (
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos3, None);
        index := Stdlib.( + ) !index 1)
      else ()
    else if newPos2 land opp_occ <> zero then (
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, Some knight);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, Some bishop);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, Some rook);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, Some queen);
      index := Stdlib.( + ) !index 1)
    else if newPos3 land opp_occ <> zero then (
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos3, Some knight);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos3, Some bishop);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos3, Some rook);
      index := Stdlib.( + ) !index 1;
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos3, Some queen);
      index := Stdlib.( + ) !index 1))
  else ();
  !index

let legal_moves_knight board (rank, file) list index =
  let open Int64 in
  let ans = list in
  let index = ref index in
  let turn = board.turn in
  let me_occ =
    board.board.(king).(turn)
    lor board.board.(queen).(turn)
    lor board.board.(rook).(turn)
    lor board.board.(bishop).(turn)
    lor board.board.(pawn).(turn)
    lor board.board.(knight).(turn)
  in
  let knightBit = tuple_to_bit (rank, file) in
  if knightBit land board.board.(knight).(board.turn) = zero then
    failwith "Not valid knight move"
  else
    for shift_index = 0 to 3 do
      let shift = knight_compass.(shift_index) in
      let newPos = shift_left knightBit shift in
      let border =
        if Stdlib.( = ) shift 6 then file8 lor shift_right_logical file8 1
        else if Stdlib.( = ) shift 15 then file8
        else if Stdlib.( = ) shift 17 then file1
        else file1 lor shift_left file1 1
      in
      if
        newPos land me_occ = zero && newPos land border = zero && newPos <> zero
      then (
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
        index := Stdlib.( + ) !index 1)
      else ()
    done;
  for shift_index = 0 to 3 do
    let shift = knight_compass.(shift_index) in
    let newPos = shift_right_logical knightBit shift in
    let border =
      if Stdlib.( = ) shift 6 then file1 lor shift_left file1 1
      else if Stdlib.( = ) shift 15 then file1
      else if Stdlib.( = ) shift 17 then file8
      else file8 lor shift_right_logical file8 1
    in
    if newPos land me_occ = zero && newPos land border = zero && newPos <> zero
    then (
      Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
      index := Stdlib.( + ) !index 1)
    else ()
  done;
  !index

let legal_moves_bishop board (rank, file) list index =
  let open Int64 in
  let ans = list in
  let index = ref index in
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

  let bishopBit = tuple_to_bit (rank, file) in
  if bishopBit land board.board.(bishop).(board.turn) = zero then
    failwith "Not a valid bishop move"
  else
    let shift1 = sliding_compass.(2) in
    let shift2 = sliding_compass.(3) in

    let rec slide2_left pos =
      let newPos = shift_left pos shift1 in
      if
        newPos = zero || newPos land file1 <> zero || newPos land me_occ <> zero
      then ()
      else begin
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
        index := Stdlib.( + ) !index 1;
        if newPos land opp_occ <> zero then () else slide2_left newPos
      end
    in
    let rec slide2_right pos =
      let newPos = shift_right_logical pos shift1 in
      if
        newPos = zero || newPos land file8 <> zero || newPos land me_occ <> zero
      then ()
      else begin
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
        index := Stdlib.( + ) !index 1;
        if newPos land opp_occ <> zero then () else slide2_right newPos
      end
    in
    let rec slide3_left pos =
      let newPos = shift_left pos shift2 in
      if
        newPos = zero || newPos land file8 <> zero || newPos land me_occ <> zero
      then ()
      else begin
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
        index := Stdlib.( + ) !index 1;
        if newPos land opp_occ <> zero then () else slide3_left newPos
      end
    in
    let rec slide3_right pos =
      let newPos = shift_right_logical pos shift2 in
      if
        newPos = zero || newPos land file1 <> zero || newPos land me_occ <> zero
      then ()
      else begin
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
        index := Stdlib.( + ) !index 1;
        if newPos land opp_occ <> zero then () else slide3_right newPos
      end
    in
    slide2_left bishopBit;
    slide2_right bishopBit;
    slide3_left bishopBit;
    slide3_right bishopBit;
    !index

let legal_moves_rook board (rank, file) list index =
  let open Int64 in
  let ans = list in
  let index = ref index in
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
  let rookBit = tuple_to_bit (rank, file) in
  if rookBit land board.board.(rook).(board.turn) = zero then
    failwith "Not a valid rook move"
  else
    let shift1 = sliding_compass.(0) in
    let shift2 = sliding_compass.(1) in
    let border1 = if Stdlib.( = ) shift1 1 then file1 else zero
    and border2 = if Stdlib.( = ) shift1 1 then file8 else zero
    and border3 = if Stdlib.( = ) shift2 1 then file1 else zero
    and border4 = if Stdlib.( = ) shift2 1 then file8 else zero in
    let rec east pos =
      let newPos = shift_left pos shift1 in
      if
        newPos = zero
        || newPos land border1 <> zero
        || newPos land me_occ <> zero
      then ()
      else begin
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
        index := Stdlib.( + ) !index 1;
        if newPos land opp_occ <> zero then () else east newPos
      end
    in
    let rec west pos =
      let newPos = shift_right_logical pos shift1 in
      if
        newPos = zero
        || newPos land border2 <> zero
        || newPos land me_occ <> zero
      then ()
      else begin
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
        index := Stdlib.( + ) !index 1;
        if newPos land opp_occ <> zero then () else west newPos
      end
    in
    let rec south pos =
      let newPos = shift_left pos shift2 in
      if
        newPos = zero
        || newPos land border3 <> zero
        || newPos land me_occ <> zero
      then ()
      else begin
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
        index := Stdlib.( + ) !index 1;
        if newPos land opp_occ <> zero then () else south newPos
      end
    in
    let rec north pos =
      let newPos = shift_right_logical pos shift2 in
      if
        newPos = zero
        || newPos land border4 <> zero
        || newPos land me_occ <> zero
      then ()
      else begin
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
        index := Stdlib.( + ) !index 1;
        if newPos land opp_occ <> zero then () else north newPos
      end
    in
    east rookBit;
    west rookBit;
    south rookBit;
    north rookBit;
    !index

let legal_moves_queen board (rank, file) list index =
  let module I = Int64 in
  let open I in
  let ans = list in
  let index = ref index in
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
  let queenBit = tuple_to_bit (rank, file) in
  if queenBit land board.board.(queen).(board.turn) = zero then
    failwith "Not a valid queen move"
  else
    for shift_index = 0 to 3 do
      let shift = sliding_compass.(shift_index) in
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
        let newPos = shift_left pos shift in
        if
          newPos = zero
          || newPos land border_left <> zero
          || newPos land me_occ <> zero
        then ()
        else begin
          Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
          index := Stdlib.( + ) !index 1;
          if newPos land opp_occ <> zero then () else slide_left newPos
        end
      in
      let rec slide_right pos =
        let newPos = shift_right_logical pos shift in
        if
          newPos = zero
          || newPos land border_right <> zero
          || newPos land me_occ <> zero
        then ()
        else begin
          Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
          index := Stdlib.( + ) !index 1;
          if newPos land opp_occ <> zero then () else slide_right newPos
        end
      in
      slide_left queenBit;
      slide_right queenBit
    done;
  !index

let legal_moves_king board (rank, file) list index =
  let open Int64 in
  let ans = list in
  let index = ref index in
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
  let kingBit = tuple_to_bit (rank, file) in
  if kingBit land board.board.(king).(board.turn) = zero then
    failwith "Not a valid king move"
  else (
    for shift_index = 0 to 3 do
      let shift = sliding_compass.(shift_index) in
      let newPos = shift_left kingBit shift in
      let border =
        if Stdlib.( = ) shift 9 || Stdlib.( = ) shift 1 then file1
        else if Stdlib.( = ) shift 7 then file8
        else zero
      in
      if
        newPos land me_occ = zero && newPos land border = zero && newPos <> zero
      then (
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
        index := Stdlib.( + ) !index 1)
      else ()
    done;
    for shift_index = 0 to 3 do
      let shift = sliding_compass.(shift_index) in
      let newPos = shift_right_logical kingBit shift in
      let border =
        if Stdlib.( = ) shift 9 || Stdlib.( = ) shift 1 then file8
        else if Stdlib.( = ) shift 7 then file1
        else zero
      in
      if
        newPos land me_occ = zero && newPos land border = zero && newPos <> zero
      then (
        Base.Array.set ans !index ((rank, file), bit_to_tuple newPos, None);
        index := Stdlib.( + ) !index 1)
      else ()
    done;
    if Stdlib.( = ) board.turn white then
      if
        castleFree.(white).(0) land (me_occ lor opp_occ) = zero
        && Stdlib.(board.castlingRights land 1 <> 0)
      then (
        Base.Array.set ans !index ((rank, file), (0, 7), None);
        index := Stdlib.( + ) !index 1;
        if
          castleFree.(white).(1) land (me_occ lor opp_occ) = zero
          && Stdlib.(board.castlingRights land 2 <> 0)
        then (
          Base.Array.set ans !index ((rank, file), (0, 0), None);
          index := Stdlib.( + ) !index 1)
        else ())
      else ()
    else if Stdlib.( = ) board.turn black then
      if
        castleFree.(black).(0) land (me_occ lor opp_occ) = zero
        && Stdlib.(board.castlingRights land 4 <> 0)
      then (
        Base.Array.set ans !index ((rank, file), (7, 7), None);
        index := Stdlib.( + ) !index 1;
        if
          castleFree.(black).(1) land (me_occ lor opp_occ) = zero
          && Stdlib.(board.castlingRights land 8 <> 0)
        then (
          Base.Array.set ans !index ((rank, file), (7, 0), None);
          index := Stdlib.( + ) !index 1)
        else ())
      else ());
  !index

let generators =
  [|
    legal_moves_pawn;
    legal_moves_knight;
    legal_moves_bishop;
    legal_moves_rook;
    legal_moves_queen;
    legal_moves_king;
  |]

let legal_moves board : move array =
  let open Int64 in
  let ans = Base.Array.create ~len:218 ((-1, -1), (-1, -1), None) in
  let index = ref 0 in
  for piece = pawn to king do
    let bitBoard = ref board.board.(piece).(board.turn) in
    while !bitBoard <> zero do
      let lsb = !bitBoard land neg !bitBoard in
      let tuple = bit_to_tuple lsb in
      index := generators.(piece) board tuple ans !index;
      bitBoard := !bitBoard land (!bitBoard - one)
    done
  done;
  ans

let castlingRights (rank, file) =
  if rank = 0 && file = 7 then 1
  else if rank = 0 && file = 0 then 2
  else if rank = 7 && file = 7 then 4
  else if rank = 7 && file = 0 then 8
  else 0

let castlingCancel = [| 0b1100; 0b11 |]

let updateBoard board ((rank1, file1), (rank2, file2), promo_opt) =
  let open Int64 in
  let start = tuple_to_bit (rank1, file1) in
  let finish = tuple_to_bit (rank2, file2) in
  for piece = pawn to king do
    if board.board.(piece).(board.turn) land start <> zero then
      if Stdlib.( = ) piece pawn then
        match promo_opt with
        | Some newPiece ->
            board.board.(piece).(board.turn) <-
              board.board.(piece).(board.turn) lxor start;
            for piece = pawn to king do
              if
                board.board.(piece).(Stdlib.( - ) 1 board.turn) land finish
                <> zero
              then
                board.board.(piece).(Stdlib.( - ) 1 board.turn) <-
                  board.board.(piece).(Stdlib.( - ) 1 board.turn) lxor finish
            done;
            board.board.(newPiece).(board.turn) <-
              board.board.(newPiece).(board.turn) land finish
        | None ->
            let opp_occ =
              let o = Stdlib.( - ) 1 board.turn in
              board.board.(king).(o)
              lor board.board.(queen).(o)
              lor board.board.(rook).(o)
              lor board.board.(bishop).(o)
              lor board.board.(pawn).(o)
              lor board.board.(knight).(o)
            in
            if opp_occ land finish = zero then (
              board.board.(piece).(board.turn) <-
                board.board.(piece).(board.turn) lxor (start lor finish);
              board.board.(piece).(Stdlib.( - ) 1 board.turn) <-
                board.board.(piece).(Stdlib.( - ) 1 board.turn)
                lxor board.enPassant)
            else (
              board.board.(piece).(board.turn) <-
                board.board.(piece).(board.turn) lxor (start lor finish);
              for piece = pawn to king do
                if
                  board.board.(piece).(Stdlib.( - ) 1 board.turn) land finish
                  <> zero
                then
                  board.board.(piece).(Stdlib.( - ) 1 board.turn) <-
                    board.board.(piece).(Stdlib.( - ) 1 board.turn) lxor finish
              done;
              if Stdlib.(file2 - file1 = 2) then board.enPassant <- finish
              else ())
      else if Stdlib.( = ) piece king then
        if board.board.(rook).(board.turn) land finish <> zero then ()
        else (
          board.board.(piece).(board.turn) <-
            board.board.(piece).(board.turn) lxor (start lor finish);
          for piece = pawn to king do
            if
              board.board.(piece).(Stdlib.( - ) 1 board.turn) land finish
              <> zero
            then
              board.board.(piece).(Stdlib.( - ) 1 board.turn) <-
                board.board.(piece).(Stdlib.( - ) 1 board.turn) lxor finish
          done;
          board.castlingRights <-
            Stdlib.( land ) board.castlingRights castlingCancel.(board.turn))
      else (
        board.board.(piece).(board.turn) <-
          board.board.(piece).(board.turn) lxor (start lor finish);
        for piece = pawn to king do
          if board.board.(piece).(Stdlib.( - ) 1 board.turn) land finish <> zero
          then
            board.board.(piece).(Stdlib.( - ) 1 board.turn) <-
              board.board.(piece).(Stdlib.( - ) 1 board.turn) lxor finish
        done;
        if Stdlib.( = ) piece rook then
          board.castlingRights <-
            Stdlib.( lxor ) board.castlingRights (castlingRights (rank1, file1))
        else ())
    else ()
  done;
  board.turn <- Stdlib.( lxor ) board.turn 1

let make_move board ((rank1, file1), (rank2, file2), promo_opt) =
  let legal_moves_list = legal_moves board in
  if
    Base.Array.mem legal_moves_list
      ((rank1, file1), (rank2, file2), promo_opt)
      ~equal:(fun
          ((rank1, file1), (rank2, file2), promo_opt1)
          ((rank3, file3), (rank4, file4), promo_opt2)
        ->
        match (promo_opt1, promo_opt2) with
        | Some int1, Some int2 ->
            rank1 = rank3 && rank2 = rank4 && file1 = file3 && file2 = file4
            && int1 = int2
        | None, None ->
            rank1 = rank3 && rank2 = rank4 && file1 = file3 && file2 = file4
        | _ -> false)
  then (
    updateBoard board ((rank1, file1), (rank2, file2), promo_opt);
    true)
  else false

let unmake_move board = failwith "fuck you"

let printerBoard board =
  let boardString = ref "" in
  for file = 7 downto 0 do
    for rank = 0 to 7 do
      let piece = get_piece board (rank, file) in
      match piece with
      | None -> boardString := !boardString ^ "#"
      | Some integer ->
          let color = Int.shift_right (8 land integer) 3 in
          let piece = (7 land integer) - 1 in
          boardString := !boardString ^ printer.(color).(piece)
    done;
    boardString := !boardString ^ "\n"
  done;
  Stdlib.print_string !boardString

let printerMoveList (movelist : move Base.Array.t) =
  let finalString = ref "" in
  Array.iter movelist ~f:(fun ((x1, y1), (x2, y2), promo_opt) ->
      match promo_opt with
      | Some p ->
          finalString :=
            !finalString
            ^ Printf.sprintf "From (%d, %d) to (%d, %d), promote to %d\n" x1 y1
                x2 y2 p
      | None ->
          finalString :=
            !finalString
            ^ Printf.sprintf "From (%d, %d) to (%d, %d)\n" x1 y1 x2 y2);
  !finalString
