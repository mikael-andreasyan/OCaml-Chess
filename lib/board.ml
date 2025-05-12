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
  List.iteri rows ~f:(fun rank_rev row ->
      let file = ref 0 in
      String.iter row ~f:(fun c ->
          if Char.is_digit c then
            file := !file + (Char.to_int c - Char.to_int '0')
          else
            let pi = piece_index c in
            let ci = color_index c in
            let rank = 7 - rank_rev in
            let bit = tuple_to_bit (rank, !file) in
            board.(pi).(ci) <- Int64.bit_or board.(pi).(ci) bit;
            Int.incr file));
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

let player_check board =
  let open Int64 in
  let turn = board.turn in
  let opp = Stdlib.( - ) 1 turn in
  let king_bit = board.board.(king).(turn) in
  if king_bit = zero then false
  else
    let square = Int64.ctz king_bit in
    let rank = Stdlib.( / ) square 8 and file = Stdlib.( mod ) square 8 in

    let kbit = tuple_to_bit (rank, file) in

    let pawn_attackers =
      if Stdlib.( = ) turn white then
        bit_or
          (shift_left kbit 7 land bit_not file1)
          (shift_left kbit 9 land bit_not file8)
      else
        bit_or
          (shift_right_logical kbit 7 land bit_not file8)
          (shift_right_logical kbit 9 land bit_not file1)
    in
    if bit_and pawn_attackers board.board.(pawn).(opp) <> zero then true
    else
      let knight_moves = [| 6; 10; 15; 17 |] in
      let is_attacked_by_knight =
        Array.exists knight_moves ~f:(fun s ->
            bit_and (shift_left kbit s) board.board.(knight).(opp) <> zero
            || bit_and (shift_right_logical kbit s) board.board.(knight).(opp)
               <> zero)
      in
      if is_attacked_by_knight then true
      else
        let rec ray_attack dir get_piece =
          let rec loop b =
            let next =
              if Stdlib.( = ) dir 1 || Stdlib.( = ) dir 8 || Stdlib.( = ) dir 9
              then shift_left b dir
              else shift_right_logical b dir
            in
            if next = zero then false
            else if
              bit_and next
                (board.board.(pawn).(turn)
                lor board.board.(knight).(turn)
                lor board.board.(bishop).(turn)
                lor board.board.(rook).(turn)
                lor board.board.(queen).(turn)
                lor board.board.(king).(turn))
              <> zero
            then false
            else if bit_and next (get_piece board.board) <> zero then true
            else loop next
          in
          loop kbit
        in
        let bishop_dirs = [| 7; 9 |] and rook_dirs = [| 1; 8 |] in
        let attacked_by_bishop =
          Array.exists bishop_dirs ~f:(fun d ->
              ray_attack d (fun b -> b.(bishop).(opp) lor b.(queen).(opp)))
        in
        let attacked_by_rook =
          Array.exists rook_dirs ~f:(fun d ->
              ray_attack d (fun b -> b.(rook).(opp) lor b.(queen).(opp)))
        in
        if attacked_by_bishop || attacked_by_rook then true
        else
          let king_attack_mask =
            bit_or
              (bit_or (shift_left kbit 1) (shift_right_logical kbit 1))
              (bit_or (shift_left kbit 8) (shift_right_logical kbit 8))
          in
          bit_and king_attack_mask board.board.(king).(opp) <> zero

let legal_moves_pawn board (rank, file) list index : move array * int =
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
  (if pawnBit land board.board.(pawn).(board.turn) = zero then
     failwith "Not valid pawn move"
   else
     let newPos1 = shift_left pawnBit 1 in
     if newPos1 land (me_occ lor opp_occ) = zero then (
       if Stdlib.( = ) file 6 then (
         Base.Array.set ans !index
           ((rank, file), bit_to_tuple newPos1, Some knight);
         index := Stdlib.( + ) !index 1;
         Base.Array.set ans !index
           ((rank, file), bit_to_tuple newPos1, Some bishop);
         index := Stdlib.( + ) !index 1;
         Base.Array.set ans !index
           ((rank, file), bit_to_tuple newPos1, Some rook);
         index := Stdlib.( + ) !index 1;
         Base.Array.set ans !index
           ((rank, file), bit_to_tuple newPos1, Some queen);
         index := Stdlib.( + ) !index 1)
       else Base.Array.set ans !index ((rank, file), bit_to_tuple newPos1, None);
       index := Stdlib.( + ) !index 1)
     else ());
  let newPos2 = shift_left pawnBit 2 in
  if Stdlib.( = ) file 1 && newPos2 land (me_occ lor opp_occ) = zero then (
    Base.Array.set ans !index ((rank, file), bit_to_tuple newPos2, None);
    index := Stdlib.( + ) !index 1);
  if Stdlib.( = ) board.turn white then
    if
      Stdlib.( = ) file 4
      && shift_left pawnBit 8
         lor shift_right_logical pawnBit 8
         land board.enPassant
         = zero
    then ()
    else if shift_right_logical pawnBit 8 land board.enPassant <> zero then (
      Base.Array.set ans !index
        ((rank, file), bit_to_tuple (shift_right_logical pawnBit 7), None);
      index := Stdlib.( + ) !index 1;
      if shift_left pawnBit 8 land board.enPassant <> zero then (
        Base.Array.set ans !index
          ((rank, file), bit_to_tuple (shift_left pawnBit 9), None);
        index := Stdlib.( + ) !index 1))
    else ()
  else if Stdlib.( = ) board.turn black then
    if
      Stdlib.( = ) file 3
      && shift_left pawnBit 8
         land shift_right_logical pawnBit 8
         land board.enPassant
         = zero
    then ()
    else if shift_right_logical pawnBit 8 land board.enPassant <> zero then (
      Base.Array.set ans !index
        ( (rank, file),
          bit_to_tuple (shift_right_logical pawnBit 9 land board.enPassant),
          None );
      index := Stdlib.( + ) !index 1)
    else if shift_left pawnBit 8 land board.enPassant <> zero then (
      Base.Array.set ans !index
        ( (rank, file),
          bit_to_tuple (shift_left pawnBit 7 land board.enPassant),
          None );
      index := Stdlib.( + ) !index 1)
    else ()
  else ();
  (ans, !index)

let legal_moves_knight board (rank, file) list index : move array * int =
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
  (ans, !index)

let legal_moves_bishop board (rank, file) list index : move array * int =
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
    (ans, !index)

let legal_moves_rook board (rank, file) list index : move array * int =
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
    (ans, !index)

let legal_moves_queen board (rank, file) list index : move array * int =
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
  (ans, !index)

let legal_moves_king board (rank, file) list index : move array * int =
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
  (ans, !index)

let legal_moves board : move array =
  let moves = Array.create ~len:maxLegalMoves ((0, 0), (0, 0), None) in
  let index = ref 0 in
  for rank = 0 to 7 do
    for file = 0 to 7 do
      match get_piece board (rank, file) with
      | Some piece_code ->
          let color = if piece_code land 8 = 0 then white else black in
          if color = board.turn then
            let piece_type = (piece_code land 7) - 1 in
            let generator =
              match piece_type with
              | 0 -> legal_moves_pawn
              | 1 -> legal_moves_knight
              | 2 -> legal_moves_bishop
              | 3 -> legal_moves_rook
              | 4 -> legal_moves_queen
              | 5 -> legal_moves_king
              | _ -> fun _ _ l i -> (l, i)
            in
            let _, new_index = generator board (rank, file) moves !index in
            index := new_index
      | None -> ()
    done
  done;
  Array.sub moves ~pos:0 ~len:!index

let make_move board ((rank1, file1), (rank2, file2), promo_opt) =
  let open Int64 in
  let legal = legal_moves board in
  let move_is_legal =
    Array.exists legal ~f:(fun ((r1, f1), (r2, f2), p) ->
        Stdlib.( = ) r1 rank1 && Stdlib.( = ) f1 file1 && Stdlib.( = ) r2 rank2
        && Stdlib.( = ) f2 file2 && Stdlib.( = ) p promo_opt)
  in
  if not move_is_legal then false
  else
    let src_bit = tuple_to_bit (rank1, file1) in
    let dst_bit = tuple_to_bit (rank2, file2) in
    let turn = board.turn in
    let opp = Stdlib.( - ) 1 turn in
    let moved_piece = ref (-1) in
    for p = 0 to 5 do
      if bit_and board.board.(p).(turn) src_bit <> zero then (
        board.board.(p).(turn) <- bit_xor board.board.(p).(turn) src_bit;
        moved_piece := p)
    done;
    for p = 0 to 5 do
      if bit_and board.board.(p).(opp) dst_bit <> zero then
        board.board.(p).(opp) <- bit_xor board.board.(p).(opp) dst_bit
    done;
    let final_piece =
      match promo_opt with
      | Some p -> p
      | None -> !moved_piece
    in
    if Stdlib.( >= ) final_piece 0 then
      board.board.(final_piece).(turn) <-
        bit_or board.board.(final_piece).(turn) dst_bit;
    board.enPassant <-
      (if Stdlib.(!moved_piece = pawn && abs (rank1 - rank2) = 2) then
         tuple_to_bit (Stdlib.(rank1 + (rank2 / 2)), file1)
       else zero);
    Stack.push board.moveList ((rank1, file1), (rank2, file2), promo_opt);
    board.turn <- opp;
    true

let unmake_move board =
  let open Int64 in
  if Stack.is_empty board.moveList then ()
  else
    let (rank1, file1), (rank2, file2), promo_opt =
      Stack.pop_exn board.moveList
    in
    let src_bit = tuple_to_bit (rank1, file1) in
    let dst_bit = tuple_to_bit (rank2, file2) in
    let opp = board.turn in
    let turn = Stdlib.( - ) 1 opp in
    board.turn <- turn;

    for p = 0 to 5 do
      board.board.(p).(turn) <- bit_and board.board.(p).(turn) (lnot dst_bit)
    done;

    let piece_index =
      match promo_opt with
      | Some _ -> pawn
      | None ->
          let found = ref (-1) in
          for p = 0 to 5 do
            if bit_and board.board.(p).(turn) dst_bit <> zero then found := p
          done;
          !found
    in

    let source_piece =
      match promo_opt with
      | Some p -> p
      | None -> piece_index
    in

    if Stdlib.( >= ) source_piece 0 then
      board.board.(source_piece).(turn) <-
        bit_or board.board.(source_piece).(turn) src_bit;

    if Stdlib.( <> ) promo_opt None then
      board.board.(Option.value_exn promo_opt).(turn) <-
        bit_and board.board.(Option.value_exn promo_opt).(turn) (lnot dst_bit);

    board.enPassant <- zero

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
  Array.iter movelist ~f:(fun ((x1, y1), (x2, y2), promo_opt) ->
      match promo_opt with
      | Some p ->
          Stdlib.Printf.printf "From (%d, %d) to (%d, %d), promote to %d\n" x1
            y1 x2 y2 p
      | None -> Stdlib.Printf.printf "From (%d, %d) to (%d, %d)\n" x1 y1 x2 y2)
