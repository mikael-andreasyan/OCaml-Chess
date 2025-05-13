(* *Quick array for piece values let pieceValuesMid = [| 82; 337; 365; 477;
   1025; 0 |]

   let pieceValuesEnd = [| 94; 281; 297; 512; 936; 0 |] let gamephaseInc = [| 0;
   1; 1; 2; 4; 0 |] let depth = 20 let bishopPair = 48 let knightPair = 16 let
   rookPair = 24

   let pawnPSTMid = [| [| 0; 0; 0; 0; 0; 0; 0; 0 |]; [| 98; 134; 61; 95; 68;
   126; 34; -11 |]; [| -6; 7; 26; 31; 65; 56; 25; -20 |]; [| -14; 13; 6; 21; 23;
   12; 17; -23 |]; [| -27; -2; -5; 12; 17; 6; 10; -25 |]; [| -26; -4; -4; -10;
   3; 3; 33; -12 |]; [| -35; -1; -20; -23; -15; 24; 38; -22 |]; [| 0; 0; 0; 0;
   0; 0; 0; 0 |]; |]

   let pawnPSTEnd = [| [| 0; 0; 0; 0; 0; 0; 0; 0 |]; [| 178; 173; 158; 134; 147;
   132; 165; 187 |]; [| 94; 100; 85; 67; 56; 53; 82; 84 |]; [| 32; 24; 13; 5;
   -2; 4; 17; 17 |]; [| 13; 9; -3; -7; -7; -8; 3; -1 |]; [| 4; 7; -6; 1; 0; -5;
   -1; -8 |]; [| 13; 8; 8; 10; 13; 0; 2; -7 |]; [| 0; 0; 0; 0; 0; 0; 0; 0 |]; |]

   let knightPSTMid = [| [| -167; -89; -34; -49; 61; -97; -15; -107 |]; [| -73;
   -41; 72; 36; 23; 62; 7; -17 |]; [| -47; 60; 37; 65; 84; 129; 73; 44 |]; [|
   -9; 17; 19; 53; 37; 69; 18; 22 |]; [| -13; 4; 16; 13; 28; 19; 21; -8 |]; [|
   -23; -9; 12; 10; 19; 17; 25; -16 |]; [| -29; -53; -12; -3; -1; 18; -14; -19
   |]; [| -105; -21; -58; -33; -17; -28; -19; -23 |]; |]

   let knightPSTEnd = [| [| -58; -38; -13; -28; -31; -27; -63; -99 |]; [| -25;
   -8; -25; -2; -9; -25; -24; -52 |]; [| -24; -20; 10; 9; -1; -9; -19; -41 |];
   [| -17; 3; 22; 22; 22; 11; 8; -18 |]; [| -18; -6; 16; 25; 16; 17; 4; -18 |];
   [| -23; -3; -1; 15; 10; -3; -20; -22 |]; [| -42; -20; -10; -5; -2; -20; -23;
   -44 |]; [| -29; -51; -23; -15; -22; -18; -50; -64 |]; |]

   let bishopPSTMid = [| [| -29; 4; -82; -37; -25; -42; 7; -8 |]; [| -26; 16;
   -18; -13; 30; 59; 18; -47 |]; [| -16; 37; 43; 40; 35; 50; 37; -2 |]; [| -4;
   5; 19; 50; 37; 37; 7; -2 |]; [| -6; 13; 13; 26; 34; 12; 10; 4 |]; [| 0; 15;
   15; 15; 14; 27; 18; 10 |]; [| 4; 15; 16; 0; 7; 21; 33; 1 |]; [| -33; -3; -14;
   -21; -13; -12; -39; -21 |]; |]

   let bishopPSTEnd = [| [| -14; -21; -11; -8; -7; -9; -17; -24 |]; [| -8; -4;
   7; -12; -3; -13; -4; -14 |]; [| 2; -8; 0; -1; -2; 6; 0; 4 |]; [| -3; 9; 12;
   9; 14; 10; 3; 2 |]; [| -6; 3; 13; 19; 7; 10; -3; -9 |]; [| -12; -3; 8; 10;
   13; 3; -7; -15 |]; [| -14; -18; -7; -1; 4; -9; -15; -27 |]; [| -23; -9; -23;
   -5; -9; -16; -5; -17 |]; |]

   let rookPSTMid = [| [| 32; 42; 32; 51; 63; 9; 31; 43 |]; [| 27; 32; 58; 62;
   80; 67; 26; 44 |]; [| -5; 19; 26; 36; 17; 45; 61; 16 |]; [| -24; -11; 7; 26;
   24; 35; -8; -20 |]; [| -36; -26; -12; -1; 9; -7; 6; -23 |]; [| -45; -25; -16;
   -17; 3; 0; -5; -33 |]; [| -44; -16; -20; -9; -1; 11; -6; -71 |]; [| -19; -13;
   1; 17; 16; 7; -37; -26 |]; |]

   let rookPSTEnd = [| [| 13; 10; 18; 15; 12; 12; 8; 5 |]; [| 11; 13; 13; 11;
   -3; 3; 8; 3 |]; [| 7; 7; 7; 5; 4; -3; -5; -3 |]; [| 4; 3; 13; 1; 2; 1; -1; 2
   |]; [| 3; 5; 8; 4; -5; -6; -8; -11 |]; [| -4; 0; -5; -1; -7; -12; -8; -16 |];
   [| -6; -6; 0; 2; -9; -9; -11; -3 |]; [| -9; 2; 3; -1; -5; -13; 4; -20 |]; |]

   let queenPSTMid = [| [| -28; 0; 29; 12; 59; 44; 43; 45 |]; [| -24; -39; -5;
   1; -16; 57; 28; 54 |]; [| -13; -17; 7; 8; 29; 56; 47; 57 |]; [| -27; -27;
   -16; -16; -1; 17; -2; 1 |]; [| -9; -26; -9; -10; -2; -4; 3; -3 |]; [| -14; 2;
   -11; -2; -5; 2; 14; 5 |]; [| -35; -8; 11; 2; 8; 15; -3; 1 |]; [| -1; -18; -9;
   10; -15; -25; -31; -50 |]; |]

   let queenPSTEnd = [| [| -9; 22; 22; 27; 27; 19; 10; 20 |]; [| -17; 20; 32;
   41; 58; 25; 30; 0 |]; [| -20; 6; 9; 49; 47; 35; 19; 9 |]; [| 3; 22; 24; 45;
   57; 40; 57; 36 |]; [| -18; 28; 19; 47; 31; 34; 39; 23 |]; [| -16; -27; 15; 6;
   9; 17; 10; 5 |]; [| -22; -23; -30; -16; -16; -23; -36; -32 |]; [| -33; -28;
   -22; -43; -5; -32; -20; -41 |]; |]

   let kingPSTMid = [| [| -65; 23; 16; -15; -56; -34; 2; 13 |]; [| 29; -1; -20;
   -7; -8; -4; -38; -29 |]; [| -9; 24; 2; -16; -20; 6; 22; -22 |]; [| -17; -20;
   -12; -27; -30; -25; -14; -36 |]; [| -49; -1; -27; -39; -46; -44; -33; -51 |];
   [| -14; -14; -22; -46; -44; -30; -15; -27 |]; [| 1; 7; -8; -64; -43; -16; 9;
   8 |]; [| -15; 36; 12; -54; 8; -28; 24; 14 |]; |]

   let kingPSTEnd = [| [| -74; -35; -18; -18; -11; 15; 4; -17 |]; [| -12; 17;
   14; 17; 17; 38; 23; 11 |]; [| 10; 17; 23; 15; 20; 45; 44; 13 |]; [| -8; 22;
   24; 27; 26; 33; 26; 3 |]; [| -18; -4; 21; 24; 27; 23; 9; -11 |]; [| -19; -3;
   11; 21; 23; 16; 7; -9 |]; [| -27; -11; 4; 13; 14; 4; -5; -17 |]; [| -53; -34;
   -21; -11; -28; -14; -24; -43 |]; |]

   let weakPawnPST = [| [| 0; 0; 0; 0; 0; 0; 0; 0 |]; [| -10; -12; -14; -16;
   -16; -14; -12; -10 |]; [| -10; -12; -14; -16; -16; -14; -12; -10 |]; [| -10;
   -12; -14; -16; -16; -14; -12; -10 |]; [| -10; -12; -14; -16; -16; -14; -12;
   -10 |]; [| -10; -12; -14; -16; -16; -14; -12; -10 |]; [| -10; -12; -14; -16;
   -16; -14; -12; -10 |]; [| 0; 0; 0; 0; 0; 0; 0; 0 |]; |]

   let passedPawnPST = [| [| 0; 0; 0; 0; 0; 0; 0; 0 |]; [| 20; 20; 20; 20; 20;
   20; 20; 20 |]; [| 20; 20; 20; 20; 20; 20; 20; 20 |]; [| 32; 32; 32; 32; 32;
   32; 32; 32 |]; [| 56; 56; 56; 56; 56; 56; 56; 56 |]; [| 92; 92; 92; 92; 92;
   92; 92; 92 |]; [| 140; 140; 140; 140; 140; 140; 140; 140 |]; [| 0; 0; 0; 0;
   0; 0; 0; 0 |]; |]

   let midTable = [| pawnPSTMid; knightPSTMid; bishopPSTMid; rookPSTMid;
   queenPSTMid; kingPSTMid; |]

   let endTable = [| pawnPSTEnd; knightPSTEnd; bishopPSTEnd; rookPSTEnd;
   queenPSTEnd; kingPSTEnd; |]

   let shield1 = 10 let shield2 = 5

   (**[wkingShield board color] is a score for how protected the white king is.
   We basically want pawns in front of the king to help protect checkmate
   opporunities. *) let wkingShield board = let open Base.Int64 in let result =
   ref 0 in let pawns = ref (Board.get_piece_bitBoard board Board.pawn
   Board.white land shift_left Board.file1 1) in let king = Board.bit_to_tuple
   (Board.get_piece_bitBoard board Board.king Board.white) in if Stdlib.( = )
   (fst king) 0 && Stdlib.( > ) (snd king) 4 then ( let return = ref 0 in while
   !pawns <> zero do let lsb = ref (!pawns land neg !pawns) in let rank, file =
   Board.bit_to_tuple !lsb in if Stdlib.( = ) file 1 && Stdlib.( > ) rank 4 then
   result := Stdlib.( + ) !result shield1 else if Stdlib.( = ) file 2 &&
   Stdlib.( > ) rank 4 then result := Stdlib.( + ) !result shield2 else ();
   pawns := !pawns land (!pawns - one) done; !return) else if Stdlib.( = ) (fst
   king) 0 && Stdlib.( <= ) (snd king) 4 then ( let return = ref 0 in while
   !pawns <> zero do let lsb = ref (!pawns land neg !pawns) in let rank, file =
   Board.bit_to_tuple !lsb in if Stdlib.( = ) file 1 && Stdlib.( <= ) rank 4
   then result := Stdlib.( + ) !result shield1 else if Stdlib.( = ) file 2 &&
   Stdlib.( <= ) rank 4 then result := Stdlib.( + ) !result shield2 else ();
   pawns := !pawns land (!pawns - one) done; !return) else 0

   (**[bkingShield board color] is a score for how protected the white king is.
   We basically want pawns in front of the king to help protect checkmate
   opporunities. *) let bkingShield board = let open Base.Int64 in let result =
   ref 0 in let pawns = ref (Board.get_piece_bitBoard board Board.pawn
   Board.black land shift_right_logical Board.file8 1) in let king =
   Board.bit_to_tuple (Board.get_piece_bitBoard board Board.king Board.black) in
   if Stdlib.( = ) (fst king) 0 && Stdlib.( > ) (snd king) 4 then ( let return =
   ref 0 in while !pawns <> zero do let lsb = ref (!pawns land neg !pawns) in
   let rank, file = Board.bit_to_tuple !lsb in if Stdlib.( = ) file 6 &&
   Stdlib.( > ) rank 4 then result := Stdlib.( + ) !result shield1 else if
   Stdlib.( = ) file 5 && Stdlib.( > ) rank 4 then result := Stdlib.( + )
   !result shield2 else (); pawns := !pawns land (!pawns - one) done; !return)
   else if Stdlib.( = ) (fst king) 0 && Stdlib.( <= ) (snd king) 4 then ( let
   return = ref 0 in while !pawns <> zero do let lsb = ref (!pawns land neg
   !pawns) in let rank, file = Board.bit_to_tuple !lsb in if Stdlib.( = ) file 6
   && Stdlib.( <= ) rank 4 then result := Stdlib.( + ) !result shield1 else if
   Stdlib.( = ) file 5 && Stdlib.( <= ) rank 4 then result := Stdlib.( + )
   !result shield2 else (); pawns := !pawns land (!pawns - one) done; !return)
   else 0

   (**The pawntable is an array of hashtables for previsouly evaluates pawn
   structures since they can be quite expensive to calculate each time. *) let
   pawnTable = [| Hashtbl.create 256; Hashtbl.create 256 |]

   (**[evalPawn board pawn color] evaluates a singular pawn on the board. *) let
   evalPawn board pawn color = let open Base.Int64 in let flagPassed = ref 1 in
   let flagOpposed = ref 1 in let flagWeak = ref 1 in let result = ref 0 in let
   supportingPawns = Board.get_piece_bitBoard board Board.pawn color in let
   opposingPawns = Board.get_piece_bitBoard board Board.pawn (Stdlib.( lxor )
   color 1) in let rank, file = Board.bit_to_tuple pawn in let bitMaskOpposed =
   shift_left Board.rankA (Stdlib.( * ) 8 rank) in if opposingPawns land
   bitMaskOpposed = zero then flagOpposed := 0 else (); let bitMaskPassed =
   bitMaskOpposed lor shift_left Board.rankA (Stdlib.( * ) 8 (Stdlib.( - ) rank
   1)) lor shift_left Board.rankA (Stdlib.( * ) 8 (Stdlib.( + ) rank 1)) in if
   opposingPawns land bitMaskPassed = zero then flagPassed := 0 else (); let
   leftPawn = shift_right_logical pawn 9 in let rightPawn = shift_left pawn 7 in
   if supportingPawns land (leftPawn land rightPawn) = zero then flagWeak := 0
   else (); if Stdlib.( = ) !flagPassed 0 then result := Stdlib.( + ) !result
   passedPawnPST.(rank).(file) else if Stdlib.( = ) !flagWeak 1 then result :=
   Stdlib.( + ) !result weakPawnPST.(rank).(file) else if Stdlib.( = )
   !flagOpposed 1 then result := Stdlib.( - ) !result 4 else (); !result

   (**[pawnScore board] evaluates the pawn score of both black and white.*) let
   pawnScore board = let open Base.Int64 in let whitePawns = ref
   (Board.get_piece_bitBoard board Board.pawn Board.white) in let blackPawns =
   ref (Board.get_piece_bitBoard board Board.pawn Board.white) in if Hashtbl.mem
   pawnTable.(Board.white) !whitePawns && Hashtbl.mem pawnTable.(Board.black)
   !blackPawns then Stdlib.( - ) (Hashtbl.find pawnTable.(Board.white)
   !whitePawns) (Hashtbl.find pawnTable.(Board.black) !blackPawns) else let
   whiteScore = ref (if Hashtbl.mem pawnTable.(Board.white) !whitePawns then
   Hashtbl.find pawnTable.(Board.white) !whitePawns else 0) and blackScore = ref
   (if Hashtbl.mem pawnTable.(Board.black) !whitePawns then Hashtbl.find
   pawnTable.(Board.black) !whitePawns else 0) in if Stdlib.( = ) !whiteScore 0
   then while !whitePawns <> zero do let lsb = ref (!whitePawns land neg
   !whitePawns) in whiteScore := Stdlib.( + ) !whiteScore (evalPawn board !lsb
   Board.white); whitePawns := !whitePawns land (!whitePawns - one) done else if
   Stdlib.( = ) !blackScore 0 then while !blackPawns <> zero do let lsb = ref
   (!blackPawns land neg !blackPawns) in blackScore := Stdlib.( + ) !blackScore
   (evalPawn board !lsb Board.black); blackPawns := !blackPawns land
   (!blackPawns - one) done else failwith "Why did we not evaluate previously?";
   Hashtbl.add pawnTable.(Board.white) !whitePawns !whiteScore; Hashtbl.add
   pawnTable.(Board.black) !blackPawns !blackScore; Stdlib.( - ) !whiteScore
   !blackScore

   let eval board = let midScore = ref 0 in let endScore = ref 0 in let
   gamePhase = ref 0 in let result = ref 0 in let adjustMaterial = [| 0; 0 |] in
   let counts = [| [| 0; 0 |]; [| 0; 0 |]; [| 0; 0 |]; [| 0; 0 |]; [| 0; 0 |];
   [| 0; 0 |] |] in for rank = 0 to 7 do for file = 0 to 7 do let piece =
   Board.get_piece board (rank, file) in match piece with | None -> () | Some x
   -> let pieceType = (x land 7) - 1 in let color = (x land 8) lsr 3 in if color
   = 1 then ( midScore := !midScore + pieceValuesMid.(pieceType) +
   midTable.(pieceType).(rank).(file); endScore := !endScore +
   pieceValuesEnd.(pieceType) + endTable.(pieceType).(rank).(file)) else (
   midScore := !midScore - pieceValuesMid.(pieceType) - midTable.(pieceType).(7
   - rank).(file); endScore := !endScore - pieceValuesEnd.(pieceType) -
   midTable.(pieceType).(7 - rank).(file)); counts.(pieceType).(color) <-
   counts.(pieceType).(color) + 1; gamePhase := !gamePhase +
   gamephaseInc.(pieceType) done done; let whiteKingShield = wkingShield board
   in let blackKingShield = bkingShield board in let pawnScore = pawnScore board
   in let midPhase = if !gamePhase > 24 then 24 else !gamePhase in let endPhase
   = 24 - midPhase in if counts.(Board.bishop).(Board.white) > 1 then
   adjustMaterial.(Board.white) <- adjustMaterial.(Board.white) + bishopPair
   else if counts.(Board.bishop).(Board.black) > 1 then
   adjustMaterial.(Board.black) <- adjustMaterial.(Board.black) + bishopPair
   else if counts.(Board.knight).(Board.white) > 1 then
   adjustMaterial.(Board.white) <- adjustMaterial.(Board.white) - knightPair
   else if counts.(Board.knight).(Board.black) > 1 then
   adjustMaterial.(Board.black) <- adjustMaterial.(Board.black) - knightPair
   else if counts.(Board.rook).(Board.white) > 1 then
   adjustMaterial.(Board.white) <- adjustMaterial.(Board.white) - rookPair else
   if counts.(Board.rook).(Board.black) > 1 then adjustMaterial.(Board.black) <-
   adjustMaterial.(Board.black) - rookPair else (); midScore := !midScore +
   whiteKingShield - blackKingShield; result := if Board.current_turn board = 0
   then !result + 10 else !result - 10; result := !result + pawnScore; result :=
   !result + (((!midScore * midPhase) - (!endScore * endPhase)) / 24); result :=
   !result + adjustMaterial.(0) - adjustMaterial.(1); !result

   (** [search_all_captures alpha beta board] searches through positions where
   only captures are available. *) let rec search_all_captures alpha beta board
   = let evaluation = eval board in if evaluation >= beta then beta else let
   alpha' = ref (max alpha evaluation) in let movesList = Board.legal_moves
   board in if Base.Array.is_empty movesList then if Board.player_check board
   (Board.current_turn board) then min_int else 0 else try for x = 0 to
   Base.Array.length movesList - 1 do let move = Base.Array.get movesList x in
   ignore (Board.make_move board move); let evaluation = -1 *
   search_all_captures (-1 * beta) (-1 * !alpha') board in Board.unmake_move
   board; if evaluation >= beta then ( alpha' := beta; failwith "") else alpha'
   := max !alpha' evaluation done; !alpha' with _ -> !alpha'

   (** [search searchDepth alpha beta board] searches for the best move and
   returns the evaluation of the best move available. *) let rec search
   searchDepth alpha beta board = if searchDepth = 0 then search_all_captures
   alpha beta board else let movesList = Board.legal_moves board in if
   Base.Array.length movesList = 0 then if Board.player_check board
   (Board.current_turn board) then min_int else 0 else let return = ref alpha in
   let alpha' = ref alpha in try for x = 0 to Base.Array.length movesList - 1 do
   let move = Base.Array.get movesList x in ignore (Board.make_move board move);
   let evaluation = -1 * search (searchDepth - 1) (-1 * beta) (-1 * !alpha')
   board in Board.unmake_move board; if evaluation >= beta then ( return :=
   beta; failwith "") else alpha' := max !alpha' evaluation done; !return with _
   -> !return

   let get_move board = let movesList = Board.legal_moves board in if
   Base.Array.is_empty movesList then failwith "No Legal Move" else let
   best_move = ref (Base.Array.get movesList 0) in let best_eval = ref min_int
   in for i = 0 to Base.Array.length movesList - 1 do let move = Base.Array.get
   movesList i in ignore (Board.make_move board move); let eval = -1 * search
   (depth - 1) min_int max_int board in Board.unmake_move board; if eval >
   !best_eval then ( best_eval := eval; best_move := move) done; !best_move *)
