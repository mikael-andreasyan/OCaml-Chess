(**Module type built to choose custom evaluation function*)
module type IncompleteEngineSettings = sig
  val depth : int
  (**[depth] is the tree depth the engine searches. The higher the depth, the
     more moves the chess engine explores and the lower the depth, the less
     moves the chess engine explores.*)
end

(**Module type for instatiating engine settings*)
module type EngineSettings = sig
  val eval : Board.t -> int
  (**[eval board] is the evaluation of a board posistion. If the number if more
     positive, then this denotes that white holds the advantage while if the
     number is more negative this denotes that black holds the advantage. *)

  val depth : int
  (**[depth] is the tree depth the engine searches. The higher the depth, the
     more moves the chess engine explores and the lower the depth, the less
     moves the chess engine explores.*)
end

(**Functor for constructing engine settings that have a evaluation function
   soley based on material. *)
module MaterialEngine (I : IncompleteEngineSettings) : EngineSettings = struct
  (**[eval board] for the MaterialEngine functor purely looks at the *)
  let eval board =
    let white = ref 0 in
    let black = ref 0 in
    for x = 0 to 7 do
      for y = 0 to 7 do
        match Board.get_piece board (x, y) with
        | None -> ()
        | Some piece -> (
            match Piece.get_type piece with
            | Piece.Pawn ->
                if Piece.get_color piece = Piece.White then white := !white + 1
                else black := !black + 1
            | Piece.Knight ->
                if Piece.get_color piece = Piece.White then white := !white + 3
                else black := !black + 3
            | Piece.Bishop ->
                if Piece.get_color piece = Piece.White then white := !white + 3
                else black := !black + 3
            | Piece.Rook ->
                if Piece.get_color piece = Piece.White then white := !white + 5
                else black := !black + 5
            | Piece.Queen ->
                if Piece.get_color piece = Piece.White then white := !white + 9
                else black := !black + 9
            | Piece.King ->
                if Piece.get_color piece = Piece.White then white := !white + 20
                else black := !black + 20)
      done
    done;
    !white - !black

  let depth = I.depth
end

(**Functor for constructing engine settings that have a evaluation function that
   uses a piece square table to evaluate. *)
module PieceSquareTable (I : IncompleteEngineSettings) : EngineSettings = struct
  let pawn_pst =
    [|
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 30; 30; 30; 40; 40; 30; 30; 30 |];
      [| 20; 20; 20; 30; 30; 30; 20; 20 |];
      [| 10; 10; 15; 25; 25; 15; 10; 10 |];
      [| 5; 5; 5; 20; 20; 5; 5; 5 |];
      [| 5; 0; 0; 5; 5; 0; 0; 5 |];
      [| 5; 5; 5; -10; -10; 5; 5; 5 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
    |]

  let knight_pst =
    [|
      [| -5; -5; -5; -5; -5; -5; -5; -5 |];
      [| -5; 0; 0; 10; 10; 0; 0; -5 |];
      [| -5; 5; 10; 10; 10; 10; 5; -5 |];
      [| -5; 5; 10; 15; 15; 10; 5; -5 |];
      [| -5; 5; 10; 15; 15; 10; 5; -5 |];
      [| -5; 5; 10; 10; 10; 10; 5; -5 |];
      [| -5; 0; 0; 5; 5; 0; 0; -5 |];
      [| -5; -10; -5; -5; -5; -5; -10; -5 |];
    |]

  let bishop_pst =
    [|
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 10; 0; 0; 0; 0; 10; 0 |];
      [| 5; 0; 10; 0; 0; 10; 0; 5 |];
      [| 0; 10; 0; 10; 10; 0; 10; 0 |];
      [| 0; 10; 0; 10; 10; 0; 10; 0 |];
      [| 0; 0; -10; 0; 0; -10; 0; 0 |];
    |]

  let rook_pst =
    [|
      [| 10; 10; 10; 10; 10; 10; 10; 10 |];
      [| 10; 10; 10; 10; 10; 10; 10; 10 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 10; 10; 0; 0; 0 |];
      [| 0; 0; 0; 10; 10; 5; 0; 0 |];
    |]

  let queen_pst =
    [|
      [| -20; -10; -10; -5; -5; -10; -10; -20 |];
      [| -10; 0; 0; 0; 0; 0; 0; -10 |];
      [| -10; 0; 5; 5; 5; 5; 0; -10 |];
      [| -5; 0; 5; 5; 5; 5; 0; -5 |];
      [| -5; 0; 5; 5; 5; 5; 0; -5 |];
      [| -10; 5; 5; 5; 5; 5; 0; -10 |];
      [| -10; 0; 5; 0; 0; 0; 0; -10 |];
      [| -20; -10; -10; 0; 0; -10; -10; -20 |];
    |]

  let king_pst =
    [|
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; -5; -5; -5; 0; 0 |];
      [| 0; 0; 10; -5; -5; -5; 10; 0 |];
    |]

  let flip_black_pos x y = (7 - x, y)

  (**[eval board] now uses both the board material and also the PST in order to
     evaluate the board. *)
  let eval board =
    let white = ref 0 in
    let black = ref 0 in
    for x = 0 to 7 do
      for y = 0 to 7 do
        match Board.get_piece board (x, y) with
        | None -> ()
        | Some piece -> (
            match Piece.get_type piece with
            | Piece.Pawn ->
                if Piece.get_color piece = Piece.White then
                  white := !white + 1 + pawn_pst.(x).(y)
                else
                  let x', y' = flip_black_pos x y in
                  black := !black + 1 + pawn_pst.(x').(y')
            | Piece.Knight ->
                if Piece.get_color piece = Piece.White then
                  white := !white + 3 + knight_pst.(x).(y)
                else
                  let x', y' = flip_black_pos x y in
                  black := !black + 3 + knight_pst.(x').(y')
            | Piece.Bishop ->
                if Piece.get_color piece = Piece.White then
                  white := !white + 3 + bishop_pst.(x).(y)
                else
                  let x', y' = flip_black_pos x y in
                  black := !black + 3 + bishop_pst.(x').(y')
            | Piece.Rook ->
                if Piece.get_color piece = Piece.White then
                  white := !white + 5 + rook_pst.(x).(y)
                else
                  let x', y' = flip_black_pos x y in
                  black := !black + 5 + rook_pst.(x').(y')
            | Piece.Queen ->
                if Piece.get_color piece = Piece.White then
                  white := !white + 9 + queen_pst.(x).(y)
                else
                  let x', y' = flip_black_pos x y in
                  black := !black + 9 + queen_pst.(x').(y')
            | Piece.King ->
                if Piece.get_color piece = Piece.White then
                  white := !white + 20 + king_pst.(x).(y)
                else
                  let x', y' = flip_black_pos x y in
                  black := !black + 20 + king_pst.(x').(y'))
      done
    done;
    !white - !black

  let depth = I.depth
end

(**Functor for constructing engine settings that have a evaluation function that
   uses a piece square table to evaluate plus pawn structure. *)
module PawnStruct (I : IncompleteEngineSettings) : EngineSettings = struct
  let pawn_pst =
    [|
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 30; 30; 30; 40; 40; 30; 30; 30 |];
      [| 20; 20; 20; 30; 30; 30; 20; 20 |];
      [| 10; 10; 15; 25; 25; 15; 10; 10 |];
      [| 5; 5; 5; 20; 20; 5; 5; 5 |];
      [| 5; 0; 0; 5; 5; 0; 0; 5 |];
      [| 5; 5; 5; -10; -10; 5; 5; 5 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
    |]

  let knight_pst =
    [|
      [| -5; -5; -5; -5; -5; -5; -5; -5 |];
      [| -5; 0; 0; 10; 10; 0; 0; -5 |];
      [| -5; 5; 10; 10; 10; 10; 5; -5 |];
      [| -5; 5; 10; 15; 15; 10; 5; -5 |];
      [| -5; 5; 10; 15; 15; 10; 5; -5 |];
      [| -5; 5; 10; 10; 10; 10; 5; -5 |];
      [| -5; 0; 0; 5; 5; 0; 0; -5 |];
      [| -5; -10; -5; -5; -5; -5; -10; -5 |];
    |]

  let bishop_pst =
    [|
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 10; 0; 0; 0; 0; 10; 0 |];
      [| 5; 0; 10; 0; 0; 10; 0; 5 |];
      [| 0; 10; 0; 10; 10; 0; 10; 0 |];
      [| 0; 10; 0; 10; 10; 0; 10; 0 |];
      [| 0; 0; -10; 0; 0; -10; 0; 0 |];
    |]

  let rook_pst =
    [|
      [| 10; 10; 10; 10; 10; 10; 10; 10 |];
      [| 10; 10; 10; 10; 10; 10; 10; 10 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 10; 10; 0; 0; 0 |];
      [| 0; 0; 0; 10; 10; 5; 0; 0 |];
    |]

  let queen_pst =
    [|
      [| -20; -10; -10; -5; -5; -10; -10; -20 |];
      [| -10; 0; 0; 0; 0; 0; 0; -10 |];
      [| -10; 0; 5; 5; 5; 5; 0; -10 |];
      [| -5; 0; 5; 5; 5; 5; 0; -5 |];
      [| -5; 0; 5; 5; 5; 5; 0; -5 |];
      [| -10; 5; 5; 5; 5; 5; 0; -10 |];
      [| -10; 0; 5; 0; 0; 0; 0; -10 |];
      [| -20; -10; -10; 0; 0; -10; -10; -20 |];
    |]

  let king_pst =
    [|
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; -5; -5; -5; 0; 0 |];
      [| 0; 0; 10; -5; -5; -5; 10; 0 |];
    |]

  let flip_black_pos x y = (7 - x, y)

  (**[eval board] now uses both the board material and also the PST in order to
     evaluate the board as well as pawn structure.*)
  let eval board = 0

  let depth = I.depth
end

(**Functor for constructing stock fish clone. *)
module StockFishClone (I : IncompleteEngineSettings) : EngineSettings = struct
  let eval board = 0
  let depth = I.depth
end
