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
        | Some piece ->
            let points =
              match Piece.get_type piece with
              | Piece.Pawn -> 1
              | Piece.Knight | Piece.Bishop -> 3
              | Piece.Rook -> 5
              | Piece.Queen -> 9
              | Piece.King -> 20
            in
            if Piece.get_color piece = Piece.White then white := !white + points
            else black := !black + points
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
        | Some piece ->
            let x', y' =
              if Piece.get_color piece = Piece.White then (x, y)
              else flip_black_pos x y
            in
            let points =
              match Piece.get_type piece with
              | Piece.Pawn -> 1 + pawn_pst.(x').(y')
              | Piece.Knight -> 3 + knight_pst.(x').(y')
              | Piece.Bishop -> 3 + bishop_pst.(x').(y')
              | Piece.Rook -> 5 + rook_pst.(x').(y')
              | Piece.Queen -> 9 + queen_pst.(x').(y')
              | Piece.King -> 20 + king_pst.(x').(y')
            in
            if Piece.get_color piece = Piece.White then white := !white + points
            else black := !black + points
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
  let eval board =
    let white = ref 0 in
    let black = ref 0 in
    let white_pawn_positions = ref [] in
    let black_pawn_positions = ref [] in

    for x = 0 to 7 do
      for y = 0 to 7 do
        match Board.get_piece board (x, y) with
        | None -> ()
        | Some piece ->
            let color = Piece.get_color piece in
            let piece_type = Piece.get_type piece in
            if piece_type = Piece.Pawn then
              if color = Piece.White then
                white_pawn_positions := (x, y) :: !white_pawn_positions
              else black_pawn_positions := (x, y) :: !black_pawn_positions;

            let x', y' =
              if color = Piece.White then (x, y) else flip_black_pos x y
            in

            let points =
              match piece_type with
              | Piece.Pawn -> 1 + pawn_pst.(x').(y')
              | Piece.Knight -> 3 + knight_pst.(x').(y')
              | Piece.Bishop -> 3 + bishop_pst.(x').(y')
              | Piece.Rook -> 5 + rook_pst.(x').(y')
              | Piece.Queen -> 9 + queen_pst.(x').(y')
              | Piece.King -> 20 + king_pst.(x').(y')
            in

            if color = Piece.White then white := !white + points
            else black := !black + points
      done
    done;

    let pawn_structure_penalty (positions : (int * int) list) : int =
      let files = Array.make 8 0 in
      List.iter (fun (_, y) -> files.(y) <- files.(y) + 1) positions;

      let penalty = ref 0 in
      for y = 0 to 7 do
        let count = files.(y) in
        if count > 1 then penalty := !penalty + ((count - 1) * 10);

        let is_isolated =
          (y = 0 || files.(y - 1) = 0) && (y = 7 || files.(y + 1) = 0)
        in
        if count > 0 && is_isolated then penalty := !penalty + 15;
        if count = 1 then
          let left = if y > 0 then files.(y - 1) else 0 in
          let right = if y < 7 then files.(y + 1) else 0 in
          if left = 0 && right = 0 then penalty := !penalty + 20
      done;
      !penalty
    in

    let white_penalty = pawn_structure_penalty !white_pawn_positions in
    let black_penalty = pawn_structure_penalty !black_pawn_positions in

    !white - white_penalty - (!black - black_penalty)

  let depth = I.depth
end

(**Functor for constructing stock fish clone. *)
module StockFishClone (I : IncompleteEngineSettings) : EngineSettings = struct
  let eval board = 0
  let depth = I.depth
end
