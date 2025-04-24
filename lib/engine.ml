open Settings

module type Engine = sig
  val eval : Board.t -> int
  (**[eval board] is the evaluation of a board posistion. If the number if more
     positive, then this denotes that white holds the advantage while if the
     number is more negative this denotes that black holds the advantage. *)

  val depth : int
  (**[depth] is the tree depth the engine searches. The higher the depth, the
     more moves the chess engine explores and the lower the depth, the less
     moves the chess engine explores.*)

  val get_move : Board.t -> Piece.color -> (int * int) * (int * int)
  (**[get_move board color] is a tuple of 2 pairs of ints that hold the starting
     posistion of the piece that we want to move and the ending posistion we
     want to move the piece to.*)
end

module BasicEngine (Settings : EngineSettings) : Engine = struct
  type t = Node of int * ((int * int) * (int * int)) * t list

  let eval = Settings.eval
  let depth = Settings.depth
  let get_move board color = ((0, 0), (0, 0))

  let rec generate_tree board depth =
    let eval = eval board in
    let children = ref [] in
    for x = 0 to 7 do
      for y = 0 to 7 do
        match Board.get_piece board (x, y) with
        | None -> ()
        | Some piece -> (
            match Piece.get_type piece with
            | Piece.Pawn -> if y = 1 || y = 6 then 
            | _ -> failwith "fuck")
      done
    done
end
