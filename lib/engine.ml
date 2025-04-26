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

  val get_move : Board.t -> (int * int) * (int * int)
  (**[get_move board color] is a tuple of 2 pairs of ints that hold the starting
     posistion of the piece that we want to move and the ending posistion we
     want to move the piece to.*)
end

module BasicEngine (Settings : EngineSettings) : Engine = struct
  type t =
    | Node of int * ((int * int) * (int * int)) list * t list
    | Leaf of int

  let eval = Settings.eval
  let depth = Settings.depth

  (**[generate_tree depth board] is a tree of all possible moves that the engine
     can do combined with the scores. *)
  let rec generate_tree depth board =
    let score = eval board in
    if depth = 0 then Leaf score
    else
      let legal_moves = Board.legal_moves board in
      let node_list =
        List.map
          (fun (x, _) -> x)
          (List.map (Board.make_move board) legal_moves)
      in
      Node (score, legal_moves, List.map (generate_tree (depth - 1)) node_list)

  (**[mini_max isMax node] is the optimal score to get.*)
  let rec mini_max isMax node =
    match node with
    | Leaf score -> (score, node)
    | Node (score, _, children) ->
        let scoreList = List.map (mini_max (not isMax)) children in
        if isMax then
          List.fold_left
            (fun (x1, y1) (x2, y2) -> if x2 > x1 then (x2, y2) else (x1, y1))
            (min_int, Leaf min_int) scoreList
        else
          List.fold_left
            (fun (x1, y1) (x2, y2) -> if x2 < x1 then (x2, y2) else (x1, y1))
            (min_int, Leaf min_int) scoreList

  let rec indexer lst index target =
    match lst with
    | [] -> raise Not_found
    | h :: t -> if h = target then index else indexer t (index + 1) target

  let get_move board =
    let tree = generate_tree depth board in
    let _, node = mini_max true tree in
    match tree with
    | Node (_, moves, children) ->
        let index = indexer children 0 node in
        List.nth moves index
    | Leaf _ -> failwith "invalid node"
end
