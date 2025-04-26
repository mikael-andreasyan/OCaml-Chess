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
