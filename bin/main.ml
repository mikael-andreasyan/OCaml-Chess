(* open Chess

   let board = Board.make_board ()

   (**regex exlusively matches only string that fufill chess notation*) let rec
   validInput input = let valid_pattern = Str.regexp "^[a-h][1-8]$" in try let _
   = Str.search_forward valid_pattern input 0 in let input = Str.matched_string
   input in input with Not_found -> let () = print_endline "Your input was
   invalid. Please try again using standard chess\n\ \ notation and only
   lowercase letters" in validInput (read_line ())

   (*Prompts the user with an input and returns a string*) let get_input () =
   validInput (read_line ())

   let piece_symbol piece_type = match piece_type with | Piece.Rook -> " R " |
   Piece.Bishop -> " B " | Piece.Knight -> " H " | Piece.Queen -> " Q " |
   Piece.King -> " K " | Piece.Pawn -> " p "

   let print_file () = ANSITerminal.print_string [ ANSITerminal.white;
   ANSITerminal.on_black ] "---------------------------\n a b c d e f g h ";
   print_newline ()

   (**Turns the first character of chess notation into a num. Requires: the
   first letter was already valid chess notation*) let get_file char = match
   char with | 'a' -> 0 | 'b' -> 1 | 'c' -> 2 | 'd' -> 3 | 'e' -> 4 | 'f' -> 5 |
   'g' -> 6 | 'h' -> 7 | _ -> raise (Sys_error "something went wrong")

   (**I found out that for some reason int_of_char returns the ascii value of
   the character instead of the number. So i had to do this janky code*) let
   get_rank rank = match rank with | '1' -> 0 | '2' -> 1 | '3' -> 2 | '4' -> 3 |
   '5' -> 4 | '6' -> 5 | '7' -> 6 | '8' -> 7 | _ -> raise (Sys_error "something
   went wrong")

   let print_board board = print_newline (); for r = 7 downto 0 do
   ANSITerminal.print_string [ ANSITerminal.white; ANSITerminal.on_black ]
   (string_of_int (r + 1) ^ " |"); for f = 0 to 7 do match Board.get_piece board
   (f, r) with | None -> ANSITerminal.print_string [ ANSITerminal.white;
   ANSITerminal.on_black ] " - " | Some piece -> ( match Piece.get_color piece
   with | Piece.White -> ANSITerminal.print_string [ ANSITerminal.white;
   ANSITerminal.on_black ] (piece_symbol (Piece.get_type piece)) | Piece.Black
   -> ANSITerminal.print_string [ ANSITerminal.blue; ANSITerminal.on_black ]
   (piece_symbol (Piece.get_type piece))) done; print_newline () done;
   print_file (); print_newline ()

   (**Takes in a string representing chess notation*) let rec process_start
   input board = let file = get_file input.[0] in let rank = get_rank input.[1]
   in match Board.get_piece board (file, rank) with | Some piece -> if
   Piece.get_color piece = Board.current_turn board then input else (
   print_endline "This piece is not a piece you can move because it is not the
   same \ color as the current player. Try another piece"; process_start
   (get_input ()) board) | None -> print_endline "There is no piece here. Try
   again"; process_start (get_input ()) board

   let rec process_input p_start p_end board = let start_file = get_file
   p_start.[0] in let start_rank = get_rank p_start.[1] in let end_file =
   get_file p_end.[0] in let end_rank = get_rank p_end.[1] in Board.make_move
   board (start_file, start_rank) (end_file, end_rank)

   let rec loop board = print_board board; (let curr_player = Board.current_turn
   board in match curr_player with | Piece.White -> print_endline "WHITE'S TURN"
   | Piece.Black -> print_endline "BLACK'S TURN"); print_endline "Pick a piece
   to move"; let start = get_input () in let start = process_start start board
   in print_endline "Where do you want to move this piece?"; let p_end =
   get_input () in if process_input start p_end board then () else print_endline
   "Sorry but the piece you chose can't move like that. You either chose an \
   invalid movement patten, your move was blocked by another piece, or you \
   tried to eat a piece of your own color\n\ It's still the same player's turn,
   please try again"; loop board

   let () = loop board *)
