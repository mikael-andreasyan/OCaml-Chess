open Graphics
open Chess

(**color definitions*)

let dark = 0x5f8522
let light = 0xd7db98
let dark_draw = black
let light_draw = 0xf9f6f2
let select = 0x92dae8
let move_color = 0xadafb9

let board =
  Board.make_board2 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

(**a collection of functions that draw the pieces as representations of basic
   shapes. All follow the same format of [draw_piece offset length (f,r) color],
   where the [offset] is how far the board is drawn from the corner of the
   screen, [length] is the length of each tile, [f,r] is the file and rank of
   the piece and [color] is the color of the piece*)

let draw_pawn offset_x length (f, r) color =
  set_color color;
  fill_rect
    (offset_x + (f * length) + (length / 4))
    ((r * length) + (length / 4))
    (length / 2) (length / 2)

let draw_knight offset_x length (f, r) color =
  set_color color;
  fill_rect
    (offset_x + (f * length) + (length / 4))
    ((r * length) + (length / 8))
    (length / 4)
    (length * 6 / 8);
  fill_rect
    (offset_x + (f * length) + (length / 4))
    ((r * length) + (length / 2))
    (length * 2 / 4)
    (length * 1 / 4)

let draw_bish offset_x length (f, r) color =
  set_color color;
  fill_rect
    (offset_x + (f * length) + (length * 3 / 8))
    ((r * length) + (length / 8))
    (length / 4)
    (length * 3 / 5);
  fill_circle
    (offset_x + (f * length) + (length / 2))
    ((r * length) + (length * 11 / 16))
    (length / 4)

let draw_rook offset_x length (f, r) color =
  set_color color;
  fill_rect
    (offset_x + (f * length) + (length / 4))
    ((r * length) + (length / 4))
    (length / 2) (length / 3);
  fill_rect
    (offset_x + (f * length) + (length / 4))
    ((r * length) + (length / 2))
    (length / 6) (length / 4);
  fill_rect
    (offset_x + (f * length) + (length / 4) + (length / 3))
    ((r * length) + (length / 2))
    (length / 6) (length / 4)

let draw_queen offset_x length (f, r) color =
  set_color color;
  fill_rect
    (offset_x + (f * length) + (length / 4))
    ((r * length) + (length / 8))
    (length / 2) (length / 4);
  let left_side_x = offset_x + (f * length) + (length / 4) in
  let right_side_x = offset_x + ((f + 1) * length) - (length / 4) in
  let mid_left_x = left_side_x + (length / 6) in
  let mid_right_x = left_side_x + (length / 3) in
  let bottom_y = (r * length) + (length * 3 / 8) in
  let top_y = (r * length) + (length * 7 / 8) in
  let points_array =
    [|
      (left_side_x, top_y);
      (right_side_x, top_y);
      (mid_right_x, bottom_y);
      (mid_left_x, bottom_y);
    |]
  in
  fill_poly points_array

let draw_king offset_x length (f, r) color =
  if
    Board.playerLose board
    && ((Board.current_turn board = Board.white && color = light_draw)
       || (Board.current_turn board = Board.black && color = dark_draw))
  then (
    set_color red;
    fill_rect (offset_x + (f * length)) (r * length) length length)
  else ();
  set_color color;
  fill_rect
    (offset_x + (f * length) + (length / 4))
    ((r * length) + (length / 8))
    (length / 2) (length / 4);
  let left_side_x = offset_x + (f * length) + (length / 4) in
  let right_side_x = offset_x + ((f + 1) * length) - (length / 4) in
  let mid_x = left_side_x + (length / 4) in
  let bottom_y = (r * length) + (length * 3 / 8) in
  let top_y = (r * length) + (length * 7 / 8) in
  let points_array =
    [| (mid_x, top_y); (left_side_x, bottom_y); (right_side_x, bottom_y) |]
  in
  fill_poly points_array

(**an array that has the appropriate index matching to the piece type. The index
   is based on the number that the piece is associated with in the get piece
   function*)
let draw_key =
  [|
    (fun _ _ _ _ -> ());
    draw_pawn;
    draw_knight;
    draw_bish;
    draw_rook;
    draw_queen;
    draw_king;
  |]

(**[draw_piece_at (rank, file) offset_x length] draws the piece found at
   [rank,file] of the board unto the screen. [offset_x] is how much the board
   drawing is offset from the edge of the screen and [length] is the size of
   each tile*)
let draw_piece_at (file, rank) offset_x length =
  match Board.get_piece board (file, rank) with
  | Some piece ->
      let color = if Int.logand piece 8 = 8 then light_draw else dark_draw in
      let piece_num = if color = light_draw then piece - 8 else piece in
      draw_key.(piece_num) offset_x length (file, rank) color
  | None -> ()

(**swaps the current color to the other color*)
let swap_color color = if color = light then dark else light

(**[draw_board location_x length] draws the board with the squares and pieces,
   with [length] being the length of the tiles and [location_x] being the
   starting point of the board on the x axis*)
let draw_board location_x length =
  let color = ref dark in
  for file = 0 to 7 do
    for rank = 0 to 7 do
      if rank = 0 then () else color := swap_color !color;
      set_color !color;
      fill_rect (location_x + (file * length)) (rank * length) length length;
      draw_piece_at (file, rank) location_x length
    done
  done

(**[draw_move (file,rank) start_x length] draws a little circle that is supposed
   to indicate a move at (file,rank)*)
let draw_move (file, rank) start_x length =
  fill_circle
    (start_x + (file * length) + (length / 2))
    ((rank * length) + (length / 2))
    (length / 8)

(**[draw_moves_for (file,rank) start_x length] draws all the possible moves for
   the piece at [file,rank] by drawing little black cirlces for valid moves.*)
let draw_moves_for (file, rank) start_x length =
  set_color move_color;
  let moves_for_piece =
    List.filter
      (fun (st, _, _) -> st = (file, rank))
      (Array.to_list (Board.legal_moves board))
  in
  List.iter (fun (_, ed, _) -> draw_move ed start_x length) moves_for_piece

let draw_selected start_x length (file, rank) =
  set_color select;
  fill_rect ((file * length) + start_x) (rank * length) length length;
  draw_piece_at (file, rank) start_x length

(**[move_piece status] checks if the location where the user pressed to select a
   piece was valid. Then, it'll make the piece follow the cursor of the player
   until they select the place they want to put their piece.*)
let rec move_piece status length start_x =
  draw_board start_x length;
  let piece_x_start = (status.mouse_x - start_x) / length in
  let piece_y_start = status.mouse_y / length in
  if
    status.mouse_x < start_x
    || status.mouse_x > start_x + (length * 8)
    || Board.get_piece board (piece_x_start, piece_y_start) = None
    || Board.playerLose board
  then
    let status_new = wait_next_event [ Button_down ] in
    move_piece status_new length start_x
  else (
    draw_selected start_x length (piece_x_start, piece_y_start);
    draw_moves_for (piece_x_start, piece_y_start) start_x length;
    let status_new = wait_next_event [ Button_down ] in
    let piece_end_x = (status_new.mouse_x - start_x) / length in
    let piece_end_y = status_new.mouse_y / length in
    if
      if
        Board.get_piece board (piece_x_start, piece_y_start) = Some 9
        && piece_end_y = 7
        || Board.get_piece board (piece_x_start, piece_y_start) = Some 1
           && piece_end_y = 0
      then
        Board.make_move board
          ( (piece_x_start, piece_y_start),
            (piece_end_x, piece_end_y),
            Some Board.queen )
      else
        Board.make_move board
          ((piece_x_start, piece_y_start), (piece_end_x, piece_end_y), None)
    then (
      draw_board start_x length;
      draw_selected start_x length (piece_end_x, piece_end_y))
    else (
      draw_board start_x length;
      move_piece status_new length start_x))

(**[start_gui ai first] starts the gui. If [ai] is true, the player is playing
   against an AI. If [first] is true it means the player is going first.
   Otherwise, it means the player is playing second*)
let start_gui ai first =
  open_graph "";
  set_window_title "Le Critters' Bloody Chess";
  resize_window 1280 720;
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ());
  draw_board ((size_x () - size_y ()) / 2) (size_y () / 8);
  while true do
    if ai && (not first) && not (Board.playerLose board) then (
      let move = Engine.get_move board in
      ignore (Board.make_move board move);
      draw_board ((size_x () - size_y ()) / 2) (size_y () / 8))
    else ();
    let status = wait_next_event [ Button_down ] in
    let y = size_y () in
    let x = size_x () in
    let tile_length = y / 8 in
    set_color black;
    fill_rect 0 0 x y;
    let location_x = (x - y) / 2 in
    draw_board location_x tile_length;
    move_piece status tile_length location_x;
    if ai && first && not (Board.playerLose board) then
      let move = Engine.get_move board in
      let _ = Board.make_move board move in
      draw_board location_x tile_length
    else print_endline "black didn't move"
  done

let print_usage () =
  print_endline
    "Usage: if you want to play against the AI, use the string white or the \
     string black as argument to this program. If you want to play a hotseat \
     game with a friend, just launch the exe"

let () =
  if Array.length Sys.argv = 1 then start_gui false false
  else if Sys.argv.(1) = "white" then start_gui true true
  else if Sys.argv.(1) = "black" then start_gui true false
  else print_usage ()
