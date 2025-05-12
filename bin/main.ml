open Graphics
open Chess

(**color definitions*)

let dark = 0x5f8522
let light = 0xd7db98
let dark_draw = black
let light_draw = 0xf9f6f2
let select = 0x92dae8

let board =
  Board.make_board2 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

(**a collection of functions that draw the pieces as representations of basic
   shapes. All follow the same format of [draw_piece offset length (r,f) color],
   where the [offset] is how far the board is drawn from the corner of the
   screen, [length] is the length of each tile, [r,f] is the rank and file of
   the piece and [color] is the color of the piece*)

let draw_pawn offset_x length (r, f) color =
  set_color color;
  fill_rect
    (offset_x + (f * length) + (length / 4))
    ((r * length) + (length / 4))
    (length / 2) (length / 2)

let draw_knight offset_x length (r, f) color =
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

let draw_bish offset_x length (r, f) color =
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

let draw_rook offset_x length (r, f) color =
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

let draw_queen offset_x length (r, f) color =
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

let draw_king offset_x length (r, f) color =
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
let draw_piece_at (rank, file) offset_x length =
  match Board.get_piece board (rank, file) with
  | Some piece ->
      let color = if Int.logand piece 8 = 8 then light_draw else dark_draw in
      let piece_num = if color = light_draw then piece - 8 else piece in
      draw_key.(piece_num) offset_x length (rank, file) color
  | None -> ()

(**swaps the current color to the other color*)
let swap_color color = if color = light then dark else light

(**[draw_board location_x length] draws the board with the squares and pieces,
   with [length] being the length of the tiles and [location_x] being the
   starting point of the board on the x axis*)
let draw_board location_x length =
  let color = ref dark in
  for r = 0 to 7 do
    for c = 0 to 7 do
      if c = 0 then () else color := swap_color !color;
      set_color !color;
      fill_rect (location_x + (r * length)) (c * length) length length;
      draw_piece_at (c, r) location_x length
    done
  done

(**[move_piece status] checks if the location where the user pressed to select a
   piece was valid. Then, it'll make the piece follow the cursor of the player
   until they select the place they want to put their piece.*)
let rec move_piece status length start_x =
  let piece_x_start = (status.mouse_x - start_x) / length in
  let piece_y_start = status.mouse_y / length in
  if
    status.mouse_x < start_x
    || status.mouse_x > start_x + (length * 8)
    || Board.get_piece board (piece_y_start, piece_x_start) = None
  then ()
  else (
    set_color select;
    fill_rect
      ((piece_x_start * length) + start_x)
      (piece_y_start * length) length length;
    let status_new = wait_next_event [ Button_down ] in
    let _ = (status.mouse_x - start_x) / length in
    let _ = status.mouse_y / length in
    if false then () else draw_board start_x length;
    move_piece status_new length start_x)

let () =
  open_graph "";
  set_window_title "Le Critters Chess";
  resize_window 1280 720;
  while true do
    let status = wait_next_event [ Button_down; Button_up ] in
    let y = size_y () in
    let x = size_x () in
    let tile_length = y / 8 in
    set_color black;
    fill_rect 0 0 x y;
    let location_x = (x - y) / 2 in
    draw_board location_x tile_length;
    if button_down () then move_piece status tile_length location_x else ()
  done
