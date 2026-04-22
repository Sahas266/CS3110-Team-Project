open Board
open Tsdl

let cell_size = 80
let margin = 36
let info_height = 136
let board_pixels = board_size * cell_size
let window_width = board_pixels + (2 * margin)
let window_height = board_pixels + (2 * margin) + info_height

type t = {
  renderer : Sdl.renderer;
  textures : (piece * Sdl.texture) list;
}

let sdl = function
  | Ok value -> value
  | Error (`Msg msg) -> failwith msg

let rgb hex = ((hex lsr 16) land 0xff, (hex lsr 8) land 0xff, hex land 0xff, 255)

let with_color renderer (r, g, b, a) f =
  sdl (Sdl.set_render_draw_color renderer r g b a);
  f ()

let rect ~x ~y ~w ~h = Sdl.Rect.create ~x ~y ~w ~h

let fill_rect renderer color ~x ~y ~w ~h =
  with_color renderer color (fun () ->
      sdl (Sdl.render_fill_rect renderer (Some (rect ~x ~y ~w ~h))))

let draw_line renderer color x1 y1 x2 y2 =
  with_color renderer color (fun () ->
      sdl (Sdl.render_draw_line renderer x1 y1 x2 y2))

let draw_outline renderer color ~x ~y ~w ~h =
  with_color renderer color (fun () ->
      sdl (Sdl.render_draw_rect renderer (Some (rect ~x ~y ~w ~h))))

let fill_circle renderer color ~cx ~cy ~radius =
  with_color renderer color (fun () ->
      for dy = -radius to radius do
        let width =
          sqrt (float_of_int ((radius * radius) - (dy * dy))) |> int_of_float
        in
        sdl
          (Sdl.render_fill_rect renderer
             (Some (rect ~x:(cx - width) ~y:(cy + dy) ~w:((2 * width) + 1) ~h:1)))
      done)

let font_scale = 3
let glyph_pixel = font_scale
let glyph_width = 5
let glyph_height = 7
let glyph_advance = (glyph_width * glyph_pixel) + glyph_pixel
let line_advance = (glyph_height * glyph_pixel) + (2 * glyph_pixel)

let glyph_rows ch =
  match Char.uppercase_ascii ch with
  | 'A' -> [ "01110"; "10001"; "10001"; "11111"; "10001"; "10001"; "10001" ]
  | 'B' -> [ "11110"; "10001"; "10001"; "11110"; "10001"; "10001"; "11110" ]
  | 'C' -> [ "01110"; "10001"; "10000"; "10000"; "10000"; "10001"; "01110" ]
  | 'D' -> [ "11110"; "10001"; "10001"; "10001"; "10001"; "10001"; "11110" ]
  | 'E' -> [ "11111"; "10000"; "10000"; "11110"; "10000"; "10000"; "11111" ]
  | 'F' -> [ "11111"; "10000"; "10000"; "11110"; "10000"; "10000"; "10000" ]
  | 'G' -> [ "01110"; "10001"; "10000"; "10111"; "10001"; "10001"; "01110" ]
  | 'H' -> [ "10001"; "10001"; "10001"; "11111"; "10001"; "10001"; "10001" ]
  | 'I' -> [ "11111"; "00100"; "00100"; "00100"; "00100"; "00100"; "11111" ]
  | 'J' -> [ "00111"; "00010"; "00010"; "00010"; "10010"; "10010"; "01100" ]
  | 'K' -> [ "10001"; "10010"; "10100"; "11000"; "10100"; "10010"; "10001" ]
  | 'L' -> [ "10000"; "10000"; "10000"; "10000"; "10000"; "10000"; "11111" ]
  | 'M' -> [ "10001"; "11011"; "10101"; "10101"; "10001"; "10001"; "10001" ]
  | 'N' -> [ "10001"; "11001"; "10101"; "10011"; "10001"; "10001"; "10001" ]
  | 'O' -> [ "01110"; "10001"; "10001"; "10001"; "10001"; "10001"; "01110" ]
  | 'P' -> [ "11110"; "10001"; "10001"; "11110"; "10000"; "10000"; "10000" ]
  | 'Q' -> [ "01110"; "10001"; "10001"; "10001"; "10101"; "10010"; "01101" ]
  | 'R' -> [ "11110"; "10001"; "10001"; "11110"; "10100"; "10010"; "10001" ]
  | 'S' -> [ "01111"; "10000"; "10000"; "01110"; "00001"; "00001"; "11110" ]
  | 'T' -> [ "11111"; "00100"; "00100"; "00100"; "00100"; "00100"; "00100" ]
  | 'U' -> [ "10001"; "10001"; "10001"; "10001"; "10001"; "10001"; "01110" ]
  | 'V' -> [ "10001"; "10001"; "10001"; "10001"; "10001"; "01010"; "00100" ]
  | 'W' -> [ "10001"; "10001"; "10001"; "10101"; "10101"; "10101"; "01010" ]
  | 'X' -> [ "10001"; "10001"; "01010"; "00100"; "01010"; "10001"; "10001" ]
  | 'Y' -> [ "10001"; "10001"; "01010"; "00100"; "00100"; "00100"; "00100" ]
  | 'Z' -> [ "11111"; "00001"; "00010"; "00100"; "01000"; "10000"; "11111" ]
  | '0' -> [ "01110"; "10001"; "10011"; "10101"; "11001"; "10001"; "01110" ]
  | '1' -> [ "00100"; "01100"; "00100"; "00100"; "00100"; "00100"; "01110" ]
  | '2' -> [ "01110"; "10001"; "00001"; "00010"; "00100"; "01000"; "11111" ]
  | '3' -> [ "11110"; "00001"; "00001"; "01110"; "00001"; "00001"; "11110" ]
  | '4' -> [ "00010"; "00110"; "01010"; "10010"; "11111"; "00010"; "00010" ]
  | '5' -> [ "11111"; "10000"; "10000"; "11110"; "00001"; "00001"; "11110" ]
  | '6' -> [ "01110"; "10000"; "10000"; "11110"; "10001"; "10001"; "01110" ]
  | '7' -> [ "11111"; "00001"; "00010"; "00100"; "01000"; "01000"; "01000" ]
  | '8' -> [ "01110"; "10001"; "10001"; "01110"; "10001"; "10001"; "01110" ]
  | '9' -> [ "01110"; "10001"; "10001"; "01111"; "00001"; "00001"; "01110" ]
  | ':' -> [ "00000"; "00100"; "00100"; "00000"; "00100"; "00100"; "00000" ]
  | ',' -> [ "00000"; "00000"; "00000"; "00000"; "00100"; "00100"; "01000" ]
  | '.' -> [ "00000"; "00000"; "00000"; "00000"; "00000"; "00100"; "00100" ]
  | '(' -> [ "00010"; "00100"; "01000"; "01000"; "01000"; "00100"; "00010" ]
  | ')' -> [ "01000"; "00100"; "00010"; "00010"; "00010"; "00100"; "01000" ]
  | '-' -> [ "00000"; "00000"; "00000"; "01110"; "00000"; "00000"; "00000" ]
  | '?' -> [ "01110"; "10001"; "00001"; "00010"; "00100"; "00000"; "00100" ]
  | ' ' -> [ "00000"; "00000"; "00000"; "00000"; "00000"; "00000"; "00000" ]
  | _ -> [ "11111"; "00001"; "00010"; "00100"; "00100"; "00000"; "00100" ]

let draw_glyph renderer color x y ch =
  let rows = glyph_rows ch in
  List.iteri
    (fun row pattern ->
      String.iteri
        (fun col pixel ->
          if pixel = '1' then
            fill_rect renderer color
              ~x:(x + (col * glyph_pixel))
              ~y:(y + (row * glyph_pixel))
              ~w:glyph_pixel ~h:glyph_pixel)
        pattern)
    rows

let draw_text renderer color ~x ~y text =
  String.iteri
    (fun index ch -> draw_glyph renderer color (x + (index * glyph_advance)) y ch)
    (String.uppercase_ascii text)

let words text =
  text |> String.split_on_char ' ' |> List.filter (fun word -> word <> "")

let wrap_text ~max_chars text =
  let finish_line current lines =
    match current with
    | "" -> lines
    | _ -> current :: lines
  in
  let append_word current word =
    if current = "" then word else current ^ " " ^ word
  in
  let rec loop current lines = function
    | [] -> List.rev (finish_line current lines)
    | word :: rest ->
        let candidate = append_word current word in
        if String.length candidate <= max_chars then loop candidate lines rest
        else if current = "" then
          let truncated =
            if String.length word <= max_chars then word
            else String.sub word 0 (max_chars - 1) ^ "."
          in
          loop "" (truncated :: lines) rest
        else loop word (current :: lines) rest
  in
  loop "" [] (words text)

let tail text max_chars =
  if max_chars <= 0 then ""
  else
    let len = String.length text in
    if len <= max_chars then text else String.sub text (len - max_chars) max_chars

let draw_info_panel renderer ~input ~status =
  let panel_x = margin in
  let panel_y = margin + board_pixels + 12 in
  let panel_w = board_pixels in
  let panel_h = info_height - 24 in
  let text_x = panel_x + 18 in
  let input_y = panel_y + 16 in
  let status_y = input_y + line_advance + 10 in
  let hint_y = status_y + (2 * line_advance) in
  let max_chars = max 1 ((panel_w - 36) / glyph_advance) in
  let input_text =
    if String.trim input = "" then "INPUT: TYPE A COMMAND" else "INPUT: " ^ input
  in
  let status_lines =
    let base =
      if String.trim status = "" then "STATUS: READY"
      else "STATUS: " ^ status
    in
    wrap_text ~max_chars base |> List.filteri (fun index _ -> index < 2)
  in
  fill_rect renderer (rgb 0x111827) ~x:panel_x ~y:panel_y ~w:panel_w ~h:panel_h;
  draw_outline renderer (rgb 0x3b4252) ~x:panel_x ~y:panel_y ~w:panel_w ~h:panel_h;
  draw_text renderer (rgb 0xe5e7eb) ~x:text_x ~y:input_y (tail input_text max_chars);
  List.iteri
    (fun index line ->
      draw_text renderer (rgb 0xcbd5e1) ~x:text_x
        ~y:(status_y + (index * line_advance)) line)
    status_lines;
  draw_text renderer (rgb 0x94a3b8) ~x:text_x ~y:hint_y
    "ENTER RUNS COMMAND. ESC QUITS."

let piece_colors = function
  | Colored (White, _) -> (rgb 0xf8f4e8, rgb 0x2b2d42)
  | Colored (Black, _) -> (rgb 0x202734, rgb 0xf8f4e8)
  | Neutral _ -> (rgb 0xc97830, rgb 0x3b1f0f)

let draw_base renderer fill outline =
  fill_rect renderer outline ~x:18 ~y:62 ~w:44 ~h:5;
  fill_rect renderer fill ~x:22 ~y:56 ~w:36 ~h:8;
  fill_rect renderer fill ~x:16 ~y:65 ~w:48 ~h:6

let draw_pawn renderer fill outline =
  fill_circle renderer fill ~cx:40 ~cy:25 ~radius:12;
  fill_circle renderer outline ~cx:40 ~cy:25 ~radius:13;
  fill_circle renderer fill ~cx:40 ~cy:25 ~radius:11;
  fill_rect renderer fill ~x:31 ~y:36 ~w:18 ~h:24;
  draw_base renderer fill outline

let draw_king renderer fill outline =
  draw_base renderer fill outline;
  fill_rect renderer fill ~x:29 ~y:28 ~w:22 ~h:32;
  fill_circle renderer fill ~cx:40 ~cy:31 ~radius:13;
  draw_line renderer outline 40 10 40 27;
  draw_line renderer outline 32 18 48 18

let draw_queen renderer fill outline =
  draw_base renderer fill outline;
  fill_rect renderer fill ~x:26 ~y:34 ~w:28 ~h:27;
  List.iter
    (fun (x, y) -> fill_circle renderer fill ~cx:x ~cy:y ~radius:8)
    [ (25, 25); (40, 17); (55, 25) ];
  draw_line renderer outline 25 25 31 42;
  draw_line renderer outline 40 17 40 42;
  draw_line renderer outline 55 25 49 42

let draw_rook renderer fill outline =
  draw_base renderer fill outline;
  fill_rect renderer fill ~x:25 ~y:25 ~w:30 ~h:35;
  List.iter
    (fun x -> fill_rect renderer fill ~x ~y:17 ~w:8 ~h:14)
    [ 23; 36; 49 ];
  fill_rect renderer outline ~x:24 ~y:31 ~w:32 ~h:4

let draw_bishop renderer fill outline =
  draw_base renderer fill outline;
  fill_circle renderer fill ~cx:40 ~cy:27 ~radius:15;
  fill_rect renderer fill ~x:31 ~y:34 ~w:18 ~h:28;
  draw_line renderer outline 47 16 34 35

let draw_knight renderer fill outline =
  draw_base renderer fill outline;
  fill_rect renderer fill ~x:28 ~y:34 ~w:21 ~h:28;
  fill_rect renderer fill ~x:30 ~y:19 ~w:28 ~h:18;
  fill_rect renderer fill ~x:23 ~y:25 ~w:16 ~h:14;
  draw_line renderer outline 53 22 61 15;
  draw_line renderer outline 29 35 22 44

let draw_camel renderer fill outline =
  fill_circle renderer fill ~cx:24 ~cy:44 ~radius:10;
  fill_rect renderer fill ~x:22 ~y:34 ~w:28 ~h:18;
  fill_circle renderer fill ~cx:32 ~cy:31 ~radius:9;
  fill_circle renderer fill ~cx:43 ~cy:30 ~radius:8;
  fill_rect renderer fill ~x:48 ~y:26 ~w:7 ~h:24;
  fill_circle renderer fill ~cx:59 ~cy:22 ~radius:7;
  fill_rect renderer fill ~x:58 ~y:21 ~w:9 ~h:5;
  fill_rect renderer fill ~x:55 ~y:14 ~w:3 ~h:6;
  fill_rect renderer fill ~x:60 ~y:13 ~w:3 ~h:5;
  List.iter
    (fun (x, y, h) -> fill_rect renderer fill ~x ~y ~w:4 ~h)
    [ (21, 50, 16); (29, 51, 15); (40, 50, 17); (48, 51, 15) ];
  List.iter
    (fun x -> fill_rect renderer outline ~x ~y:66 ~w:6 ~h:3)
    [ 20; 28; 39; 47 ];
  draw_line renderer outline 15 39 11 36;
  draw_line renderer outline 17 42 23 38;
  draw_line renderer outline 24 33 32 24;
  draw_line renderer outline 33 24 43 22;
  draw_line renderer outline 43 22 53 18;
  draw_line renderer outline 54 18 66 21;
  draw_line renderer outline 54 25 63 25;
  draw_line renderer outline 22 50 51 50

let draw_piece_shape renderer piece =
  let fill, outline = piece_colors piece in
  let draw =
    match piece with
    | Colored (_, King) -> draw_king
    | Colored (_, Queen) -> draw_queen
    | Colored (_, Rook) -> draw_rook
    | Colored (_, Bishop) -> draw_bishop
    | Colored (_, Knight) -> draw_knight
    | Colored (_, Pawn) -> draw_pawn
    | Colored (_, Camel) | Neutral Camel -> draw_camel
    | Neutral King -> draw_king
    | Neutral Queen -> draw_queen
    | Neutral Rook -> draw_rook
    | Neutral Bishop -> draw_bishop
    | Neutral Knight -> draw_knight
    | Neutral Pawn -> draw_pawn
  in
  draw renderer fill outline

let make_piece_texture renderer piece =
  let texture =
    sdl
      (Sdl.create_texture renderer Sdl.Pixel.format_rgba8888
         Sdl.Texture.access_target ~w:cell_size ~h:cell_size)
  in
  sdl (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_blend);
  sdl (Sdl.set_render_target renderer (Some texture));
  sdl (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);
  sdl (Sdl.set_render_draw_color renderer 0 0 0 0);
  sdl (Sdl.render_clear renderer);
  draw_piece_shape renderer piece;
  sdl (Sdl.set_render_target renderer None);
  texture

let pieces =
  [
    Colored (White, King);
    Colored (White, Queen);
    Colored (White, Rook);
    Colored (White, Bishop);
    Colored (White, Knight);
    Colored (White, Pawn);
    Colored (White, Camel);
    Colored (Black, King);
    Colored (Black, Queen);
    Colored (Black, Rook);
    Colored (Black, Bishop);
    Colored (Black, Knight);
    Colored (Black, Pawn);
    Colored (Black, Camel);
    Neutral Camel;
  ]

let create renderer =
  {
    renderer;
    textures = List.map (fun p -> (p, make_piece_texture renderer p)) pieces;
  }

let destroy view =
  List.iter (fun (_, texture) -> Sdl.destroy_texture texture) view.textures

let square_color row col =
  if (row + col) mod 2 = 0 then rgb 0xe7d8b1 else rgb 0x8b5e3c

let draw_target renderer x y occupied =
  if occupied then (
    fill_rect renderer (rgb 0x3f9b57) ~x:(x + 4) ~y:(y + 4) ~w:(cell_size - 8)
      ~h:5;
    fill_rect renderer (rgb 0x3f9b57) ~x:(x + 4)
      ~y:(y + cell_size - 9)
      ~w:(cell_size - 8) ~h:5;
    fill_rect renderer (rgb 0x3f9b57) ~x:(x + 4) ~y:(y + 4) ~w:5
      ~h:(cell_size - 8);
    fill_rect renderer (rgb 0x3f9b57)
      ~x:(x + cell_size - 9)
      ~y:(y + 4) ~w:5 ~h:(cell_size - 8))
  else
    fill_circle renderer (rgb 0x3f9b57)
      ~cx:(x + (cell_size / 2))
      ~cy:(y + (cell_size / 2))
      ~radius:9

let draw ?(input = "") ?(status = "") ?selected ?(targets = []) view board =
  let renderer = view.renderer in
  fill_rect renderer (rgb 0x1f2933) ~x:0 ~y:0 ~w:window_width ~h:window_height;
  for row = 0 to board_size - 1 do
    for col = 0 to board_size - 1 do
      let x = margin + (col * cell_size) in
      let y = margin + (row * cell_size) in
      let piece = get board row col in
      let color =
        if selected = Some (row, col) then rgb 0x77b77a
        else square_color row col
      in
      fill_rect renderer color ~x ~y ~w:cell_size ~h:cell_size;
      if List.mem (row, col) targets then
        draw_target renderer x y (piece <> None);
      match piece with
      | None -> ()
      | Some piece ->
          let texture = List.assoc piece view.textures in
          let dst = rect ~x ~y ~w:cell_size ~h:cell_size in
          sdl (Sdl.render_copy ~dst renderer texture)
    done
  done;
  draw_info_panel renderer ~input ~status;
  Sdl.render_present renderer
