open Board
open Tsdl

let cell_size = 80
let margin = 36
let info_height = 252
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
  | '[' -> [ "01110"; "01000"; "01000"; "01000"; "01000"; "01000"; "01110" ]
  | ']' -> [ "01110"; "00010"; "00010"; "00010"; "00010"; "00010"; "01110" ]
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

let draw_glyph_scaled renderer color x y ch ~pixel =
  List.iteri
    (fun row pattern ->
      String.iteri
        (fun col px ->
          if px = '1' then
            fill_rect renderer color
              ~x:(x + (col * pixel)) ~y:(y + (row * pixel))
              ~w:pixel ~h:pixel)
        pattern)
    (glyph_rows ch)

let draw_text_scaled renderer color ~x ~y ~pixel text =
  let advance = (glyph_width * pixel) + pixel in
  String.iteri
    (fun i ch -> draw_glyph_scaled renderer color (x + (i * advance)) y ch ~pixel)
    (String.uppercase_ascii text)

let text_w ~pixel text = ((glyph_width * pixel) + pixel) * String.length text
let center_text ~pixel text = (window_width - text_w ~pixel text) / 2

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

let default_hint = "ENTER RUNS COMMAND. ESC QUITS."

let draw_info_panel renderer ~input ~status ~turn ~check ~hint =
  let panel_x = margin in
  let panel_y = margin + board_pixels + margin + 12 in
  let panel_w = board_pixels in
  let panel_h = info_height - 24 in
  let text_x = panel_x + 18 in
  let turn_y = panel_y + 16 in
  let input_y = turn_y + line_advance + 10 in
  let status_y = input_y + line_advance + 10 in
  let check_y = status_y + (2 * line_advance) + 10 in
  let hint_y = check_y + line_advance + 10 in
  let max_chars = max 1 ((panel_w - 36) / glyph_advance) in
  let turn_text =
    if String.trim turn = "" then "TURN: ?" else "TURN: " ^ turn
  in
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
  draw_text renderer (rgb 0xfacc15) ~x:text_x ~y:turn_y (tail turn_text max_chars);
  draw_text renderer (rgb 0xe5e7eb) ~x:text_x ~y:input_y (tail input_text max_chars);
  List.iteri
    (fun index line ->
      draw_text renderer (rgb 0xcbd5e1) ~x:text_x
        ~y:(status_y + (index * line_advance)) line)
    status_lines;
  if String.trim check <> "" then
    draw_text renderer (rgb 0xef4444) ~x:text_x ~y:check_y (tail check max_chars);
  draw_text renderer (rgb 0x94a3b8) ~x:text_x ~y:hint_y (tail hint max_chars)

let piece_colors = function
  | Colored (White, _) -> (rgb 0xfffbf0, rgb 0x050505)
  | Colored (Black, _) -> (rgb 0x242424, rgb 0xfffbf0)
  | Neutral _ -> (rgb 0xc97830, rgb 0x3b1f0f)

let fill_circle_outline renderer fill outline ~cx ~cy ~radius =
  fill_circle renderer outline ~cx ~cy ~radius:(radius + 3);
  fill_circle renderer fill ~cx ~cy ~radius

let fill_rect_outline renderer fill outline ~x ~y ~w ~h =
  fill_rect renderer outline ~x:(x - 3) ~y:(y - 3) ~w:(w + 6) ~h:(h + 6);
  fill_rect renderer fill ~x ~y ~w ~h

let draw_base renderer fill outline =
  fill_rect renderer outline ~x:13 ~y:65 ~w:54 ~h:7;
  fill_rect renderer outline ~x:19 ~y:58 ~w:42 ~h:8;
  fill_rect renderer outline ~x:24 ~y:53 ~w:32 ~h:8;
  fill_rect renderer fill ~x:16 ~y:65 ~w:48 ~h:4;
  fill_rect renderer fill ~x:22 ~y:58 ~w:36 ~h:5;
  fill_rect renderer fill ~x:27 ~y:53 ~w:26 ~h:5

let draw_pawn renderer fill outline =
  fill_circle_outline renderer fill outline ~cx:40 ~cy:22 ~radius:11;
  fill_circle_outline renderer fill outline ~cx:40 ~cy:42 ~radius:14;
  fill_rect renderer fill ~x:29 ~y:39 ~w:22 ~h:19;
  draw_base renderer fill outline

let draw_king renderer fill outline =
  draw_base renderer fill outline;
  fill_rect_outline renderer fill outline ~x:29 ~y:33 ~w:22 ~h:25;
  fill_circle_outline renderer fill outline ~cx:40 ~cy:31 ~radius:14;
  fill_rect renderer outline ~x:37 ~y:8 ~w:6 ~h:19;
  fill_rect renderer outline ~x:31 ~y:14 ~w:18 ~h:6;
  fill_rect renderer fill ~x:39 ~y:10 ~w:2 ~h:16;
  fill_rect renderer fill ~x:33 ~y:16 ~w:14 ~h:2

let draw_queen renderer fill outline =
  draw_base renderer fill outline;
  fill_rect_outline renderer fill outline ~x:27 ~y:34 ~w:26 ~h:25;
  List.iter
    (fun (x, y) -> fill_circle_outline renderer fill outline ~cx:x ~cy:y ~radius:6)
    [ (21, 27); (31, 20); (40, 16); (49, 20); (59, 27) ];
  List.iter
    (fun (x1, y1, x2, y2) -> draw_line renderer outline x1 y1 x2 y2)
    [ (21, 27, 30, 43); (31, 20, 35, 43); (40, 16, 40, 43);
      (49, 20, 45, 43); (59, 27, 50, 43) ]

let draw_rook renderer fill outline =
  draw_base renderer fill outline;
  fill_rect_outline renderer fill outline ~x:24 ~y:29 ~w:32 ~h:30;
  fill_rect renderer outline ~x:20 ~y:18 ~w:40 ~h:14;
  List.iter
    (fun x -> fill_rect renderer fill ~x ~y:18 ~w:7 ~h:11)
    [ 23; 36; 50 ];
  fill_rect renderer fill ~x:21 ~y:25 ~w:38 ~h:6

let draw_bishop renderer fill outline =
  draw_base renderer fill outline;
  fill_circle_outline renderer fill outline ~cx:40 ~cy:27 ~radius:16;
  fill_rect_outline renderer fill outline ~x:31 ~y:36 ~w:18 ~h:23;
  draw_line renderer outline 49 15 34 38;
  draw_line renderer outline 50 16 35 39

let draw_knight renderer fill outline =
  draw_base renderer fill outline;
  fill_rect_outline renderer fill outline ~x:29 ~y:35 ~w:22 ~h:24;
  fill_circle_outline renderer fill outline ~cx:40 ~cy:31 ~radius:16;
  fill_rect renderer fill ~x:28 ~y:19 ~w:26 ~h:24;
  fill_rect renderer outline ~x:24 ~y:18 ~w:9 ~h:18;
  fill_rect renderer outline ~x:52 ~y:22 ~w:8 ~h:6;
  draw_line renderer outline 34 13 29 22;
  draw_line renderer outline 34 13 45 20;
  draw_line renderer outline 45 20 58 23;
  draw_line renderer outline 30 38 22 48

let chess_glyph_masks =
  [
    (King, [| 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000380000000L; 0x00000003c0000000L; 0x00000003c0000000L; 0x00000007e0000000L; 0x0000003ffc000000L; 0x0000003ffc000000L; 0x00000003c0000000L; 0x00000003c0000000L; 0x00000007e0000000L; 0x0000000ff0000000L; 0x0000001c38000000L; 0x0000003818000000L; 0x0000fff81fff0000L; 0x0003fff81fffc000L; 0x000ffff81fffe000L; 0x001ffffc3ffff800L; 0x003f807ffc01fc00L; 0x007e3f1ff8fc7c00L; 0x007cffcfe3ff3e00L; 0x00f9ffe7e7ff9f00L; 0x00f3fff7efffdf00L; 0x01f7fff7efffcf00L; 0x01e7fff7efffef80L; 0x01effff7efffe780L; 0x03effff7effff780L; 0x03cffff7effff780L; 0x03cffff7effff780L; 0x03effff7effff780L; 0x03effff7effff780L; 0x01effff7efffe780L; 0x01e7fff7efffef80L; 0x01f7fff7efffcf00L; 0x00f3fff7efff9f00L; 0x00f9fff7efff3e00L; 0x007c7ff7effe7e00L; 0x007f0007e000fc00L; 0x003ffffffffff800L; 0x001ffffffffff800L; 0x000fffffffffe000L; 0x0003ffffffffc000L; 0x0001e00000070000L; 0x0001e7ffffe70000L; 0x0001effffff70000L; 0x0001effffff70000L; 0x0001effffff70000L; 0x0001effffff70000L; 0x0001effffff70000L; 0x0001effffff70000L; 0x0001efffffe70000L; 0x0001e00000070000L; 0x0001ffffffff0000L; 0x0001ffffffff0000L; 0x0001ffffffff0000L; 0x0000ffffffff0000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L |]);
    (Queen, [| 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x00000003e0000000L; 0x00000007f0000000L; 0x00001c0ff03c0000L; 0x00007f0ff87e0000L; 0x00007f0ff0ff0000L; 0x0000ff87f0ff0000L; 0x0000ff81c0ff0000L; 0x00007f01c0ff0000L; 0x00003f01c07e0000L; 0x00181e01c07c1c00L; 0x007e0e03c0783f00L; 0x00ff0f03c0787f00L; 0x00ff0f03c0707f80L; 0x00ff0f03c0f07f80L; 0x00ff0f03c0f07f80L; 0x007e0f83e0f03f00L; 0x003c0f83e0f03c00L; 0x000c0783e1f07800L; 0x000e0783e1f07000L; 0x000e07c3e1e0f000L; 0x000707c3e1e0f000L; 0x000707c3e3e1e000L; 0x000787c3e3e1e000L; 0x000387e3e3e3e000L; 0x0003c7e3e3e3c000L; 0x0003c7e3e7e7c000L; 0x0001e7e3e7e78000L; 0x0001e7f3e7cf8000L; 0x0001f7f3e7cf8000L; 0x0000f7f7efdf0000L; 0x0000fff7efdf0000L; 0x0000fbffefff0000L; 0x0000ffffeffe0000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x0000700000060000L; 0x0000700000060000L; 0x0000700000060000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x0000700000060000L; 0x0000700000060000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x00007ffffffe0000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L |]);
    (Rook, [| 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x000ff81ff81ff800L; 0x000ffc1ff81ff800L; 0x000ffc1ff81ff800L; 0x000ffc1ff81ff800L; 0x000ffc1ff81ff800L; 0x000ffc1ff81ff800L; 0x0000ffffffff8000L; 0x0000ffffffff8000L; 0x0000ffffffff8000L; 0x0000f800001f8000L; 0x0000f800001f8000L; 0x0000f800001f8000L; 0x0000ffffffff0000L; 0x00007ffffffe0000L; 0x00003ffffffe0000L; 0x00003ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00001ffffffc0000L; 0x00003ffffffc0000L; 0x00003ffffffe0000L; 0x00007fffffff0000L; 0x0000ffffffff0000L; 0x0000ffffffff8000L; 0x0000e00000038000L; 0x0000e00000038000L; 0x0000e00000038000L; 0x0000ffffffff8000L; 0x000ffffffffff800L; 0x000ffffffffff800L; 0x000c000000003800L; 0x000c000000003800L; 0x000ffffffffff800L; 0x000ffffffffff800L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L |]);
    (Bishop, [| 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000080000000L; 0x00000007f0000000L; 0x0000000ff8000000L; 0x0000001ff8000000L; 0x0000001ffc000000L; 0x0000003ffc000000L; 0x0000003ffc000000L; 0x0000003ffc000000L; 0x0000001ffc000000L; 0x0000001ff8000000L; 0x0000000ff0000000L; 0x0000001ffc000000L; 0x0000007ffe000000L; 0x000000ffff000000L; 0x000001ffff800000L; 0x000003fc3fc00000L; 0x000003fc3fc00000L; 0x000007fc3fe00000L; 0x000007fc3fe00000L; 0x000007fc1fe00000L; 0x0000070000f00000L; 0x0000070000f00000L; 0x0000070000f00000L; 0x000007fc3fe00000L; 0x000007fc3fe00000L; 0x000003fc3fe00000L; 0x000003fc3fc00000L; 0x000001ffffc00000L; 0x000001ffff800000L; 0x000000ffff000000L; 0x0000007ffe000000L; 0x0000007fff000000L; 0x000000ffff000000L; 0x000000ffff000000L; 0x000000ffff000000L; 0x000000ffff800000L; 0x000000ffff000000L; 0x000000ffff000000L; 0x000000ffff000000L; 0x000000ffff000000L; 0x000000ffff000000L; 0x000001ffff800000L; 0x000001ffffc00000L; 0x000003ffffc00000L; 0x000007ffffe00000L; 0x000007fffff00000L; 0x00000ffffff00000L; 0x00001ffffff80000L; 0x00003ffffffc0000L; 0x00003ffffffc0000L; 0x00007ffffffe0000L; 0x0000ffffffff0000L; 0x0000ffffffff8000L; 0x0000ffffffff8000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L |]);
    (Knight, [| 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000600000000L; 0x0000000f00000000L; 0x0000000f80000000L; 0x0000180f80000000L; 0x00003c0fc0000000L; 0x00003e07c0000000L; 0x00003f07c0000000L; 0x00003f8fc0000000L; 0x00001fff80000000L; 0x00000fffc0000000L; 0x00000ffff0000000L; 0x00001ffff8000000L; 0x00003fffbc000000L; 0x00007fffde000000L; 0x0000ffffe7000000L; 0x0000f1fff3800000L; 0x0001e3fff9c00000L; 0x0001e3fffce00000L; 0x0003e7fffee00000L; 0x0003ffffff700000L; 0x0003ffffffb80000L; 0x0003ffffffb80000L; 0x0003ffffffdc0000L; 0x0003ffffffcc0000L; 0x0003ffffffee0000L; 0x0003ffffffe60000L; 0x0003fffffff70000L; 0x0003fffffff30000L; 0x0007fffffffb8000L; 0x0007fffffffb8000L; 0x0007fffffff98000L; 0x0007fffefff9c000L; 0x000f3ffdfffdc000L; 0x000e3ffbfffcc000L; 0x000e7fe7fffcc000L; 0x000fff8ffffce000L; 0x000fff1ffffee000L; 0x0003df3ffffee000L; 0x00009cfffffe6000L; 0x000011fffffe7000L; 0x000003fffffe7000L; 0x000007fffffe7000L; 0x00000ffffffe7000L; 0x00000fffffff7000L; 0x00001fffffff3000L; 0x00001fffffff3800L; 0x00003fffffff3800L; 0x00003fffffff3800L; 0x00003fffffff3800L; 0x00003fffffff3800L; 0x00003ffffffff800L; 0x00003ffffffff800L; 0x00001ffffffff800L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L |]);
    (Pawn, [| 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x00000001c0000000L; 0x00000007f0000000L; 0x0000000ff8000000L; 0x0000001ffc000000L; 0x0000001ffc000000L; 0x0000003ffc000000L; 0x0000003ffc000000L; 0x0000003ffc000000L; 0x0000001ffc000000L; 0x0000001ff8000000L; 0x0000000ff0000000L; 0x0000003ffc000000L; 0x0000007fff000000L; 0x000000ffff800000L; 0x000001ffff800000L; 0x000003ffffc00000L; 0x000003ffffe00000L; 0x000007ffffe00000L; 0x000007ffffe00000L; 0x000007fffff00000L; 0x000007fffff00000L; 0x000007fffff00000L; 0x000007fffff00000L; 0x000007ffffe00000L; 0x000007ffffe00000L; 0x000003ffffe00000L; 0x000003ffffc00000L; 0x000001ffffc00000L; 0x000000ffff800000L; 0x000001ffffc00000L; 0x000007fffff00000L; 0x00001ffffff80000L; 0x00003ffffffc0000L; 0x00007fffffff0000L; 0x0000ffffffff8000L; 0x0001ffffffff8000L; 0x0003ffffffffc000L; 0x0003ffffffffe000L; 0x0007ffffffffe000L; 0x0007fffffffff000L; 0x000ffffffffff000L; 0x000ffffffffff800L; 0x001ffffffffff800L; 0x001ffffffffff800L; 0x001ffffffffffc00L; 0x001ffffffffffc00L; 0x001ffffffffffc00L; 0x003ffffffffffc00L; 0x003ffffffffffc00L; 0x003ffffffffffc00L; 0x003ffffffffffc00L; 0x001ffffffffffc00L; 0x001ffffffffffc00L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L; 0x0000000000000000L |]);
  ]

let mask_has mask row col =
  row >= 0 && row < Array.length mask && col >= 0 && col < 64
  && Int64.logand
       (Int64.shift_right_logical mask.(row) (63 - col))
       1L
     = 1L

let draw_mask renderer color mask ~x ~y ~grow =
  for row = 0 to 63 do
    for col = 0 to 63 do
      if mask_has mask row col then
        fill_rect renderer color ~x:(x + col - grow) ~y:(y + row - grow)
          ~w:((2 * grow) + 1) ~h:((2 * grow) + 1)
    done
  done

let draw_glyph_piece renderer kind fill outline =
  let mask = List.assoc kind chess_glyph_masks in
  draw_mask renderer outline mask ~x:8 ~y:8 ~grow:2;
  draw_mask renderer fill mask ~x:8 ~y:8 ~grow:0

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
  match piece with
  | Colored (_, Camel) | Neutral Camel -> draw_camel renderer fill outline
  | Colored (_, kind) | Neutral kind -> draw_glyph_piece renderer kind fill outline

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

let label_color = rgb 0xe5e7eb

let draw_coordinate_labels renderer ~flipped =
  let glyph_w = glyph_width * glyph_pixel in
  let glyph_h = glyph_height * glyph_pixel in
  let file_x col = margin + (col * cell_size) + ((cell_size - glyph_w) / 2) in
  let rank_y row = margin + (row * cell_size) + ((cell_size - glyph_h) / 2) in
  let top_y = (margin - glyph_h) / 2 in
  let bottom_y = margin + board_pixels + ((margin - glyph_h) / 2) in
  let left_x = (margin - glyph_w) / 2 in
  let right_x = margin + board_pixels + ((margin - glyph_w) / 2) in
  for col = 0 to board_size - 1 do
    let lc = if flipped then board_size - 1 - col else col in
    let ch = Char.chr (Char.code 'A' + lc) in
    draw_glyph renderer label_color (file_x col) top_y ch;
    draw_glyph renderer label_color (file_x col) bottom_y ch
  done;
  for row = 0 to board_size - 1 do
    let lr = if flipped then board_size - 1 - row else row in
    let ch = Char.chr (Char.code '0' + (board_size - lr)) in
    draw_glyph renderer label_color left_x (rank_y row) ch;
    draw_glyph renderer label_color right_x (rank_y row) ch
  done

let draw ?(input = "") ?(status = "") ?(turn = "") ?(check = "")
    ?(check_squares = []) ?(hint = default_hint) ?selected ?(targets = [])
    ?(flipped = false) view board =
  let renderer = view.renderer in
  fill_rect renderer (rgb 0x1f2933) ~x:0 ~y:0 ~w:window_width ~h:window_height;
  draw_coordinate_labels renderer ~flipped;
  for row = 0 to board_size - 1 do
    for col = 0 to board_size - 1 do
      let x = margin + (if flipped then board_size - 1 - col else col) * cell_size in
      let y = margin + (if flipped then board_size - 1 - row else row) * cell_size in
      let piece = get board row col in
      let color =
        if selected = Some (row, col) then rgb 0x77b77a
        else if List.mem (row, col) check_squares then rgb 0xc24a4a
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
  draw_info_panel renderer ~input ~status ~turn ~check ~hint;
  Sdl.render_present renderer

let draw_menu_panel renderer ~input ~status ~hint =
  let panel_x = margin in
  let panel_y = margin + board_pixels + margin + 12 in
  let panel_w = board_pixels in
  let panel_h = info_height - 24 in
  let text_x = panel_x + 18 in
  let max_chars = max 1 ((panel_w - 36) / glyph_advance) in
  fill_rect renderer (rgb 0x111827) ~x:panel_x ~y:panel_y ~w:panel_w ~h:panel_h;
  draw_outline renderer (rgb 0x3b4252) ~x:panel_x ~y:panel_y ~w:panel_w ~h:panel_h;
  let input_text =
    if String.trim input = "" then "INPUT: TYPE AN OPTION"
    else "INPUT: " ^ input
  in
  draw_text renderer (rgb 0xe5e7eb) ~x:text_x ~y:(panel_y + 16) (tail input_text max_chars);
  (if String.trim status <> "" then
    List.iteri
      (fun i line ->
        draw_text renderer (rgb 0xcbd5e1) ~x:text_x
          ~y:(panel_y + 16 + line_advance + 10 + (i * line_advance)) line)
      (wrap_text ~max_chars ("STATUS: " ^ status) |> List.filteri (fun i _ -> i < 2)));
  draw_text renderer (rgb 0x94a3b8) ~x:text_x
    ~y:(panel_y + 16 + (3 * line_advance) + 20) (tail hint max_chars)

let draw_title ?(input = "") ?(status = "") view =
  let renderer = view.renderer in
  fill_rect renderer (rgb 0x1f2933) ~x:0 ~y:0 ~w:window_width ~h:window_height;
  draw_text_scaled renderer (rgb 0xfbbf24)
    ~x:(center_text ~pixel:6 "CAMEL CHESS") ~y:100 ~pixel:6 "CAMEL CHESS";
  draw_text_scaled renderer (rgb 0x94a3b8)
    ~x:(center_text ~pixel:3 "BY THE BARD RACCOONS") ~y:168 ~pixel:3 "BY THE BARD RACCOONS";
  fill_rect renderer (rgb 0x3b4252)
    ~x:margin ~y:204 ~w:(window_width - (2 * margin)) ~h:2;
  Array.iteri
    (fun i text ->
      draw_text_scaled renderer (rgb 0xe5e7eb)
        ~x:(center_text ~pixel:4 text) ~y:(300 + (i * 80)) ~pixel:4 text)
    [| "1. SINGLEPLAYER"; "2. MULTIPLAYER"; "3. RULES" |];
  draw_menu_panel renderer ~input ~status ~hint:"ENTER SELECTS.  ESC QUITS.";
  Sdl.render_present renderer

let draw_rules view =
  let renderer = view.renderer in
  fill_rect renderer (rgb 0x1f2933) ~x:0 ~y:0 ~w:window_width ~h:window_height;
  draw_text_scaled renderer (rgb 0xfbbf24)
    ~x:(center_text ~pixel:5 "RULES") ~y:60 ~pixel:5 "RULES";
  fill_rect renderer (rgb 0x3b4252)
    ~x:margin ~y:122 ~w:(window_width - (2 * margin)) ~h:2;
  let text_x = margin + 18 in
  let max_chars = max 1 ((window_width - (2 * margin) - 36) / glyph_advance) in
  let max_y = margin + board_pixels - line_advance in
  let y = ref 140 in
  let put line =
    if !y <= max_y then begin
      draw_text renderer (rgb 0xe5e7eb) ~x:text_x ~y:!y line;
      y := !y + line_advance
    end
  in
  List.iter
    (fun para ->
      if para = "" then y := !y + (line_advance / 2)
      else List.iter put (wrap_text ~max_chars para))
    [ "CAMEL CHESS IS A PARODY OF DUCK CHESS. IN DUCK CHESS, A RUBBER DUCK SITS ON THE BOARD AS AN INVIOLABLE OBSTACLE. HERE, THE DUCK IS REPLACED BY A NEUTRAL CAMEL.";
      "";
      "ON EACH TURN: MOVE ONE OF YOUR PIECES, THEN MOVE THE CAMEL TO ANY EMPTY SQUARE. THE CAMEL BLOCKS ALL MOVEMENT AND CANNOT BE CAPTURED. THE GAME ENDS WHEN A KING IS TAKEN." ];
  y := !y + (line_advance / 2);
  draw_text renderer (rgb 0xfbbf24) ~x:text_x ~y:!y "COMMANDS:";
  y := !y + line_advance + 4;
  let commands = [
    ("MOVE [FROM] [TO]", Some "MOVE E2 E4");
    ("MOVE [TO]",        Some "MOVE E4");
    ("[COORD]",          Some "E2");
    ("IDENTIFY [COORD]", Some "IDENTIFY E2");
    ("HELP",             None);
    ("CLEAR",            None);
  ] in
  let cmd_x = text_x + 24 in
  let max_syn = List.fold_left (fun a (s, _) -> max a (String.length s)) 0 commands in
  let ex_x = cmd_x + (max_syn + 2) * glyph_advance in
  List.iter
    (fun (syntax, example) ->
      if !y <= max_y then begin
        draw_text renderer (rgb 0xe5e7eb) ~x:cmd_x ~y:!y syntax;
        (match example with
         | None    -> ()
         | Some ex -> draw_text renderer (rgb 0x22d3ee) ~x:ex_x ~y:!y ex);
        y := !y + line_advance
      end)
    commands;
  draw_menu_panel renderer ~input:"" ~status:"" ~hint:"PRESS ENTER TO RETURN TO TITLE.";
  Sdl.render_present renderer

let draw_multiplayer_menu ?(input = "") ?(status = "") view =
  let renderer = view.renderer in
  fill_rect renderer (rgb 0x1f2933) ~x:0 ~y:0 ~w:window_width ~h:window_height;
  draw_text_scaled renderer (rgb 0xfbbf24)
    ~x:(center_text ~pixel:5 "MULTIPLAYER") ~y:80 ~pixel:5 "MULTIPLAYER";
  fill_rect renderer (rgb 0x3b4252)
    ~x:margin ~y:150 ~w:(window_width - (2 * margin)) ~h:2;
  let text_x = margin + 18 in
  let max_chars = max 1 ((window_width - (2 * margin) - 54) / glyph_advance) in
  let y = ref 200 in
  List.iter
    (fun (heading, desc) ->
      draw_text_scaled renderer (rgb 0xe5e7eb) ~x:text_x ~y:!y ~pixel:4 heading;
      y := !y + 44;
      List.iter
        (fun line ->
          draw_text renderer (rgb 0x94a3b8) ~x:(text_x + 24) ~y:!y line;
          y := !y + line_advance)
        (wrap_text ~max_chars desc);
      y := !y + 36)
    [ ("1. HOST GAME",  "YOU HOST. SHARE YOUR IP AND PORT WITH YOUR OPPONENT.");
      ("2. JOIN GAME",  "ENTER YOUR OPPONENT'S IP ADDRESS TO CONNECT.") ];
  draw_menu_panel renderer ~input ~status ~hint:"ENTER SELECTS.  ESC GOES BACK.";
  Sdl.render_present renderer
