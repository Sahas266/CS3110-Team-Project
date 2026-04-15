open Board
open Notty

let rgb_hex hex =
  A.rgb_888
    ~r:((hex lsr 16) land 0xff)
    ~g:((hex lsr 8) land 0xff)
    ~b:(hex land 0xff)

let piece_label = function
  | None -> "."
  | Some (Neutral Camel) -> "C"
  | Some (Neutral _) -> "?"
  | Some (Colored (White, King)) -> "K"
  | Some (Colored (White, Queen)) -> "Q"
  | Some (Colored (White, Rook)) -> "R"
  | Some (Colored (White, Bishop)) -> "B"
  | Some (Colored (White, Knight)) -> "N"
  | Some (Colored (White, Pawn)) -> "P"
  | Some (Colored (White, Camel)) -> "C"
  | Some (Colored (Black, King)) -> "k"
  | Some (Colored (Black, Queen)) -> "q"
  | Some (Colored (Black, Rook)) -> "r"
  | Some (Colored (Black, Bishop)) -> "b"
  | Some (Colored (Black, Knight)) -> "n"
  | Some (Colored (Black, Pawn)) -> "p"
  | Some (Colored (Black, Camel)) -> "c"

let piece_attr = function
  | None -> A.empty
  | Some (Neutral Camel) -> A.(fg (rgb_hex 0xcc6600) ++ st bold)
  | Some (Neutral _) -> A.(fg lightred ++ st bold)
  | Some (Colored (White, _)) -> A.(fg (rgb_hex 0x1d3557) ++ st bold)
  | Some (Colored (Black, _)) -> A.(fg (rgb_hex 0xf1faee) ++ st bold)

let square_attr row col =
  if (row + col) mod 2 = 0 then A.bg (rgb_hex 0xe9d8a6)
  else A.bg (rgb_hex 0x99582a)

let make_cell row col piece =
  let background = square_attr row col in
  I.string A.(background ++ piece_attr piece) (Printf.sprintf " %s " (piece_label piece))

let rank_label row =
  let text = string_of_int (board_size - row) in
  I.string A.(fg lightwhite ++ st bold) (text ^ " ")

let file_label col =
  let ch = Char.chr (Char.code 'a' + col) |> Char.escaped in
  I.string A.(fg lightwhite ++ st bold) (" " ^ ch ^ " ")

let row_image board row =
  let cells = List.init board_size (fun col -> make_cell row col (get board row col)) in
  I.hcat (rank_label row :: cells)

let file_labels =
  let left_margin = I.void 2 1 in
  let labels = List.init board_size file_label |> I.hcat in
  I.hcat [ left_margin; labels ]

let banner =
  I.string A.(fg lightcyan ++ st bold)
    "Camel Chess  White=uppercase  Black=lowercase  Camel=C  q quits"

let board_image board =
  let rows = List.init board_size (row_image board) |> I.vcat in
  I.vcat [ banner; rows; file_labels ]
