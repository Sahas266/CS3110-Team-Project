type command =
  | Help
  | Clear
  | Identify of (int * int)
  | Unknown

let parse_coordinate input =
  let s = String.trim (String.lowercase_ascii input) in
  if String.length s <> 2 then None
  else
    let file = s.[0] in
    let rank = s.[1] in
    if file < 'a' || file > 'h' || rank < '1' || rank > '8' then None
    else
      let col = Char.code file - Char.code 'a' in
      let rank_num = Char.code rank - Char.code '0' in
      let row = Board.board_size - rank_num in
      Some (row, col)

let parse_command input =
  let words =
    input |> String.lowercase_ascii |> String.split_on_char ' '
    |> List.filter (fun word -> word <> "")
  in
  match words with
  | [] -> Clear
  | [ "help" ] -> Help
  | [ "clear" ] -> Clear
  | [ "identify"; coord ] -> (
      match parse_coordinate coord with
      | Some square -> Identify square
      | None -> Unknown)
  | _ -> Unknown

let kind_name = function
  | Board.King -> "king"
  | Board.Queen -> "queen"
  | Board.Rook -> "rook"
  | Board.Bishop -> "bishop"
  | Board.Knight -> "knight"
  | Board.Pawn -> "pawn"
  | Board.Camel -> "camel"

let describe_square board row col =
  let coord =
    let file = Char.chr (Char.code 'a' + col) in
    let rank = string_of_int (Board.board_size - row) in
    Char.escaped file ^ rank
  in
  match Board.get board row col with
  | None -> coord ^ " is empty."
  | Some (Board.Neutral Board.Camel) -> coord ^ " contains a neutral camel."
  | Some (Board.Neutral kind) ->
      coord ^ " contains a neutral " ^ kind_name kind ^ "."
  | Some (Board.Colored (color, kind)) ->
      let color_name =
        match color with
        | Board.White -> "white"
        | Board.Black -> "black"
      in
      coord ^ " contains a " ^ color_name ^ " " ^ kind_name kind ^ "."

let evaluate_input board input =
  match parse_command input with
  | Help ->
      "Available commands: help, clear, identify <coord> (e.g. identify e2)"
  | Clear -> ""
  | Identify (row, col) -> describe_square board row col
  | Unknown -> "Unknown input. Use help or try: identify <coord>"
