open Notty_unix

type state = {
  board : Camel_chess.Board.t;
  input : string;
  status : string;
}

let redraw term st =
  Term.image term
    (Camel_chess.Render.board_image ~input:st.input ~status:st.status st.board)

let drop_last_char s =
  let len = String.length s in
  if len = 0 then s else String.sub s 0 (len - 1)

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
      let row = Camel_chess.Board.board_size - rank_num in
      Some (row, col)

let parse_identify_command input =
  let words =
    input |> String.lowercase_ascii |> String.split_on_char ' '
    |> List.filter (fun w -> w <> "")
  in
  match words with
  | [ "identify"; coord ] -> parse_coordinate coord
  | _ -> None

let describe_square board row col =
  let open Camel_chess.Board in
  let coord =
    let file = Char.chr (Char.code 'a' + col) in
    let rank = string_of_int (board_size - row) in
    Char.escaped file ^ rank
  in
  match get board row col with
  | None -> coord ^ " is empty."
  | Some (Neutral Camel) -> coord ^ " contains a neutral camel."
  | Some (Neutral kind) ->
      let kind_name =
        match kind with
        | King -> "king"
        | Queen -> "queen"
        | Rook -> "rook"
        | Bishop -> "bishop"
        | Knight -> "knight"
        | Pawn -> "pawn"
        | Camel -> "camel"
      in
      coord ^ " contains a neutral " ^ kind_name ^ "."
  | Some (Colored (color, kind)) ->
      let color_name =
        match color with
        | White -> "white"
        | Black -> "black"
      in
      let kind_name =
        match kind with
        | King -> "king"
        | Queen -> "queen"
        | Rook -> "rook"
        | Bishop -> "bishop"
        | Knight -> "knight"
        | Pawn -> "pawn"
        | Camel -> "camel"
      in
      coord ^ " contains a " ^ color_name ^ " " ^ kind_name ^ "."

let handle_command st =
  let cmd = String.trim st.input in
  if cmd = "" then { st with status = "" }
  else
    match String.lowercase_ascii cmd with
    | "help" ->
        {
          st with
          status =
            "Available commands: help, clear, identify <coord> (e.g. identify \
             e2)";
        }
    | "clear" -> { st with status = "" }
    | _ -> (
        match parse_identify_command cmd with
        | Some (row, col) ->
            { st with status = describe_square st.board row col }
        | None ->
            {
              st with
              status = "Unknown input. Use help or try: identify <coord>";
            })

let rec event_loop term st =
  redraw term st;
  match Term.event term with
  | `Key (`Escape, _) -> ()
  | `Key (`ASCII c, mods)
    when Char.code c = 3 || (List.mem `Ctrl mods && Char.lowercase_ascii c = 'c')
    -> ()
  | `Key (`Backspace, _) ->
      event_loop term { st with input = drop_last_char st.input }
  | `Key (`Enter, _) ->
      let updated = handle_command st in
      event_loop term { updated with input = "" }
  | `Key (`ASCII c, _) when Char.code c >= 32 && Char.code c <= 126 ->
      event_loop term { st with input = st.input ^ String.make 1 c }
  | _ -> event_loop term st

let () =
  Sys.catch_break true;
  let term = Term.create () in
  Fun.protect
    (fun () ->
      let board = Camel_chess.Board.initial () in
      let initial_state =
        {
          board;
          input = "";
          status = "Type help or identify <coord>, then press Enter.";
        }
      in
      try event_loop term initial_state with Sys.Break -> ())
    ~finally:(fun () -> Term.release term)
