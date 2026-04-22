open Notty_unix

type state = {
  board : Camel_chess.Board.t;
  input : string;
  status : string;
  selected : (int * int) option;
  targets : (int * int) list;
}

let redraw term st =
  Term.image term
    (Camel_chess.Render.board_image ~input:st.input ~status:st.status
       ~selected:st.selected ~targets:st.targets st.board)

let drop_last_char s =
  let len = String.length s in
  if len = 0 then s else String.sub s 0 (len - 1)

let handle_command st =
  match Camel_chess.Logic.parse_command st.input with
  | Camel_chess.Logic.Valid (row, col) ->
      let targets = Camel_chess.Logic.valid_moves st.board row col in
      { st with
        selected = Some (row, col);
        targets;
        status = Camel_chess.Logic.describe_valid st.board row col targets }
  | _ ->
      { st with
        selected = None;
        targets = [];
        status = Camel_chess.Logic.evaluate_input st.board st.input }

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
          status = "Type help, identify <coord>, or valid <coord>, then press Enter.";
          selected = None;
          targets = [];
        }
      in
      try event_loop term initial_state with Sys.Break -> ())
    ~finally:(fun () -> Term.release term)
