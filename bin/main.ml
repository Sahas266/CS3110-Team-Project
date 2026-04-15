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

let handle_command st =
  let cmd = String.trim st.input in
  if cmd = "" then { st with status = "" }
  else
    match String.lowercase_ascii cmd with
    | "help" -> { st with status = "Available commands: help, clear" }
    | "clear" -> { st with status = "" }
    | _ -> { st with status = "Input received: " ^ cmd }

let rec event_loop term st =
  redraw term st;
  match Term.event term with
  | `Key (`Escape, _) -> ()
  | `Key (`Backspace, _) ->
      event_loop term { st with input = drop_last_char st.input }
  | `Key (`Enter, _) ->
      let updated = handle_command st in
      event_loop term { updated with input = "" }
  | `Key (`ASCII c, _) when Char.code c >= 32 && Char.code c <= 126 ->
      event_loop term { st with input = st.input ^ String.make 1 c }
  | _ -> event_loop term st

let () =
  let term = Term.create () in
  Fun.protect
    (fun () ->
      let board = Camel_chess.Board.initial () in
      let initial_state =
        { board; input = ""; status = "Type an input and press Enter." }
      in
      event_loop term initial_state)
    ~finally:(fun () -> Term.release term)
