open Notty_unix

let rec event_loop term =
  match Term.event term with
  | `Key (`ASCII 'q', _)
  | `Key (`ASCII 'Q', _)
  | `Key (`Escape, _) ->
      ()
  | _ -> event_loop term

let () =
  let term = Term.create () in
  Fun.protect
    (fun () ->
      let board = Camel_chess.Board.initial () in
      Term.image term (Camel_chess.Render.board_image board);
      event_loop term)
    ~finally:(fun () -> Term.release term)
