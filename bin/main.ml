open Tsdl

type state = {
  board : Camel_chess.Board.t;
  input : string;
  status : string;
  selected : (int * int) option;
  targets : (int * int) list;
  turn : Camel_chess.Logic.turn;
}

let sdl context = function
  | Ok value -> value
  | Error (`Msg msg) -> failwith (context ^ ": " ^ msg)

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
  | (Camel_chess.Logic.Move _ | Camel_chess.Logic.Camel_move _) as cmd -> (
      match Camel_chess.Logic.is_valid_move st.board st.turn cmd with
      | Ok () ->
          Camel_chess.Logic.apply_move st.board st.turn cmd;
          let new_turn = Camel_chess.Logic.advance_turn st.turn in
          { st with
            turn = new_turn;
            selected = None;
            targets = [];
            status = Camel_chess.Logic.prompt_for new_turn }
      | Error msg ->
          { st with selected = None; targets = []; status = msg })
  | _ ->
      { st with
        selected = None;
        targets = [];
        status = Camel_chess.Logic.evaluate_input st.board st.input }

let clipped_title st =
  let input = if st.input = "" then "<type command>" else st.input in
  let title = Printf.sprintf "Camel Chess | Input: %s | %s" input st.status in
  if String.length title <= 180 then title else String.sub title 0 177 ^ "..."

let redraw window view st =
  Sdl.set_window_title window (clipped_title st);
  Camel_chess.Render.draw ~input:st.input ~status:st.status
    ~turn:(Camel_chess.Logic.turn_label st.turn) ?selected:st.selected
    ~targets:st.targets view st.board

let append_text st text =
  let keep_printable c = Char.code c >= 32 && Char.code c <= 126 in
  let chars = text |> String.to_seq |> Seq.filter keep_printable |> String.of_seq in
  { st with input = st.input ^ chars }

let report_command command status =
  let command = String.trim command in
  let status = String.trim status in
  if command <> "" then Printf.printf "> %s\n%!" command;
  if status <> "" then Printf.printf "%s\n%!" status

let handle_event event st =
  match Sdl.Event.(enum (get event typ)) with
  | `Quit -> (`Quit, st)
  | `Key_down -> (
      let key = Sdl.Event.(get event keyboard_keycode) in
      match key with
      | key when key = Sdl.K.escape -> (`Quit, st)
      | key when key = Sdl.K.backspace -> (`Continue, { st with input = drop_last_char st.input })
      | key when key = Sdl.K.return ->
          let updated = handle_command st in
          report_command st.input updated.status;
          (`Continue, { updated with input = "" })
      | _ -> (`Continue, st))
  | `Text_input ->
      let text = Sdl.Event.(get event text_input_text) in
      (`Continue, append_text st text)
  | _ -> (`Continue, st)

let rec pump_events event st =
  if Sdl.poll_event (Some event) then
    match handle_event event st with
    | `Quit, st -> (`Quit, st)
    | `Continue, st -> pump_events event st
  else (`Continue, st)

let rec event_loop window view event st =
  redraw window view st;
  match pump_events event st with
  | `Quit, _ -> ()
  | `Continue, st ->
      Sdl.delay 16l;
      event_loop window view event st

let main () =
  sdl "SDL init" (Sdl.init Sdl.Init.(video + events));
  let window_flags = Sdl.Window.(shown + resizable) in
  let window =
    sdl "Create window"
      (Sdl.create_window ~w:Camel_chess.Render.window_width
         ~h:Camel_chess.Render.window_height "Camel Chess" window_flags)
  in
  let renderer =
    sdl "Create renderer"
      (Sdl.create_renderer window
         ~flags:Sdl.Renderer.(accelerated + presentvsync + targettexture))
  in
  sdl "Set logical size"
    (Sdl.render_set_logical_size renderer Camel_chess.Render.window_width
       Camel_chess.Render.window_height);
  let view = Camel_chess.Render.create renderer in
  Sdl.start_text_input ();
  Fun.protect
    (fun () ->
      let board = Camel_chess.Board.initial () in
      let turn = Camel_chess.Logic.White_move in
      let initial_state =
        {
          board;
          input = "";
          status = Camel_chess.Logic.prompt_for turn;
          selected = None;
          targets = [];
          turn;
        }
      in
      Printf.printf "%s\n%!" initial_state.status;
      event_loop window view (Sdl.Event.create ()) initial_state)
    ~finally:(fun () ->
      Sdl.stop_text_input ();
      Camel_chess.Render.destroy view;
      Sdl.destroy_renderer renderer;
      Sdl.destroy_window window;
      Sdl.quit ())

let () = main ()
