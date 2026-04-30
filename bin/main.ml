open Tsdl

let sdl context = function
  | Ok value -> value
  | Error (`Msg msg) -> failwith (context ^ ": " ^ msg)

let drop_last_char s =
  let len = String.length s in
  if len = 0 then s else String.sub s 0 (len - 1)

(* ── Game state ─────────────────────────────────────────────────────────── *)

type game_mode = Solo | Host | Client

type game_state = {
  board         : Camel_chess.Board.t;
  input         : string;
  status        : string;
  selected      : (int * int) option;
  targets       : (int * int) list;
  turn          : Camel_chess.Logic.turn;
  mode          : game_mode;
  check         : string;
  check_squares : (int * int) list;
  winner        : Camel_chess.Board.color option;
}

(* ── Screen ─────────────────────────────────────────────────────────────── *)

type screen =
  | Title          of { input : string; status : string }
  | Rules
  | Multiplayer_menu of { input : string; status : string }
  | Game           of game_state

(* ── Helpers ─────────────────────────────────────────────────────────────── *)

let checkmate_message = function
  | Camel_chess.Board.White -> "CHECKMATE - WHITE WINS!"
  | Camel_chess.Board.Black -> "CHECKMATE - BLACK WINS!"

let detect_winner board =
  let module L = Camel_chess.Logic in
  let module B = Camel_chess.Board in
  match L.find_king board B.White, L.find_king board B.Black with
  | None, Some _ -> Some B.Black
  | Some _, None -> Some B.White
  | _            -> None

let compute_check board =
  let w, b = Camel_chess.Logic.checks board in
  let squares = List.filter_map Fun.id [ w; b ] in
  let msg = match w, b with
    | Some _, Some _ -> "Check on White and Black!"
    | Some _, None   -> "Check on White!"
    | None, Some _   -> "Check on Black!"
    | None, None     -> ""
  in
  (msg, squares)

let new_game mode =
  let board = Camel_chess.Board.initial () in
  let turn  = Camel_chess.Logic.White_move in
  { board; input = ""; status = Camel_chess.Logic.prompt_for turn;
    selected = None; targets = []; turn; mode;
    check = ""; check_squares = []; winner = None }

let is_flipped gs =
  match gs.mode with
  | Solo   -> Camel_chess.Logic.turn_color gs.turn = Camel_chess.Board.Black
  | Host   -> false
  | Client -> true

let handle_game_command gs =
  match Camel_chess.Logic.parse_command gs.input with
  | Camel_chess.Logic.Valid (row, col) ->
      let targets = Camel_chess.Logic.valid_moves gs.board row col in
      { gs with selected = Some (row, col); targets;
                status = Camel_chess.Logic.describe_valid gs.board row col targets }
  | (Camel_chess.Logic.Move _ | Camel_chess.Logic.Camel_move _) as cmd ->
      (match Camel_chess.Logic.is_valid_move gs.board gs.turn cmd with
       | Ok () ->
           Camel_chess.Logic.apply_move gs.board gs.turn cmd;
           (match detect_winner gs.board with
            | Some color ->
                { gs with selected = None; targets = []; status = "";
                          check = checkmate_message color;
                          check_squares = []; winner = Some color }
            | None ->
                let new_turn = Camel_chess.Logic.advance_turn gs.turn in
                let check, check_squares = compute_check gs.board in
                { gs with turn = new_turn; selected = None; targets = [];
                          status = Camel_chess.Logic.prompt_for new_turn;
                          check; check_squares })
       | Error msg ->
           { gs with selected = None; targets = []; status = msg })
  | _ ->
      { gs with selected = None; targets = [];
                status = Camel_chess.Logic.evaluate_input gs.board gs.input }

let report cmd status =
  if String.trim cmd    <> "" then Printf.printf "> %s\n%!" cmd;
  if String.trim status <> "" then Printf.printf "%s\n%!" status

let append_text_screen screen text =
  let keep c = Char.code c >= 32 && Char.code c <= 126 in
  let s = text |> String.to_seq |> Seq.filter keep |> String.of_seq in
  if s = "" then screen
  else match screen with
  | Title { input; status }            -> Title { input = input ^ s; status }
  | Multiplayer_menu { input; status } -> Multiplayer_menu { input = input ^ s; status }
  | Game gs                            -> Game { gs with input = gs.input ^ s }
  | other                              -> other

(* ── Key handling ────────────────────────────────────────────────────────── *)

let handle_key key screen =
  if key = Sdl.K.backspace then
    (match screen with
     | Title { input; status }            -> Title { input = drop_last_char input; status }
     | Multiplayer_menu { input; status } -> Multiplayer_menu { input = drop_last_char input; status }
     | Game gs                            -> Game { gs with input = drop_last_char gs.input }
     | Rules                              -> Rules)
  else if key = Sdl.K.return then
    (match screen with
     | Title { input; _ } ->
         (match String.trim (String.lowercase_ascii input) with
          | "1" | "singleplayer" -> Game (new_game Solo)
          | "2" | "multiplayer"  -> Multiplayer_menu { input = ""; status = "" }
          | "3" | "rules"        -> Rules
          | _ -> Title { input = ""; status = "TYPE 1, 2, OR 3 AND PRESS ENTER." })
     | Rules -> Title { input = ""; status = "" }
     | Multiplayer_menu { input; _ } ->
         (match String.trim (String.lowercase_ascii input) with
          | "1" | "host"             ->
              Multiplayer_menu { input = ""; status = "HOST MODE COMING SOON." }
          | "2" | "join" | "connect" ->
              Multiplayer_menu { input = ""; status = "JOIN MODE COMING SOON." }
          | _ ->
              Multiplayer_menu { input = ""; status = "TYPE 1 OR 2 AND PRESS ENTER." })
     | Game gs ->
         let updated = handle_game_command gs in
         report gs.input updated.status;
         Game { updated with input = "" })
  else screen

(* ── Event loop ─────────────────────────────────────────────────────────── *)

let handle_event event screen : [ `Quit | `Continue of screen ] =
  match Sdl.Event.(enum (get event typ)) with
  | `Quit -> `Quit
  | `Text_input ->
      let text = Sdl.Event.(get event text_input_text) in
      `Continue (append_text_screen screen text)
  | `Key_down ->
      let key = Sdl.Event.(get event keyboard_keycode) in
      if key = Sdl.K.escape then
        (match screen with
         | Title _ | Game _ -> `Quit
         | Rules | Multiplayer_menu _ ->
             `Continue (Title { input = ""; status = "" }))
      else if key = Sdl.K.return then
        (match screen with
         | Game gs when gs.winner <> None ->
             (match String.trim (String.lowercase_ascii gs.input) with
              | "exit" -> `Quit
              | "restart" ->
                  let fresh = new_game gs.mode in
                  report gs.input fresh.status;
                  `Continue (Game fresh)
              | _ ->
                  let msg = "Game over. Type EXIT or RESTART." in
                  report gs.input msg;
                  `Continue (Game { gs with input = ""; status = msg }))
         | _ -> `Continue (handle_key key screen))
      else
        `Continue (handle_key key screen)
  | _ -> `Continue screen

let clipped_title gs =
  let lbl = Camel_chess.Logic.turn_label gs.turn in
  let inp = if gs.input = "" then "<type command>" else gs.input in
  let s = Printf.sprintf "Camel Chess | %s | Input: %s | %s" lbl inp gs.status in
  if String.length s <= 180 then s else String.sub s 0 177 ^ "..."

let redraw window view screen =
  match screen with
  | Title { input; status } ->
      Sdl.set_window_title window "Camel Chess";
      Camel_chess.Render.draw_title ~input ~status view
  | Rules ->
      Sdl.set_window_title window "Camel Chess - Rules";
      Camel_chess.Render.draw_rules view
  | Multiplayer_menu { input; status } ->
      Sdl.set_window_title window "Camel Chess - Multiplayer";
      Camel_chess.Render.draw_multiplayer_menu ~input ~status view
  | Game gs ->
      Sdl.set_window_title window (clipped_title gs);
      let hint =
        if gs.winner <> None then "TYPE EXIT OR RESTART."
        else "ENTER RUNS COMMAND. ESC QUITS."
      in
      Camel_chess.Render.draw ~input:gs.input ~status:gs.status
        ~turn:(Camel_chess.Logic.turn_label gs.turn) ~check:gs.check
        ~check_squares:gs.check_squares ~hint ?selected:gs.selected
        ~targets:gs.targets ~flipped:(is_flipped gs) view gs.board

let rec pump_events event screen =
  if Sdl.poll_event (Some event) then
    match handle_event event screen with
    | `Quit            -> `Quit
    | `Continue screen -> pump_events event screen
  else `Continue screen

let rec event_loop window view event screen =
  redraw window view screen;
  match pump_events event screen with
  | `Quit -> ()
  | `Continue screen ->
      Sdl.delay 16l;
      event_loop window view event screen

(* ── Entry point ─────────────────────────────────────────────────────────── *)

let main () =
  sdl "SDL init" (Sdl.init Sdl.Init.(video + events));
  let window =
    sdl "Create window"
      (Sdl.create_window ~w:Camel_chess.Render.window_width
         ~h:Camel_chess.Render.window_height "Camel Chess"
         Sdl.Window.(shown + resizable))
  in
  let renderer =
    sdl "Create renderer"
      (Sdl.create_renderer window
         ~flags:Sdl.Renderer.(accelerated + presentvsync + targettexture))
  in
  sdl "Set logical size"
    (Sdl.render_set_logical_size renderer
       Camel_chess.Render.window_width Camel_chess.Render.window_height);
  let view = Camel_chess.Render.create renderer in
  Sdl.start_text_input ();
  Fun.protect
    (fun () ->
      event_loop window view (Sdl.Event.create ())
        (Title { input = ""; status = "" }))
    ~finally:(fun () ->
      Sdl.stop_text_input ();
      Camel_chess.Render.destroy view;
      Sdl.destroy_renderer renderer;
      Sdl.destroy_window window;
      Sdl.quit ())

let () = main ()
