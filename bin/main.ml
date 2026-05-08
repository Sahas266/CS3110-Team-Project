open Tsdl

let sdl context = function
  | Ok value -> value
  | Error (`Msg msg) -> failwith (context ^ ": " ^ msg)

let drop_last_char s =
  let len = String.length s in
  if len = 0 then s else String.sub s 0 (len - 1)

(* ── Networking ──────────────────────────────────────────────────────────── *)

let mp_port = 9876

let is_wsl () =
  try
    let ic = open_in "/proc/version" in
    let line = input_line ic in
    close_in ic;
    let low = String.lowercase_ascii line in
    let contains s sub =
      let slen = String.length s and n = String.length sub in
      let rec go i = i <= slen - n && (String.sub s i n = sub || go (i + 1)) in
      go 0
    in
    contains low "microsoft" || contains low "wsl"
  with _ -> false

let local_ip () =
  let udp_try () =
    try
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
      Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_of_string "8.8.8.8", 80));
      let addr = Unix.getsockname fd in
      Unix.close fd;
      match addr with
      | Unix.ADDR_INET (a, _) -> Some (Unix.string_of_inet_addr a)
      | _ -> None
    with _ -> None
  in
  let powershell_try () =
    try
      let ic = Unix.open_process_in
        {|powershell.exe -NoProfile -Command "(Get-NetIPAddress -AddressFamily IPv4 -PrefixOrigin Dhcp | Select-Object -First 1).IPAddress" 2>/dev/null|} in
      let ip = String.trim (input_line ic) in
      ignore (Unix.close_process_in ic);
      if ip = "" || ip = "127.0.0.1" then None else Some ip
    with _ -> None
  in
  if is_wsl () then
    (match powershell_try () with Some ip -> ip | None -> "127.0.0.1")
  else
    (match udp_try () with Some ip -> ip | None -> "127.0.0.1")

let create_server port =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen fd 1;
  Unix.set_nonblock fd;
  fd

let poll_accept server_fd =
  try
    let (client_fd, _) = Unix.accept server_fd in
    Unix.set_nonblock client_fd;
    Some client_fd
  with Unix.Unix_error (_, _, _) -> None

let connect_to ip port =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  try
    let addr = Unix.ADDR_INET (Unix.inet_addr_of_string ip, port) in
    Unix.connect fd addr;
    Unix.set_nonblock fd;
    Ok fd
  with
  | Unix.Unix_error (e, _, _) ->
      Unix.close fd; Error (Unix.error_message e)
  | Failure msg | Invalid_argument msg ->
      Unix.close fd; Error ("Invalid address: " ^ msg)

let send_move fd cmd =
  let msg = String.uppercase_ascii (String.trim cmd) ^ "\n" in
  let b = Bytes.of_string msg in
  (try ignore (Unix.send fd b 0 (Bytes.length b) []) with _ -> ())

let try_read_line fd buf =
  let tmp = Bytes.create 256 in
  (try
    let n = Unix.recv fd tmp 0 256 [] in
    if n > 0 then Buffer.add_string buf (Bytes.sub_string tmp 0 n)
  with Unix.Unix_error (_, _, _) -> ());
  let s = Buffer.contents buf in
  match String.index_opt s '\n' with
  | None -> None
  | Some i ->
      let line = String.sub s 0 i in
      let rest = String.sub s (i + 1) (String.length s - i - 1) in
      Buffer.clear buf;
      Buffer.add_string buf rest;
      Some (String.trim line)

let close_fd fd = (try Unix.close fd with _ -> ())

(* ── Game state ─────────────────────────────────────────────────────────── *)

type game_mode =
  | Solo
  | Host   of Unix.file_descr
  | Client of Unix.file_descr

type game_state = {
  board             : Camel_chess.Board.t;
  input             : string;
  status            : string;
  selected          : (int * int) option;
  targets           : (int * int) list;
  turn              : Camel_chess.Logic.turn;
  mode              : game_mode;
  check             : string;
  check_squares     : (int * int) list;
  winner            : Camel_chess.Board.color option;
  recv_buf          : Buffer.t;
  promotion_pending : (int * int) option;
}

(* ── Screen ─────────────────────────────────────────────────────────────── *)

type screen =
  | Title             of { input : string; status : string }
  | Rules
  | Multiplayer_menu  of { input : string; status : string }
  | Host_waiting      of { server_fd : Unix.file_descr; ip : string; port : int }
  | Client_connecting of { input : string; status : string }
  | Game              of game_state

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
    check = ""; check_squares = []; winner = None;
    recv_buf = Buffer.create 256; promotion_pending = None }

let promotion_status () = "Choose promotion: Q, R, B, or N."

(* After a Move/Camel_move/Promote that leaves the board in a normal state,
   this advances the turn and refreshes derived fields. *)
let post_move_update gs =
  match detect_winner gs.board with
  | Some color ->
      { gs with selected = None; targets = []; status = "";
                check = checkmate_message color;
                check_squares = []; winner = Some color;
                promotion_pending = None }
  | None ->
      let new_turn = Camel_chess.Logic.advance_turn gs.turn in
      let check, check_squares = compute_check gs.board in
      { gs with turn = new_turn; selected = None; targets = [];
                status = Camel_chess.Logic.prompt_for new_turn;
                check; check_squares; promotion_pending = None }

let is_flipped gs =
  match gs.mode with
  | Solo     -> Camel_chess.Logic.turn_color gs.turn = Camel_chess.Board.Black
  | Host _   -> false
  | Client _ -> true

let is_my_turn gs =
  match gs.mode with
  | Solo   -> true
  | Host _ ->
      (match gs.turn with
       | Camel_chess.Logic.White_move | Camel_chess.Logic.White_camel -> true
       | _ -> false)
  | Client _ ->
      (match gs.turn with
       | Camel_chess.Logic.Black_move | Camel_chess.Logic.Black_camel -> true
       | _ -> false)

let apply_peer_move gs input =
  match Camel_chess.Logic.parse_command input with
  | Camel_chess.Logic.Promote kind ->
      (match gs.promotion_pending with
       | Some sq ->
           Camel_chess.Logic.promote_pawn gs.board sq kind;
           post_move_update gs
       | None -> gs)
  | (Camel_chess.Logic.Move _ | Camel_chess.Logic.Camel_move _) as cmd ->
      (match Camel_chess.Logic.is_valid_move gs.board gs.turn cmd with
       | Ok () ->
           Camel_chess.Logic.apply_move gs.board gs.turn cmd;
           (match Camel_chess.Logic.pending_promotion gs.board with
            | Some sq ->
                { gs with promotion_pending = Some sq;
                          status = promotion_status () }
            | None -> post_move_update gs)
       | Error _ -> gs)
  | _ -> gs

let promotion_kind_of_input s =
  match String.lowercase_ascii (String.trim s) with
  | "q" | "queen" | "promote q" | "promote queen" ->
      Some Camel_chess.Board.Queen
  | "r" | "rook" | "promote r" | "promote rook" ->
      Some Camel_chess.Board.Rook
  | "b" | "bishop" | "promote b" | "promote bishop" ->
      Some Camel_chess.Board.Bishop
  | "n" | "knight" | "promote n" | "promote knight" ->
      Some Camel_chess.Board.Knight
  | _ -> None

let handle_game_command gs =
  match gs.promotion_pending with
  | Some sq when is_my_turn gs ->
      (match promotion_kind_of_input gs.input with
       | None ->
           { gs with status = promotion_status () }
       | Some kind ->
           Camel_chess.Logic.promote_pawn gs.board sq kind;
           let kind_letter =
             match kind with
             | Camel_chess.Board.Queen -> "q"
             | Camel_chess.Board.Rook -> "r"
             | Camel_chess.Board.Bishop -> "b"
             | Camel_chess.Board.Knight -> "n"
             | _ -> "q"
           in
           (match gs.mode with
            | Host fd | Client fd -> send_move fd ("promote " ^ kind_letter)
            | Solo -> ());
           post_move_update gs)
  | Some _ ->
      { gs with status = "Wait for opponent's promotion." }
  | None ->
      (match Camel_chess.Logic.parse_command gs.input with
       | Camel_chess.Logic.Valid (row, col) ->
           let targets = Camel_chess.Logic.valid_moves gs.board row col in
           { gs with selected = Some (row, col); targets;
                     status = Camel_chess.Logic.describe_valid gs.board row col targets }
       | (Camel_chess.Logic.Move _ | Camel_chess.Logic.Camel_move _) as cmd ->
           if not (is_my_turn gs) then
             { gs with selected = None; targets = []; status = "Wait for your turn." }
           else
             (match Camel_chess.Logic.is_valid_move gs.board gs.turn cmd with
              | Ok () ->
                  Camel_chess.Logic.apply_move gs.board gs.turn cmd;
                  (match gs.mode with
                   | Host fd | Client fd -> send_move fd gs.input
                   | Solo -> ());
                  (match Camel_chess.Logic.pending_promotion gs.board with
                   | Some sq ->
                       { gs with selected = None; targets = [];
                                 promotion_pending = Some sq;
                                 status = promotion_status () }
                   | None -> post_move_update gs)
              | Error msg ->
                  { gs with selected = None; targets = []; status = msg })
       | _ ->
           { gs with selected = None; targets = [];
                     status = Camel_chess.Logic.evaluate_input gs.board gs.input })

let report cmd status =
  if String.trim cmd    <> "" then Printf.printf "> %s\n%!" cmd;
  if String.trim status <> "" then Printf.printf "%s\n%!" status

let append_text_screen screen text =
  let keep c = Char.code c >= 32 && Char.code c <= 126 in
  let s = text |> String.to_seq |> Seq.filter keep |> String.of_seq in
  if s = "" then screen
  else match screen with
  | Title { input; status }             -> Title { input = input ^ s; status }
  | Multiplayer_menu { input; status }  -> Multiplayer_menu { input = input ^ s; status }
  | Client_connecting { input; status } -> Client_connecting { input = input ^ s; status }
  | Game gs                             -> Game { gs with input = gs.input ^ s }
  | other                               -> other

(* ── Key handling ────────────────────────────────────────────────────────── *)

let handle_key key screen =
  if key = Sdl.K.backspace then
    (match screen with
     | Title { input; status }             -> Title { input = drop_last_char input; status }
     | Multiplayer_menu { input; status }  -> Multiplayer_menu { input = drop_last_char input; status }
     | Client_connecting { input; status } -> Client_connecting { input = drop_last_char input; status }
     | Game gs                             -> Game { gs with input = drop_last_char gs.input }
     | Rules | Host_waiting _              -> screen)
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
          | "1" | "host" ->
              (try
                 let server_fd = create_server mp_port in
                 Host_waiting { server_fd; ip = local_ip (); port = mp_port }
               with Unix.Unix_error (e, _, _) ->
                 Multiplayer_menu { input = "";
                   status = "Could not open port: " ^ Unix.error_message e })
          | "2" | "join" | "connect" ->
              Client_connecting { input = ""; status = "" }
          | _ ->
              Multiplayer_menu { input = ""; status = "TYPE 1 OR 2 AND PRESS ENTER." })
     | Client_connecting { input; _ } ->
         let ip = String.trim input in
         if ip = "" then
           Client_connecting { input = ""; status = "Enter an IP address." }
         else
           (match connect_to ip mp_port with
            | Ok fd    -> Game (new_game (Client fd))
            | Error msg -> Client_connecting { input = ""; status = msg })
     | Host_waiting _ -> screen
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
         | Title _ -> `Quit
         | Game { mode = Host fd; _ } | Game { mode = Client fd; _ } ->
             close_fd fd; `Quit
         | Game _ -> `Quit
         | Host_waiting { server_fd; _ } ->
             close_fd server_fd;
             `Continue (Title { input = ""; status = "" })
         | Rules | Multiplayer_menu _ | Client_connecting _ ->
             `Continue (Title { input = ""; status = "" }))
      else if key = Sdl.K.return then
        (match screen with
         | Game gs when gs.winner <> None ->
             (match String.trim (String.lowercase_ascii gs.input) with
              | "exit" ->
                  (match gs.mode with
                   | Host fd | Client fd -> close_fd fd
                   | Solo -> ());
                  `Quit
              | "restart" ->
                  let fresh = new_game Solo in
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
  | Host_waiting { ip; port; _ } ->
      Sdl.set_window_title window "Camel Chess - Hosting";
      Camel_chess.Render.draw_host_waiting ~ip ~port view
  | Client_connecting { input; status } ->
      Sdl.set_window_title window "Camel Chess - Join Game";
      Camel_chess.Render.draw_client_connecting ~input ~status view
  | Game gs ->
      Sdl.set_window_title window (clipped_title gs);
      let hint =
        if gs.winner <> None then "TYPE EXIT OR RESTART."
        else if gs.promotion_pending <> None then
          "TYPE Q, R, B, OR N AND PRESS ENTER."
        else "ENTER RUNS COMMAND. ESC QUITS."
      in
      Camel_chess.Render.draw ~input:gs.input ~status:gs.status
        ~turn:(Camel_chess.Logic.turn_label gs.turn) ~check:gs.check
        ~check_squares:gs.check_squares ~hint ?selected:gs.selected
        ~targets:gs.targets ~flipped:(is_flipped gs) view gs.board

(* ── Network polling (called each frame) ────────────────────────────────── *)

let rec poll_screen screen =
  match screen with
  | Host_waiting { server_fd; ip = _; port = _ } ->
      (match poll_accept server_fd with
       | None -> screen
       | Some client_fd ->
           Unix.close server_fd;
           Game (new_game (Host client_fd)))
  | Game gs ->
      let fd_opt = match gs.mode with
        | Host fd | Client fd -> Some fd
        | Solo                -> None
      in
      let peer_turn = match gs.mode with
        | Host _   ->
            (match gs.turn with
             | Camel_chess.Logic.Black_move | Camel_chess.Logic.Black_camel -> true
             | _ -> false)
        | Client _ ->
            (match gs.turn with
             | Camel_chess.Logic.White_move | Camel_chess.Logic.White_camel -> true
             | _ -> false)
        | Solo -> false
      in
      (match fd_opt with
       | Some fd when peer_turn ->
           (match try_read_line fd gs.recv_buf with
            | None -> screen
            | Some line ->
                let updated = apply_peer_move gs line in
                poll_screen (Game { updated with input = "" }))
       | _ -> screen)
  | other -> other

let rec pump_events event screen =
  if Sdl.poll_event (Some event) then
    match handle_event event screen with
    | `Quit            -> `Quit
    | `Continue screen -> pump_events event screen
  else `Continue screen

let rec event_loop window view event screen =
  let screen = poll_screen screen in
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
