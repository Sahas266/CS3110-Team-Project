type turn =
  | White_move
  | White_camel
  | Black_move
  | Black_camel

type command =
  | Help
  | Clear
  | Identify of (int * int)
  | Valid of (int * int)
  | Move of (int * int) * (int * int)
  | Camel_move of (int * int)
  | Unknown

let turn_color = function
  | White_move | White_camel -> Board.White
  | Black_move | Black_camel -> Board.Black

let advance_turn = function
  | White_move -> White_camel
  | White_camel -> Black_move
  | Black_move -> Black_camel
  | Black_camel -> White_move

let prompt_for = function
  | White_move -> "White to move. Use MOVE FROM TO. EX: MOVE E2 E4."
  | White_camel -> "White camel move. Use MOVE TO. EX: MOVE E4."
  | Black_move -> "Black to move. Use MOVE FROM TO. EX: MOVE E7 E5."
  | Black_camel -> "Black camel move. Use MOVE TO. EX: MOVE E5."

let turn_label = function
  | White_move -> "White - Piece move"
  | White_camel -> "White - Camel move"
  | Black_move -> "Black - Piece move"
  | Black_camel -> "Black - Camel move"

let find_camel board =
  let result = ref None in
  for r = 0 to Board.board_size - 1 do
    for c = 0 to Board.board_size - 1 do
      if !result = None then
        match Board.get board r c with
        | Some (Board.Neutral Board.Camel) -> result := Some (r, c)
        | _ -> ()
    done
  done;
  !result

let find_king board color =
  let result = ref None in
  for r = 0 to Board.board_size - 1 do
    for c = 0 to Board.board_size - 1 do
      if !result = None then
        match Board.get board r c with
        | Some (Board.Colored (c2, Board.King)) when c2 = color ->
            result := Some (r, c)
        | _ -> ()
    done
  done;
  !result

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
  | [ "valid"; coord ] | [ coord ] -> (
      match parse_coordinate coord with
      | Some square -> Valid square
      | None -> Unknown)
  | [ "move"; src; dst ] -> (
      match (parse_coordinate src, parse_coordinate dst) with
      | Some s, Some d -> Move (s, d)
      | _ -> Unknown)
  | [ "move"; dst ] -> (
      match parse_coordinate dst with
      | Some d -> Camel_move d
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

(** [with_castling] is [false] when computing attacks so king castling targets
    do not participate (avoids cyclic dependence with [castling_moves]). *)

let rec moves_from board row col ~with_castling =
  match Board.get board row col with
  | None -> []
  | Some (Board.Neutral Board.Camel) ->
      let acc = ref [] in
      for r = 0 to Board.board_size - 1 do
        for c = 0 to Board.board_size - 1 do
          if Board.get board r c = None then acc := (r, c) :: !acc
        done
      done;
      !acc
  | Some (Board.Neutral _) -> []
  | Some (Board.Colored (color, kind)) -> (
      let slide dr dc =
        let acc = ref [] in
        let r = ref (row + dr) and c = ref (col + dc) in
        let go = ref true in
        while Board.in_bounds !r !c && !go do
          match Board.get board !r !c with
          | None ->
              acc := (!r, !c) :: !acc;
              r := !r + dr;
              c := !c + dc
          | Some (Board.Colored (c2, _)) ->
              if c2 <> color then acc := (!r, !c) :: !acc;
              go := false
          | Some (Board.Neutral _) -> go := false
        done;
        !acc
      in
      let step dr dc =
        let r = row + dr and c = col + dc in
        if not (Board.in_bounds r c) then []
        else
          match Board.get board r c with
          | None -> [ (r, c) ]
          | Some (Board.Colored (c2, _)) when c2 <> color -> [ (r, c) ]
          | _ -> []
      in
      let rook_dirs = [ (1, 0); (-1, 0); (0, 1); (0, -1) ] in
      let bishop_dirs = [ (1, 1); (1, -1); (-1, 1); (-1, -1) ] in
      match kind with
      | Board.Rook -> List.concat_map (fun (dr, dc) -> slide dr dc) rook_dirs
      | Board.Bishop ->
          List.concat_map (fun (dr, dc) -> slide dr dc) bishop_dirs
      | Board.Queen ->
          List.concat_map (fun (dr, dc) -> slide dr dc) (rook_dirs @ bishop_dirs)
      | Board.King ->
          let steps =
            List.concat_map
              (fun (dr, dc) -> step dr dc)
              [
                (1, 0);
                (-1, 0);
                (0, 1);
                (0, -1);
                (1, 1);
                (1, -1);
                (-1, 1);
                (-1, -1);
              ]
          in
          if with_castling then steps @ castling_moves board row col color
          else steps
      | Board.Knight ->
          List.concat_map
            (fun (dr, dc) -> step dr dc)
            [
              (2, 1);
              (2, -1);
              (-2, 1);
              (-2, -1);
              (1, 2);
              (1, -2);
              (-1, 2);
              (-1, -2);
            ]
      | Board.Pawn ->
          let dir =
            match color with
            | Board.White -> -1
            | Board.Black -> 1
          in
          let start_row =
            match color with
            | Board.White -> 6
            | Board.Black -> 1
          in
          let acc = ref [] in
          let r1 = row + dir in
          if Board.in_bounds r1 col && Board.get board r1 col = None then begin
            acc := (r1, col) :: !acc;
            let r2 = row + (2 * dir) in
            if row = start_row && Board.get board r2 col = None then
              acc := (r2, col) :: !acc
          end;
          List.iter
            (fun dc ->
              let r = row + dir and c = col + dc in
              if Board.in_bounds r c then
                match Board.get board r c with
                | Some (Board.Colored (c2, _)) when c2 <> color ->
                    acc := (r, c) :: !acc
                | _ -> ())
            [ -1; 1 ];
          !acc
      | Board.Camel -> [])

and castling_moves board row col color =
  let cast = Board.get_castling board in
  let opp =
    match color with
    | Board.White -> Board.Black
    | Board.Black -> Board.White
  in
  let start_row =
    match color with
    | Board.White -> 7
    | Board.Black -> 0
  in
  if row <> start_row || col <> 4 then []
  else if is_attacked board (row, col) opp then []
  else
    let acc = ref [] in
    let try_kingside () =
      if
        match color with
        | Board.White -> cast.white_kingside
        | Board.Black -> cast.black_kingside
      then
        match Board.get board row 7 with
        | Some (Board.Colored (c, Board.Rook)) when c = color ->
            if
              Board.get board row 5 = None
              && Board.get board row 6 = None
              && (not (is_attacked board (row, 5) opp))
              && not (is_attacked board (row, 6) opp)
            then acc := (row, 6) :: !acc
        | _ -> ()
    in
    let try_queenside () =
      if
        match color with
        | Board.White -> cast.white_queenside
        | Board.Black -> cast.black_queenside
      then
        match Board.get board row 0 with
        | Some (Board.Colored (c, Board.Rook)) when c = color ->
            if
              Board.get board row 1 = None
              && Board.get board row 2 = None
              && Board.get board row 3 = None
              && (not (is_attacked board (row, 3) opp))
              && not (is_attacked board (row, 2) opp)
            then acc := (row, 2) :: !acc
        | _ -> ()
    in
    try_kingside ();
    try_queenside ();
    !acc

and is_attacked board (kr, kc) by_color =
  let found = ref false in
  for r = 0 to Board.board_size - 1 do
    for c = 0 to Board.board_size - 1 do
      if not !found then
        match Board.get board r c with
        | Some (Board.Colored (col, _)) when col = by_color ->
            if List.mem (kr, kc) (moves_from board r c ~with_castling:false)
            then found := true
        | _ -> ()
    done
  done;
  !found

let valid_moves board row col = moves_from board row col ~with_castling:true

let describe_valid board row col targets =
  if not (Board.in_bounds row col) then "Invalid coordinate."
  else
    let coord =
      let file = Char.chr (Char.code 'a' + col) in
      let rank = string_of_int (Board.board_size - row) in
      Char.escaped file ^ rank
    in
    match Board.get board row col with
    | None -> "No piece at " ^ coord ^ "."
    | Some piece ->
        let desc =
          match piece with
          | Board.Neutral Board.Camel -> "neutral camel"
          | Board.Neutral kind -> "neutral " ^ kind_name kind
          | Board.Colored (color, kind) ->
              (match color with
                | Board.White -> "white"
                | Board.Black -> "black")
              ^ " " ^ kind_name kind
        in
        let name =
          String.uppercase_ascii (String.sub coord 0 1)
          ^ String.sub coord 1 (String.length coord - 1)
        in
        if targets = [] then name ^ " (" ^ desc ^ ") has no legal moves."
        else name ^ " (" ^ desc ^ ") can move to the squares shown above."

let is_valid_move board turn cmd =
  match (turn, cmd) with
  | (White_move | Black_move), Move ((sr, sc), dst) -> (
      let color = turn_color turn in
      match Board.get board sr sc with
      | None -> Error "No piece at source square."
      | Some (Board.Neutral _) ->
          Error "Neutral pieces cannot be moved during the piece phase."
      | Some (Board.Colored (c, _)) when c <> color ->
          Error "That piece does not belong to you."
      | Some (Board.Colored (_, Board.Camel)) ->
          Error "Camels are moved during the camel phase."
      | Some (Board.Colored _) ->
          let moves = valid_moves board sr sc in
          if List.mem dst moves then Ok ()
          else Error "That piece cannot move to that square.")
  | (White_move | Black_move), Camel_move _ ->
      Error "Piece move phase. Use MOVE FROM TO. EX: MOVE E2 E4."
  | (White_camel | Black_camel), Camel_move dst -> (
      match find_camel board with
      | None -> Error "No camel on the board."
      | Some (cr, cc) ->
          let moves = valid_moves board cr cc in
          if List.mem dst moves then Ok ()
          else Error "Camel must move to an empty square.")
  | (White_camel | Black_camel), Move _ ->
      Error "Camel move phase. Use MOVE TO. EX: MOVE E4."
  | _, _ -> Error "Not a move command."

let clear_castling_for_capture_on_corner board dr dc =
  let c = Board.get_castling board in
  let c' =
    match (dr, dc) with
    | 7, 7 -> { c with white_kingside = false }
    | 7, 0 -> { c with white_queenside = false }
    | 0, 7 -> { c with black_kingside = false }
    | 0, 0 -> { c with black_queenside = false }
    | _ -> c
  in
  if c <> c' then Board.set_castling board c'

let clear_castling_for_moving_piece board sr sc piece =
  let c = Board.get_castling board in
  let c' =
    match piece with
    | Board.Colored (Board.White, Board.King) ->
        { c with white_kingside = false; white_queenside = false }
    | Board.Colored (Board.Black, Board.King) ->
        { c with black_kingside = false; black_queenside = false }
    | Board.Colored (Board.White, Board.Rook) when sr = 7 && sc = 7 ->
        { c with white_kingside = false }
    | Board.Colored (Board.White, Board.Rook) when sr = 7 && sc = 0 ->
        { c with white_queenside = false }
    | Board.Colored (Board.Black, Board.Rook) when sr = 0 && sc = 7 ->
        { c with black_kingside = false }
    | Board.Colored (Board.Black, Board.Rook) when sr = 0 && sc = 0 ->
        { c with black_queenside = false }
    | _ -> c
  in
  if c <> c' then Board.set_castling board c'

(** Promotions onto a corner rook square can wrongly leave castling rights true;
    fix when pawn promotion exists. *)

let apply_move board turn cmd =
  match (turn, cmd) with
  | (White_move | Black_move), Move ((sr, sc), (dr, dc)) -> (
      match Board.get board sr sc with
      | None -> ()
      | Some piece ->
          clear_castling_for_capture_on_corner board dr dc;
          clear_castling_for_moving_piece board sr sc piece;
          let is_castle =
            match piece with
            | Board.Colored (_, Board.King) ->
                sr = dr
                && abs (dc - sc) = 2
                && sc = 4
                && ((sr = 7 && dc = 6)
                   || (sr = 7 && dc = 2)
                   || (sr = 0 && dc = 6)
                   || (sr = 0 && dc = 2))
            | _ -> false
          in
          if is_castle then begin
            Board.set board sr sc None;
            Board.set board dr dc (Some piece);
            let rook_from_col, rook_to_col =
              if dc > sc then (7, 5) else (0, 3)
            in
            let rook_piece = Board.get board sr rook_from_col in
            Board.set board sr rook_from_col None;
            Board.set board sr rook_to_col rook_piece
          end
          else begin
            Board.set board sr sc None;
            Board.set board dr dc (Some piece)
          end)
  | (White_camel | Black_camel), Camel_move (dr, dc) -> (
      match find_camel board with
      | Some (cr, cc) ->
          Board.set board cr cc None;
          Board.set board dr dc (Some (Board.Neutral Board.Camel))
      | None -> ())
  | _ -> ()

let checks board =
  let for_color color =
    match find_king board color with
    | Some sq ->
        let opp =
          match color with
          | Board.White -> Board.Black
          | Board.Black -> Board.White
        in
        if is_attacked board sq opp then Some sq else None
    | None -> None
  in
  (for_color Board.White, for_color Board.Black)

let evaluate_input board input =
  match parse_command input with
  | Help ->
      "Commands: HELP CLEAR IDENTIFY E2 VALID E2 MOVE E2 E4. Camel EX: MOVE E4"
  | Clear -> "Cleared."
  | Identify (row, col) -> describe_square board row col
  | Valid _ -> ""
  | Move _ | Camel_move _ -> ""
  | Unknown -> "Unknown input. EX: MOVE E2 E4. Type HELP for all commands"
