type command =
  | Help
  | Clear
  | Identify of (int * int)
  | Valid of (int * int)
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
  | [ "valid"; coord ] | [ coord ] -> (
      match parse_coordinate coord with
      | Some square -> Valid square
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

let valid_moves board row col =
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
  | Some (Board.Colored (color, kind)) ->
      let slide dr dc =
        let acc = ref [] in
        let r = ref (row + dr) and c = ref (col + dc) in
        let go = ref true in
        while Board.in_bounds !r !c && !go do
          match Board.get board !r !c with
          | None -> acc := (!r, !c) :: !acc; r := !r + dr; c := !c + dc
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
        else match Board.get board r c with
          | None -> [ (r, c) ]
          | Some (Board.Colored (c2, _)) when c2 <> color -> [ (r, c) ]
          | _ -> []
      in
      let rook_dirs   = [ (1, 0); (-1, 0); (0, 1); (0, -1) ] in
      let bishop_dirs = [ (1, 1); (1, -1); (-1, 1); (-1, -1) ] in
      (match kind with
      | Board.Rook   -> List.concat_map (fun (dr, dc) -> slide dr dc) rook_dirs
      | Board.Bishop -> List.concat_map (fun (dr, dc) -> slide dr dc) bishop_dirs
      | Board.Queen  -> List.concat_map (fun (dr, dc) -> slide dr dc) (rook_dirs @ bishop_dirs)
      | Board.King   ->
          List.concat_map (fun (dr, dc) -> step dr dc)
            [ (1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1) ]
      | Board.Knight ->
          List.concat_map (fun (dr, dc) -> step dr dc)
            [ (2, 1); (2, -1); (-2, 1); (-2, -1); (1, 2); (1, -2); (-1, 2); (-1, -2) ]
      | Board.Pawn ->
          let dir = match color with Board.White -> -1 | Board.Black -> 1 in
          let start_row = match color with Board.White -> 6 | Board.Black -> 1 in
          let acc = ref [] in
          let r1 = row + dir in
          if Board.in_bounds r1 col && Board.get board r1 col = None then begin
            acc := (r1, col) :: !acc;
            let r2 = row + (2 * dir) in
            if row = start_row && Board.get board r2 col = None then
              acc := (r2, col) :: !acc
          end;
          List.iter (fun dc ->
            let r = row + dir and c = col + dc in
            if Board.in_bounds r c then
              match Board.get board r c with
              | Some (Board.Colored (c2, _)) when c2 <> color -> acc := (r, c) :: !acc
              | _ -> ()) [ -1; 1 ];
          !acc
      | Board.Camel -> [])

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
        let desc = match piece with
          | Board.Neutral Board.Camel -> "neutral camel"
          | Board.Neutral kind -> "neutral " ^ kind_name kind
          | Board.Colored (color, kind) ->
              (match color with Board.White -> "white" | Board.Black -> "black")
              ^ " " ^ kind_name kind
        in
        let name = String.uppercase_ascii (String.sub coord 0 1) ^ String.sub coord 1 (String.length coord - 1) in
        if targets = [] then name ^ " (" ^ desc ^ ") has no legal moves."
        else name ^ " (" ^ desc ^ ") can move to the squares shown above."

let evaluate_input board input =
  match parse_command input with
  | Help ->
      "Available commands: help, clear, identify e2, valid e2"
  | Clear -> "Cleared."
  | Identify (row, col) -> describe_square board row col
  | Valid _ -> ""
  | Unknown -> "Unknown input. Use help or try: identify e2 or valid e2"
