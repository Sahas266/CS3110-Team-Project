type command =
  | Help
  | Clear
  | Identify of (int * int)
  | Unknown

val parse_coordinate : string -> (int * int) option
(** Parse a coordinate like ["e2"] into [(row, col)] board indices. *)

val parse_command : string -> command
(** Parse user input into a command. Supports:
    - [help]
    - [clear]
    - [identify <coord>] *)

val describe_square : Board.t -> int -> int -> string
(** Human-readable description of the square at [(row, col)]. *)

val valid_moves : Board.t -> int -> int -> (int * int) list
(** [valid_moves board row col] returns the list of squares the piece at
    [(row, col)] can legally move to, respecting bounds and blocking.
    Returns [[]] if the square is empty.
    The neutral camel may move to any empty square. *)

val evaluate_input : Board.t -> string -> string
(** Convert raw user input into a status line response. *)
