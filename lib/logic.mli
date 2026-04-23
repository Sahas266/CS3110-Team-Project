type turn =
  | White_move
  | White_camel
  | Black_move
  | Black_camel
(** Whose turn it is and which phase: piece move or camel relocation. *)

type command =
  | Help
  | Clear
  | Identify of (int * int)
  | Valid of (int * int)
  | Move of (int * int) * (int * int)
  | Camel_move of (int * int)
  | Unknown

val parse_coordinate : string -> (int * int) option
(** Parse a coordinate like ["e2"] into [(row, col)] board indices. *)

val parse_command : string -> command
(** Parse user input into a command. Supports:
    - [help], [clear]
    - [identify <coord>], [valid <coord>]
    - [move <from> <to>] for a piece move
    - [move <to>] for a camel relocation *)

val describe_square : Board.t -> int -> int -> string
(** Human-readable description of the square at [(row, col)]. *)

val valid_moves : Board.t -> int -> int -> (int * int) list
(** [valid_moves board row col] returns the squares the piece at [(row, col)]
    can legally move to, respecting bounds and blocking. The neutral camel may
    move to any empty square. *)

val describe_valid : Board.t -> int -> int -> (int * int) list -> string
(** Status string for the valid-moves display. *)

val turn_color : turn -> Board.color
val advance_turn : turn -> turn
val prompt_for : turn -> string
val turn_label : turn -> string
val find_camel : Board.t -> (int * int) option

val is_valid_move : Board.t -> turn -> command -> (unit, string) result
(** Validates a [Move] or [Camel_move] against the current [turn]:
    - correct phase (piece vs camel)
    - source piece belongs to the side to move and is not a camel
    - destination is in [valid_moves] of the source (or of the camel) *)

val apply_move : Board.t -> turn -> command -> unit
(** Mutates [board] to apply the move. Caller is expected to validate first. *)

val evaluate_input : Board.t -> string -> string
(** Convert raw user input into a status line response. *)
