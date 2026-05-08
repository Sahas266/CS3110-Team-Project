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
    move to any empty square. Kings include castling destinations when allowed
    by occupancy, castling rights, check, and attacked transit/landing squares. *)

val describe_valid : Board.t -> int -> int -> (int * int) list -> string
(** Status string for the valid-moves display. *)

val turn_color : turn -> Board.color
(** [turn_color turn] returns the side to act for [turn]. *)
val advance_turn : turn -> turn
(** [advance_turn turn] advances from move phase to camel phase, or to the
    opponent's move phase after camel relocation. *)
val prompt_for : turn -> string
(** Prompt text for the current phase shown in the UI. *)
val turn_label : turn -> string
(** Short label for display, e.g. whose turn and phase. *)
val find_camel : Board.t -> (int * int) option
(** [find_camel board] returns the camel coordinate when present. *)
val find_king : Board.t -> Board.color -> (int * int) option
(** [find_king board color] returns [Some (row, col)] for [color]'s king,
    or [None] if that king is absent. *)

val is_attacked : Board.t -> int * int -> Board.color -> bool
(** [is_attacked board sq by_color] is true when some piece of [by_color] can
    reach [sq] (same movement as [valid_moves] except kings never attack via
    castling, which avoids cycles and matches standard attack detection). *)

val checks : Board.t -> (int * int) option * (int * int) option
(** Returns [(white_king_in_check, black_king_in_check)]. Each component is
    [Some square] when that color's king is currently attacked, else [None]. *)

val is_valid_move : Board.t -> turn -> command -> (unit, string) result
(** Validates a [Move] or [Camel_move] against the current [turn]:
    - correct phase (piece vs camel)
    - source piece belongs to the side to move and is not a camel
    - destination is in [valid_moves] of the source (or of the camel) *)

val apply_move : Board.t -> turn -> command -> unit
(** Mutates [board] to apply the move. Caller is expected to validate first. *)

val evaluate_input : Board.t -> string -> string
(** Convert raw user input into a status line response. *)
