(** Piece color for standard sides. *)
type color =
  | White
  | Black

(** Supported piece types. *)
type kind =
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
  | Camel

(** A piece can be owned by one side or be neutral. *)
type piece =
  | Colored of color * kind
  | Neutral of kind

type square = piece option
(** A board square either contains a piece or is empty. *)

type t
(** Abstract board type.

    The concrete representation is intentionally hidden so callers use this
    module's API rather than depending on mutable internals. *)

val board_size : int
(** Number of files/ranks on the board. *)

val empty : unit -> t
(** Create an empty board. *)

val in_bounds : int -> int -> bool
(** [in_bounds row col] is [true] when [(row, col)] is a valid coordinate. *)

val get : t -> int -> int -> square
(** [get board row col] returns the contents of the square at [(row, col)].
    Raises [Invalid_argument] when the coordinate is out of bounds. *)

val set : t -> int -> int -> square -> unit
(** [set board row col occupant] updates the square at [(row, col)] to
    [occupant]. Raises [Invalid_argument] when the coordinate is out of bounds.
*)

val is_occupied : t -> int -> int -> bool
(** [is_occupied board row col] is [true] when square [(row, col)] contains a
  piece and [false] when it is empty.

  Raises [Invalid_argument] when the coordinate is out of bounds. *)

val initial : unit -> t
(** Initial Camel Chess setup.

    Standard chess pieces are placed in their usual starting squares, and a
  neutral camel (represented as [Neutral Camel]) is placed at the board
    center. *)
