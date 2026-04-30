(** TSDL renderer for the chess board. *)

val cell_size : int
(** Pixel size of one board square. *)

val margin : int
(** Pixel margin around the board. *)

val info_height : int
(** Pixel height of the command/status panel under the board. *)

val window_width : int
(** Recommended SDL window width for the board. *)

val window_height : int
(** Recommended SDL window height for the board. *)

type t
(** Cached renderer state, including the piece textures. *)

val create : Tsdl.Sdl.renderer -> t
(** [create renderer] builds reusable SDL textures for the chess pieces. *)

val destroy : t -> unit
(** Release SDL textures owned by the renderer state. *)

val draw :
  ?input:string ->
  ?status:string ->
  ?turn:string ->
  ?check:string ->
  ?check_squares:(int * int) list ->
  ?selected:int * int ->
  ?targets:(int * int) list ->
  t ->
  Board.t ->
  unit
(** Draw the board and its piece images into the SDL window. *)
