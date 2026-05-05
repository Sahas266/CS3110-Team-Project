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
  ?hint:string ->
  ?selected:int * int ->
  ?targets:(int * int) list ->
  ?flipped:bool ->
  t ->
  Board.t ->
  unit
(** Draw the board. When [flipped] is true the board renders from black's
    perspective (black's home rank at the bottom). *)

val draw_title : ?input:string -> ?status:string -> t -> unit
(** Draw the title / main-menu screen. *)

val draw_rules : t -> unit
(** Draw the rules screen. *)

val draw_multiplayer_menu : ?input:string -> ?status:string -> t -> unit
(** Draw the multiplayer host/join selection screen. *)

val draw_host_waiting : ip:string -> port:int -> t -> unit
(** Draw the hosting screen while waiting for a client to connect. *)

val draw_client_connecting : ?input:string -> ?status:string -> t -> unit
(** Draw the join-game screen where the player enters the host IP. *)
