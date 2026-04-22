(** Render a full terminal image of the current game state.

    Optional [input] and [status] strings are shown below the board. *)
val board_image :
  ?input:string ->
  ?status:string ->
  ?selected:(int * int) option ->
  ?targets:(int * int) list ->
  Board.t -> Notty.image