(** Render a full terminal image of the current game state.

    Optional [input] and [status] strings are shown below the board. *)
val board_image :
  ?input:string -> ?status:string -> Board.t -> Notty.image