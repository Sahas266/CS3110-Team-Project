type color =
  | White
  | Black

type kind =
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
  | Camel

type piece =
  | Colored of color * kind
  | Neutral of kind

type square = piece option

type t = square array array

let board_size = 8

let empty () = Array.make_matrix board_size board_size None

let in_bounds row col =
  row >= 0 && row < board_size && col >= 0 && col < board_size

let get board row col =
  if in_bounds row col then board.(row).(col) else invalid_arg "Board.get"

let set board row col piece =
  if in_bounds row col then board.(row).(col) <- piece
  else invalid_arg "Board.set"

let back_rank color =
  [|
    Colored (color, Rook);
    Colored (color, Knight);
    Colored (color, Bishop);
    Colored (color, Queen);
    Colored (color, King);
    Colored (color, Bishop);
    Colored (color, Knight);
    Colored (color, Rook);
  |]

let initial () =
  let board = empty () in
  for col = 0 to board_size - 1 do
    board.(0).(col) <- Some (back_rank Black).(col);
    board.(1).(col) <- Some (Colored (Black, Pawn));
    board.(6).(col) <- Some (Colored (White, Pawn));
    board.(7).(col) <- Some (back_rank White).(col)
  done;
  board.(3).(3) <- Some (Neutral Camel);
  board
