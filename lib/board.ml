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

type castling = {
  white_kingside : bool;
  white_queenside : bool;
  black_kingside : bool;
  black_queenside : bool;
}

type t = {
  squares : square array array;
  mutable white_kingside : bool;
  mutable white_queenside : bool;
  mutable black_kingside : bool;
  mutable black_queenside : bool;
  mutable en_passant : (int * int) option;
}

let board_size = 8

let get_castling b : castling =
  {
    white_kingside = b.white_kingside;
    white_queenside = b.white_queenside;
    black_kingside = b.black_kingside;
    black_queenside = b.black_queenside;
  }

let set_castling (b : t) (c : castling) =
  b.white_kingside <- c.white_kingside;
  b.white_queenside <- c.white_queenside;
  b.black_kingside <- c.black_kingside;
  b.black_queenside <- c.black_queenside

let get_en_passant b = b.en_passant
let set_en_passant b ep = b.en_passant <- ep

let empty () =
  {
    squares = Array.make_matrix board_size board_size None;
    white_kingside = false;
    white_queenside = false;
    black_kingside = false;
    black_queenside = false;
    en_passant = None;
  }

let in_bounds row col =
  row >= 0 && row < board_size && col >= 0 && col < board_size

let get board row col =
  if in_bounds row col then board.squares.(row).(col) else invalid_arg "Board.get"

let set board row col piece =
  if in_bounds row col then board.squares.(row).(col) <- piece
  else invalid_arg "Board.set"

let is_occupied board row col =
  if in_bounds row col then
    match board.squares.(row).(col) with
    | Some _ -> true
    | None -> false
  else invalid_arg "Board.is_occupied"

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
  let b = empty () in
  b.white_kingside <- true;
  b.white_queenside <- true;
  b.black_kingside <- true;
  b.black_queenside <- true;
  for col = 0 to board_size - 1 do
    b.squares.(0).(col) <- Some (back_rank Black).(col);
    b.squares.(1).(col) <- Some (Colored (Black, Pawn));
    b.squares.(6).(col) <- Some (Colored (White, Pawn));
    b.squares.(7).(col) <- Some (back_rank White).(col)
  done;
  b.squares.(3).(3) <- Some (Neutral Camel);
  b
