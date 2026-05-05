open OUnit2

module B = Camel_chess.Board
module L = Camel_chess.Logic

(** Minimal White O-O setup: White Ke1, Rh1; Black king on e8; all vacant
    squares clear; kingside rights on. *)

let kingside_fixtures_all_rights () =
  let b = B.empty () in
  B.set_castling b
    {
      B.white_kingside = true;
      B.white_queenside = true;
      B.black_kingside = false;
      B.black_queenside = false;
    };
  B.set b 7 4 (Some (B.Colored (B.White, B.King)));
  B.set b 7 7 (Some (B.Colored (B.White, B.Rook)));
  B.set b 0 4 (Some (B.Colored (B.Black, B.King)));
  b

let castling_tests =
  "castling"
  >::: [
         ( "kingside destination in valid_moves when legal" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           let moves = L.valid_moves b 7 4 in
           assert_bool "(7,6) / g1 should be a king move"
             (List.mem (7, 6) moves) );
         ( "kingside disallowed when f1 occupied" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           B.set b 7 5 (Some (B.Colored (B.White, B.Knight)));
           let moves = L.valid_moves b 7 4 in
           assert_bool "blocked by knight on f1" (not (List.mem (7, 6) moves)) );
         ( "kingside disallowed when king is in check" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           (* Black rook attacks e-file on e8's rank would need line; use queen on row 7 col elsewhere *)
           B.set b 7 3 (Some (B.Colored (B.Black, B.Queen)));
           let moves = L.valid_moves b 7 4 in
           assert_bool "in check on e1" (not (List.mem (7, 6) moves)) );
         ( "kingside disallowed when f1 is attacked" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           B.set b 0 5 (Some (B.Colored (B.Black, B.Rook)));
           let moves = L.valid_moves b 7 4 in
           assert_bool "transit f1 attacked from f8"
             (not (List.mem (7, 6) moves)) );
         ( "kingside disallowed when white_kingside right is false" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           let c = B.get_castling b in
           B.set_castling b { c with white_kingside = false };
           let moves = L.valid_moves b 7 4 in
           assert_bool "no right" (not (List.mem (7, 6) moves)) );
         ( "apply_move O-O places king on g1 and rook on f1" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           L.apply_move b L.White_move (L.Move ((7, 4), (7, 6)));
           assert_equal (Some (B.Colored (B.White, B.King))) (B.get b 7 6);
           assert_equal (Some (B.Colored (B.White, B.Rook))) (B.get b 7 5);
           assert_equal None (B.get b 7 4);
           assert_equal None (B.get b 7 7);
           let c = B.get_castling b in
           assert_bool "lost white KS" (not c.white_kingside);
           assert_bool "lost white QS" (not c.white_queenside) );
       ]

let logic_tests =
  "logic tests"
  >::: [
         ( "parse_coordinate parses e2" >:: fun _ ->
           assert_equal (Some (6, 4)) (Camel_chess.Logic.parse_coordinate "e2")
         );
         ( "parse_coordinate rejects out-of-range" >:: fun _ ->
           assert_equal None (Camel_chess.Logic.parse_coordinate "z9") );
         ( "parse_command parses identify with spacing/case" >:: fun _ ->
           assert_equal
             (Camel_chess.Logic.Identify (6, 4))
             (Camel_chess.Logic.parse_command "  IDENTIFY   e2  ") );
         ( "evaluate_input identifies occupied square" >:: fun _ ->
           let board = Camel_chess.Board.initial () in
           assert_equal "e1 contains a white king."
             (Camel_chess.Logic.evaluate_input board "identify e1") );
         ( "evaluate_input identifies empty square" >:: fun _ ->
           let board = Camel_chess.Board.initial () in
           assert_equal "e4 is empty."
             (Camel_chess.Logic.evaluate_input board "identify e4") );
       ]

let tests =
  "test suite"
  >::: [
         ( "camel starts in center" >:: fun _ ->
           let board = Camel_chess.Board.initial () in
           assert_equal
             (Some (Camel_chess.Board.Neutral Camel_chess.Board.Camel))
             (Camel_chess.Board.get board 3 3) );
         ( "white king starts on e1" >:: fun _ ->
           let board = Camel_chess.Board.initial () in
           assert_equal
             (Some
                (Camel_chess.Board.Colored
                   (Camel_chess.Board.White, Camel_chess.Board.King)))
             (Camel_chess.Board.get board 7 4) );
         ( "black queen starts on d8" >:: fun _ ->
           let board = Camel_chess.Board.initial () in
           assert_equal
             (Some
                (Camel_chess.Board.Colored
                   (Camel_chess.Board.Black, Camel_chess.Board.Queen)))
             (Camel_chess.Board.get board 0 3) );
         ( "is_occupied returns true for occupied square" >:: fun _ ->
           let board = Camel_chess.Board.initial () in
           assert_bool "expected e1 to be occupied"
             (Camel_chess.Board.is_occupied board 7 4) );
         ( "is_occupied returns false for empty square" >:: fun _ ->
           let board = Camel_chess.Board.initial () in
           assert_bool "expected e4 to be empty"
             (not (Camel_chess.Board.is_occupied board 4 4)) );
         ( "is_occupied raises on out-of-bounds coordinates" >:: fun _ ->
           let board = Camel_chess.Board.initial () in
           assert_raises (Invalid_argument "Board.is_occupied") (fun () ->
               ignore (Camel_chess.Board.is_occupied board 8 0)) );
       ]

let tests = "all tests" >::: [ tests; logic_tests; castling_tests ]
let _ = run_test_tt_main tests
