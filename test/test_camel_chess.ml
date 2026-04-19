open OUnit2

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

let tests = "all tests" >::: [ tests; logic_tests ]
let _ = run_test_tt_main tests
