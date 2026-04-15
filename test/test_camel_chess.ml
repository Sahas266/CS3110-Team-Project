open OUnit2

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
       ]

let _ = run_test_tt_main tests
