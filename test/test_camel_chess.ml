open OUnit2
module B = Camel_chess.Board
module L = Camel_chess.Logic
module Sdl = Tsdl.Sdl

(* ── Fixtures ─────────────────────────────────────────────────────────── *)

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

let queenside_fixture () =
  let b = B.empty () in
  B.set_castling b
    {
      B.white_kingside = false;
      B.white_queenside = true;
      B.black_kingside = false;
      B.black_queenside = true;
    };
  B.set b 7 4 (Some (B.Colored (B.White, B.King)));
  B.set b 7 0 (Some (B.Colored (B.White, B.Rook)));
  B.set b 0 4 (Some (B.Colored (B.Black, B.King)));
  B.set b 0 0 (Some (B.Colored (B.Black, B.Rook)));
  b

(* ── Castling tests (existing + new black/queenside coverage) ─────────── *)

let castling_tests =
  "castling"
  >::: [
         ( "kingside destination in valid_moves when legal" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           let moves = L.valid_moves b 7 4 in
           assert_bool "g1 should be a king move"
             (List.mem (7, 6) moves) );
         ( "kingside disallowed when f1 occupied" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           B.set b 7 5 (Some (B.Colored (B.White, B.Knight)));
           let moves = L.valid_moves b 7 4 in
           assert_bool "blocked by knight on f1" (not (List.mem (7, 6) moves)) );
         ( "kingside disallowed when king is in check" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           B.set b 7 3 (Some (B.Colored (B.Black, B.Queen)));
           let moves = L.valid_moves b 7 4 in
           assert_bool "in check on e1" (not (List.mem (7, 6) moves)) );
         ( "kingside disallowed when f1 attacked" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           B.set b 0 5 (Some (B.Colored (B.Black, B.Rook)));
           let moves = L.valid_moves b 7 4 in
           assert_bool "f1 attacked" (not (List.mem (7, 6) moves)) );
         ( "kingside disallowed when right is false" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           let c = B.get_castling b in
           B.set_castling b { c with white_kingside = false };
           let moves = L.valid_moves b 7 4 in
           assert_bool "no right" (not (List.mem (7, 6) moves)) );
         ( "apply_move O-O places king on g1, rook on f1" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           L.apply_move b L.White_move (L.Move ((7, 4), (7, 6)));
           assert_equal (Some (B.Colored (B.White, B.King))) (B.get b 7 6);
           assert_equal (Some (B.Colored (B.White, B.Rook))) (B.get b 7 5);
           assert_equal None (B.get b 7 4);
           assert_equal None (B.get b 7 7);
           let c = B.get_castling b in
           assert_bool "lost white KS" (not c.white_kingside);
           assert_bool "lost white QS" (not c.white_queenside) );
         ( "queenside white legal then applied" >:: fun _ ->
           let b = queenside_fixture () in
           let moves = L.valid_moves b 7 4 in
           assert_bool "c1 in moves" (List.mem (7, 2) moves);
           L.apply_move b L.White_move (L.Move ((7, 4), (7, 2)));
           assert_equal (Some (B.Colored (B.White, B.King))) (B.get b 7 2);
           assert_equal (Some (B.Colored (B.White, B.Rook))) (B.get b 7 3);
           assert_equal None (B.get b 7 0) );
         ( "queenside blocked when b1 occupied" >:: fun _ ->
           let b = queenside_fixture () in
           B.set b 7 1 (Some (B.Colored (B.White, B.Knight)));
           let moves = L.valid_moves b 7 4 in
           assert_bool "blocked" (not (List.mem (7, 2) moves)) );
         ( "queenside blocked when c1 occupied" >:: fun _ ->
           let b = queenside_fixture () in
           B.set b 7 2 (Some (B.Colored (B.White, B.Knight)));
           let moves = L.valid_moves b 7 4 in
           assert_bool "blocked" (not (List.mem (7, 2) moves)) );
         ( "queenside blocked when d1 attacked" >:: fun _ ->
           let b = queenside_fixture () in
           B.set b 0 3 (Some (B.Colored (B.Black, B.Rook)));
           let moves = L.valid_moves b 7 4 in
           assert_bool "d1 attacked" (not (List.mem (7, 2) moves)) );
         ( "black kingside O-O" >:: fun _ ->
           let b = B.empty () in
           B.set_castling b
             {
               B.white_kingside = false;
               B.white_queenside = false;
               B.black_kingside = true;
               B.black_queenside = false;
             };
           B.set b 0 4 (Some (B.Colored (B.Black, B.King)));
           B.set b 0 7 (Some (B.Colored (B.Black, B.Rook)));
           B.set b 7 4 (Some (B.Colored (B.White, B.King)));
           let moves = L.valid_moves b 0 4 in
           assert_bool "g8 in moves" (List.mem (0, 6) moves);
           L.apply_move b L.Black_move (L.Move ((0, 4), (0, 6)));
           assert_equal (Some (B.Colored (B.Black, B.King))) (B.get b 0 6);
           assert_equal (Some (B.Colored (B.Black, B.Rook))) (B.get b 0 5) );
         ( "black queenside O-O-O" >:: fun _ ->
           let b = queenside_fixture () in
           let moves = L.valid_moves b 0 4 in
           assert_bool "c8 in moves" (List.mem (0, 2) moves);
           L.apply_move b L.Black_move (L.Move ((0, 4), (0, 2)));
           assert_equal (Some (B.Colored (B.Black, B.King))) (B.get b 0 2);
           assert_equal (Some (B.Colored (B.Black, B.Rook))) (B.get b 0 3) );
         ( "castling not generated when king is off the back rank" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           B.set b 7 4 None;
           B.set b 6 4 (Some (B.Colored (B.White, B.King)));
           let moves = L.valid_moves b 6 4 in
           assert_bool "no g-file castle from e2"
             (not (List.mem (6, 6) moves)) );
         ( "castling not generated when king on back rank but wrong file" ^
           "" >:: fun _ ->
           (* covers the col != 4 branch of the guard *)
           let b = B.empty () in
           B.set_castling b
             {
               B.white_kingside = true;
               B.white_queenside = true;
               B.black_kingside = false;
               B.black_queenside = false;
             };
           B.set b 7 3 (Some (B.Colored (B.White, B.King)));
           B.set b 7 7 (Some (B.Colored (B.White, B.Rook)));
           B.set b 0 4 (Some (B.Colored (B.Black, B.King)));
           let moves = L.valid_moves b 7 3 in
           assert_bool "no castle from d1" (not (List.mem (7, 5) moves)) );
         ( "castling not generated when rook is missing" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           B.set b 7 7 None;
           let moves = L.valid_moves b 7 4 in
           assert_bool "no rook -> no castle"
             (not (List.mem (7, 6) moves)) );
         ( "rook move clears the matching castling right" >:: fun _ ->
           let b = kingside_fixtures_all_rights () in
           L.apply_move b L.White_move (L.Move ((7, 7), (7, 6)));
           let c = B.get_castling b in
           assert_bool "white KS lost" (not c.white_kingside);
           assert_bool "white QS still" c.white_queenside );
         ( "rook move from a1 clears white_queenside" >:: fun _ ->
           let b = queenside_fixture () in
           L.apply_move b L.White_move (L.Move ((7, 0), (7, 1)));
           let c = B.get_castling b in
           assert_bool "white QS lost" (not c.white_queenside) );
         ( "rook move from h8 clears black_kingside" >:: fun _ ->
           let b = B.empty () in
           B.set_castling b
             {
               B.white_kingside = false;
               B.white_queenside = false;
               B.black_kingside = true;
               B.black_queenside = true;
             };
           B.set b 0 4 (Some (B.Colored (B.Black, B.King)));
           B.set b 0 7 (Some (B.Colored (B.Black, B.Rook)));
           B.set b 0 0 (Some (B.Colored (B.Black, B.Rook)));
           B.set b 7 4 (Some (B.Colored (B.White, B.King)));
           L.apply_move b L.Black_move (L.Move ((0, 7), (0, 6)));
           let c = B.get_castling b in
           assert_bool "black KS lost" (not c.black_kingside) );
         ( "rook move from a8 clears black_queenside" >:: fun _ ->
           let b = queenside_fixture () in
           L.apply_move b L.Black_move (L.Move ((0, 0), (0, 1)));
           let c = B.get_castling b in
           assert_bool "black QS lost" (not c.black_queenside) );
         ( "capture on h1 corner clears white_kingside" >:: fun _ ->
           let b = B.empty () in
           B.set_castling b
             {
               B.white_kingside = true;
               B.white_queenside = true;
               B.black_kingside = false;
               B.black_queenside = false;
             };
           B.set b 7 7 (Some (B.Colored (B.White, B.Rook)));
           B.set b 6 7 (Some (B.Colored (B.Black, B.Pawn)));
           (* black pawn captures diagonally from h2 to h1? No — pawns don't
              capture forward. Use a black knight at f2 (5,5) attacking h1.   *)
           B.set b 6 7 None;
           B.set b 5 5 (Some (B.Colored (B.Black, B.Knight)));
           L.apply_move b L.Black_move (L.Move ((5, 5), (7, 7)));
           let c = B.get_castling b in
           assert_bool "white KS lost" (not c.white_kingside) );
         ( "capture on a1 clears white_queenside" >:: fun _ ->
           let b = B.empty () in
           B.set_castling b
             {
               B.white_kingside = true;
               B.white_queenside = true;
               B.black_kingside = false;
               B.black_queenside = false;
             };
           B.set b 7 0 (Some (B.Colored (B.White, B.Rook)));
           B.set b 5 1 (Some (B.Colored (B.Black, B.Knight)));
           L.apply_move b L.Black_move (L.Move ((5, 1), (7, 0)));
           let c = B.get_castling b in
           assert_bool "white QS lost" (not c.white_queenside) );
         ( "capture on h8 clears black_kingside" >:: fun _ ->
           let b = B.empty () in
           B.set_castling b
             {
               B.white_kingside = false;
               B.white_queenside = false;
               B.black_kingside = true;
               B.black_queenside = true;
             };
           B.set b 0 7 (Some (B.Colored (B.Black, B.Rook)));
           B.set b 2 6 (Some (B.Colored (B.White, B.Knight)));
           L.apply_move b L.White_move (L.Move ((2, 6), (0, 7)));
           let c = B.get_castling b in
           assert_bool "black KS lost" (not c.black_kingside) );
         ( "capture on a8 clears black_queenside" >:: fun _ ->
           let b = B.empty () in
           B.set_castling b
             {
               B.white_kingside = false;
               B.white_queenside = false;
               B.black_kingside = true;
               B.black_queenside = true;
             };
           B.set b 0 0 (Some (B.Colored (B.Black, B.Rook)));
           B.set b 2 1 (Some (B.Colored (B.White, B.Knight)));
           L.apply_move b L.White_move (L.Move ((2, 1), (0, 0)));
           let c = B.get_castling b in
           assert_bool "black QS lost" (not c.black_queenside) );
       ]

(* ── parse_coordinate / parse_command ─────────────────────────────────── *)

let parse_tests =
  "parse"
  >::: [
         ( "parse_coordinate a1" >:: fun _ ->
           assert_equal (Some (7, 0)) (L.parse_coordinate "a1") );
         ( "parse_coordinate h8" >:: fun _ ->
           assert_equal (Some (0, 7)) (L.parse_coordinate "h8") );
         ( "parse_coordinate uppercase" >:: fun _ ->
           assert_equal (Some (6, 4)) (L.parse_coordinate "E2") );
         ( "parse_coordinate empty" >:: fun _ ->
           assert_equal None (L.parse_coordinate "") );
         ( "parse_coordinate len 1" >:: fun _ ->
           assert_equal None (L.parse_coordinate "e") );
         ( "parse_coordinate len 3" >:: fun _ ->
           assert_equal None (L.parse_coordinate "e22") );
         ( "parse_coordinate bad file" >:: fun _ ->
           assert_equal None (L.parse_coordinate "i1") );
         ( "parse_coordinate file below 'a'" >:: fun _ ->
           (* digit-as-file: '1' < 'a', exercises [file < 'a'] = true branch *)
           assert_equal None (L.parse_coordinate "11") );
         ( "parse_coordinate bad rank low" >:: fun _ ->
           assert_equal None (L.parse_coordinate "a0") );
         ( "parse_coordinate bad rank high" >:: fun _ ->
           assert_equal None (L.parse_coordinate "a9") );
         ( "parse_command help" >:: fun _ ->
           assert_equal L.Help (L.parse_command "help") );
         ( "parse_command clear" >:: fun _ ->
           assert_equal L.Clear (L.parse_command "clear") );
         ( "parse_command empty -> Clear" >:: fun _ ->
           assert_equal L.Clear (L.parse_command "") );
         ( "parse_command spaces -> Clear" >:: fun _ ->
           assert_equal L.Clear (L.parse_command "    ") );
         ( "parse_command identify good" >:: fun _ ->
           assert_equal (L.Identify (6, 4)) (L.parse_command "identify e2") );
         ( "parse_command identify bad" >:: fun _ ->
           assert_equal L.Unknown (L.parse_command "identify zz") );
         ( "parse_command valid good" >:: fun _ ->
           assert_equal (L.Valid (6, 4)) (L.parse_command "valid e2") );
         ( "parse_command bare coord" >:: fun _ ->
           assert_equal (L.Valid (6, 4)) (L.parse_command "e2") );
         ( "parse_command bare bad coord" >:: fun _ ->
           assert_equal L.Unknown (L.parse_command "zz") );
         ( "parse_command move from to" >:: fun _ ->
           assert_equal
             (L.Move ((6, 4), (4, 4)))
             (L.parse_command "move e2 e4") );
         ( "parse_command move from bad" >:: fun _ ->
           assert_equal L.Unknown (L.parse_command "move zz e4") );
         ( "parse_command move to bad" >:: fun _ ->
           assert_equal L.Unknown (L.parse_command "move e2 zz") );
         ( "parse_command move bare" >:: fun _ ->
           assert_equal (L.Camel_move (4, 4)) (L.parse_command "move e4") );
         ( "parse_command move bare bad" >:: fun _ ->
           assert_equal L.Unknown (L.parse_command "move zz") );
         ( "parse_command garbage" >:: fun _ ->
           assert_equal L.Unknown (L.parse_command "foo bar baz qux") );
       ]

(* ── turn helpers ─────────────────────────────────────────────────────── *)

let turn_tests =
  "turn"
  >::: [
         ( "turn_color White_move" >:: fun _ ->
           assert_equal B.White (L.turn_color L.White_move) );
         ( "turn_color White_camel" >:: fun _ ->
           assert_equal B.White (L.turn_color L.White_camel) );
         ( "turn_color Black_move" >:: fun _ ->
           assert_equal B.Black (L.turn_color L.Black_move) );
         ( "turn_color Black_camel" >:: fun _ ->
           assert_equal B.Black (L.turn_color L.Black_camel) );
         ( "advance_turn cycle" >:: fun _ ->
           assert_equal L.White_camel (L.advance_turn L.White_move);
           assert_equal L.Black_move (L.advance_turn L.White_camel);
           assert_equal L.Black_camel (L.advance_turn L.Black_move);
           assert_equal L.White_move (L.advance_turn L.Black_camel) );
         ( "prompt_for all four" >:: fun _ ->
           let _ = L.prompt_for L.White_move in
           let _ = L.prompt_for L.White_camel in
           let _ = L.prompt_for L.Black_move in
           let _ = L.prompt_for L.Black_camel in
           () );
         ( "turn_label all four" >:: fun _ ->
           let _ = L.turn_label L.White_move in
           let _ = L.turn_label L.White_camel in
           let _ = L.turn_label L.Black_move in
           let _ = L.turn_label L.Black_camel in
           () );
       ]

(* ── valid_moves per piece ───────────────────────────────────────────── *)

let valid_moves_tests =
  "valid_moves"
  >::: [
         ( "empty square -> []" >:: fun _ ->
           let b = B.initial () in
           assert_equal [] (L.valid_moves b 4 4) );
         ( "neutral non-camel returns []" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Neutral B.King));
           assert_equal [] (L.valid_moves b 4 4) );
         ( "colored Camel returns []" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Colored (B.White, B.Camel)));
           assert_equal [] (L.valid_moves b 4 4) );
         ( "neutral camel = all empty squares" >:: fun _ ->
           let b = B.empty () in
           B.set b 3 3 (Some (B.Neutral B.Camel));
           let moves = L.valid_moves b 3 3 in
           (* 64 - 1 (the camel itself) = 63 *)
           assert_equal 63 (List.length moves);
           assert_bool "doesn't include own square"
             (not (List.mem (3, 3) moves)) );
         ( "white pawn from e2 has e3 and e4" >:: fun _ ->
           let b = B.initial () in
           let moves = L.valid_moves b 6 4 in
           assert_bool "e3" (List.mem (5, 4) moves);
           assert_bool "e4" (List.mem (4, 4) moves) );
         ( "white pawn blocked by piece in front" >:: fun _ ->
           let b = B.empty () in
           B.set b 6 4 (Some (B.Colored (B.White, B.Pawn)));
           B.set b 5 4 (Some (B.Colored (B.White, B.Knight)));
           assert_equal [] (L.valid_moves b 6 4) );
         ( "white pawn diagonal capture" >:: fun _ ->
           let b = B.empty () in
           B.set b 6 4 (Some (B.Colored (B.White, B.Pawn)));
           B.set b 5 5 (Some (B.Colored (B.Black, B.Knight)));
           let moves = L.valid_moves b 6 4 in
           assert_bool "captures" (List.mem (5, 5) moves) );
         ( "white pawn no diagonal on empty" >:: fun _ ->
           let b = B.empty () in
           B.set b 6 4 (Some (B.Colored (B.White, B.Pawn)));
           let moves = L.valid_moves b 6 4 in
           assert_bool "no diagonal" (not (List.mem (5, 5) moves)) );
         ( "black pawn from e7 has e6 and e5" >:: fun _ ->
           let b = B.initial () in
           let moves = L.valid_moves b 1 4 in
           assert_bool "e6" (List.mem (2, 4) moves);
           assert_bool "e5" (List.mem (3, 4) moves) );
         ( "knight from g1 reaches f3 and h3" >:: fun _ ->
           let b = B.initial () in
           let moves = L.valid_moves b 7 6 in
           assert_bool "f3" (List.mem (5, 5) moves);
           assert_bool "h3" (List.mem (5, 7) moves) );
         ( "rook on a1 with cleared file slides" >:: fun _ ->
           let b = B.empty () in
           B.set b 7 0 (Some (B.Colored (B.White, B.Rook)));
           let moves = L.valid_moves b 7 0 in
           assert_bool "a8" (List.mem (0, 0) moves);
           assert_bool "h1" (List.mem (7, 7) moves) );
         ( "rook blocked by camel" >:: fun _ ->
           let b = B.empty () in
           B.set b 7 0 (Some (B.Colored (B.White, B.Rook)));
           B.set b 4 0 (Some (B.Neutral B.Camel));
           let moves = L.valid_moves b 7 0 in
           assert_bool "stops before camel" (not (List.mem (4, 0) moves));
           assert_bool "stops before camel above"
             (not (List.mem (3, 0) moves)) );
         ( "rook captures enemy" >:: fun _ ->
           let b = B.empty () in
           B.set b 7 0 (Some (B.Colored (B.White, B.Rook)));
           B.set b 4 0 (Some (B.Colored (B.Black, B.Pawn)));
           let moves = L.valid_moves b 7 0 in
           assert_bool "can capture" (List.mem (4, 0) moves);
           assert_bool "stops past capture"
             (not (List.mem (3, 0) moves)) );
         ( "rook blocked by own piece" >:: fun _ ->
           let b = B.empty () in
           B.set b 7 0 (Some (B.Colored (B.White, B.Rook)));
           B.set b 4 0 (Some (B.Colored (B.White, B.Pawn)));
           let moves = L.valid_moves b 7 0 in
           assert_bool "no own-square" (not (List.mem (4, 0) moves)) );
         ( "bishop on d4 diagonal" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 3 (Some (B.Colored (B.White, B.Bishop)));
           let moves = L.valid_moves b 4 3 in
           assert_bool "a1 reach" (List.mem (7, 0) moves);
           assert_bool "h8 reach" (List.mem (0, 7) moves) );
         ( "queen combines rook+bishop" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 3 (Some (B.Colored (B.White, B.Queen)));
           let moves = L.valid_moves b 4 3 in
           assert_bool "rank d4-d8" (List.mem (0, 3) moves);
           assert_bool "diag a1" (List.mem (7, 0) moves) );
         ( "king step into own piece blocked" >:: fun _ ->
           let b = B.empty () in
           B.set b 7 4 (Some (B.Colored (B.White, B.King)));
           B.set b 7 5 (Some (B.Colored (B.White, B.Pawn)));
           let moves = L.valid_moves b 7 4 in
           assert_bool "no own f1" (not (List.mem (7, 5) moves)) );
         ( "knight from corner has fewer moves" >:: fun _ ->
           let b = B.empty () in
           B.set b 0 0 (Some (B.Colored (B.White, B.Knight)));
           let moves = L.valid_moves b 0 0 in
           assert_equal 2 (List.length moves) );
       ]

(* ── find_king / find_camel / is_attacked / checks ───────────────────── *)

let attack_tests =
  "attacks"
  >::: [
         ( "find_king on initial board" >:: fun _ ->
           let b = B.initial () in
           assert_equal (Some (7, 4)) (L.find_king b B.White);
           assert_equal (Some (0, 4)) (L.find_king b B.Black) );
         ( "find_king missing" >:: fun _ ->
           let b = B.empty () in
           assert_equal None (L.find_king b B.White) );
         ( "find_king with only opposite color present" >:: fun _ ->
           let b = B.empty () in
           B.set b 0 4 (Some (B.Colored (B.Black, B.King)));
           (* exercises the [c2 = color] guard's false branch *)
           assert_equal None (L.find_king b B.White);
           assert_equal (Some (0, 4)) (L.find_king b B.Black) );
         ( "find_camel missing" >:: fun _ ->
           let b = B.empty () in
           assert_equal None (L.find_camel b) );
         ( "find_camel present" >:: fun _ ->
           let b = B.initial () in
           assert_equal (Some (3, 3)) (L.find_camel b) );
         ( "is_attacked false on initial" >:: fun _ ->
           let b = B.initial () in
           assert_bool "white king safe"
             (not (L.is_attacked b (7, 4) B.Black)) );
         ( "is_attacked true by knight" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Colored (B.White, B.King)));
           B.set b 2 5 (Some (B.Colored (B.Black, B.Knight)));
           assert_bool "knight attacks e4 from f6"
             (L.is_attacked b (4, 4) B.Black) );
         ( "is_attacked excludes castling targets" >:: fun _ ->
           (* king alone with rights set: castling isn't a real attack *)
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
           assert_bool "g1 attacked by rook (sanity)"
             (L.is_attacked b (7, 6) B.White);
           B.set b 7 7 None;
           assert_bool "without rook, g1 isn't attacked via castling"
             (not (L.is_attacked b (7, 6) B.White)) );
         ( "checks none" >:: fun _ ->
           let b = B.initial () in
           assert_equal (None, None) (L.checks b) );
         ( "checks white only" >:: fun _ ->
           let b = B.empty () in
           B.set b 7 4 (Some (B.Colored (B.White, B.King)));
           B.set b 0 4 (Some (B.Colored (B.Black, B.King)));
           B.set b 7 0 (Some (B.Colored (B.Black, B.Rook)));
           let w, b' = L.checks b in
           assert_equal (Some (7, 4)) w;
           assert_equal None b' );
         ( "checks black only" >:: fun _ ->
           let b = B.empty () in
           B.set b 7 4 (Some (B.Colored (B.White, B.King)));
           B.set b 0 4 (Some (B.Colored (B.Black, B.King)));
           B.set b 0 0 (Some (B.Colored (B.White, B.Rook)));
           let w, b' = L.checks b in
           assert_equal None w;
           assert_equal (Some (0, 4)) b' );
         ( "checks no king -> None" >:: fun _ ->
           let b = B.empty () in
           assert_equal (None, None) (L.checks b) );
       ]

(* ── is_valid_move error branches ────────────────────────────────────── *)

let validate_tests =
  "is_valid_move"
  >::: [
         ( "no piece at source" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "No piece at source square.")
             (L.is_valid_move b L.White_move (L.Move ((4, 4), (5, 4)))) );
         ( "neutral piece in piece phase" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "Neutral pieces cannot be moved during the piece phase.")
             (L.is_valid_move b L.White_move (L.Move ((3, 3), (4, 4)))) );
         ( "wrong color" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "That piece does not belong to you.")
             (L.is_valid_move b L.White_move (L.Move ((1, 4), (2, 4)))) );
         ( "colored camel during piece phase" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Colored (B.White, B.Camel)));
           assert_equal
             (Error "Camels are moved during the camel phase.")
             (L.is_valid_move b L.White_move (L.Move ((4, 4), (5, 4)))) );
         ( "valid target ok" >:: fun _ ->
           let b = B.initial () in
           assert_equal (Ok ())
             (L.is_valid_move b L.White_move (L.Move ((6, 4), (4, 4)))) );
         ( "invalid target" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "That piece cannot move to that square.")
             (L.is_valid_move b L.White_move (L.Move ((6, 4), (3, 4)))) );
         ( "Camel_move during piece phase" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "Piece move phase. Use MOVE FROM TO. EX: MOVE E2 E4.")
             (L.is_valid_move b L.White_move (L.Camel_move (4, 4))) );
         ( "Move during camel phase" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "Camel move phase. Use MOVE TO. EX: MOVE E4.")
             (L.is_valid_move b L.White_camel (L.Move ((6, 4), (4, 4)))) );
         ( "Camel_move ok" >:: fun _ ->
           let b = B.initial () in
           assert_equal (Ok ())
             (L.is_valid_move b L.White_camel (L.Camel_move (4, 4))) );
         ( "Camel_move to occupied" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "Camel must move to an empty square.")
             (L.is_valid_move b L.White_camel (L.Camel_move (7, 4))) );
         ( "Camel_move with no camel" >:: fun _ ->
           let b = B.empty () in
           assert_equal
             (Error "No camel on the board.")
             (L.is_valid_move b L.White_camel (L.Camel_move (4, 4))) );
         ( "non-move command -> error" >:: fun _ ->
           let b = B.initial () in
           assert_equal (Error "Not a move command.")
             (L.is_valid_move b L.White_move L.Help) );
         ( "Black_move ok" >:: fun _ ->
           let b = B.initial () in
           assert_equal (Ok ())
             (L.is_valid_move b L.Black_move (L.Move ((1, 4), (3, 4)))) );
         ( "Black_move wrong color" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "That piece does not belong to you.")
             (L.is_valid_move b L.Black_move (L.Move ((6, 4), (4, 4)))) );
         ( "Black_move neutral source" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "Neutral pieces cannot be moved during the piece phase.")
             (L.is_valid_move b L.Black_move (L.Move ((3, 3), (4, 4)))) );
         ( "Black_move colored camel" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Colored (B.Black, B.Camel)));
           assert_equal
             (Error "Camels are moved during the camel phase.")
             (L.is_valid_move b L.Black_move (L.Move ((4, 4), (5, 4)))) );
         ( "Black_move empty source" >:: fun _ ->
           let b = B.empty () in
           assert_equal
             (Error "No piece at source square.")
             (L.is_valid_move b L.Black_move (L.Move ((4, 4), (5, 4)))) );
         ( "Black_move invalid target" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "That piece cannot move to that square.")
             (L.is_valid_move b L.Black_move (L.Move ((1, 4), (4, 4)))) );
         ( "Black_move with Camel_move -> piece phase err" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "Piece move phase. Use MOVE FROM TO. EX: MOVE E2 E4.")
             (L.is_valid_move b L.Black_move (L.Camel_move (4, 4))) );
         ( "Black_camel Camel_move ok" >:: fun _ ->
           let b = B.initial () in
           assert_equal (Ok ())
             (L.is_valid_move b L.Black_camel (L.Camel_move (4, 4))) );
         ( "Black_camel Camel_move bad target" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "Camel must move to an empty square.")
             (L.is_valid_move b L.Black_camel (L.Camel_move (0, 4))) );
         ( "Black_camel Camel_move no camel" >:: fun _ ->
           let b = B.empty () in
           assert_equal
             (Error "No camel on the board.")
             (L.is_valid_move b L.Black_camel (L.Camel_move (4, 4))) );
         ( "Black_camel Move -> camel phase err" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Error "Camel move phase. Use MOVE TO. EX: MOVE E4.")
             (L.is_valid_move b L.Black_camel (L.Move ((1, 4), (3, 4)))) );
       ]

(* ── apply_move branches ─────────────────────────────────────────────── *)

let apply_move_tests =
  "apply_move"
  >::: [
         ( "normal move" >:: fun _ ->
           let b = B.initial () in
           L.apply_move b L.White_move (L.Move ((6, 4), (4, 4)));
           assert_equal None (B.get b 6 4);
           assert_equal
             (Some (B.Colored (B.White, B.Pawn)))
             (B.get b 4 4) );
         ( "capture move" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Colored (B.White, B.Rook)));
           B.set b 4 7 (Some (B.Colored (B.Black, B.Pawn)));
           L.apply_move b L.White_move (L.Move ((4, 4), (4, 7)));
           assert_equal
             (Some (B.Colored (B.White, B.Rook)))
             (B.get b 4 7) );
         ( "apply_move with empty source no-ops" >:: fun _ ->
           let b = B.initial () in
           L.apply_move b L.White_move (L.Move ((4, 4), (3, 3)));
           assert_equal
             (Some (B.Neutral B.Camel))
             (B.get b 3 3) );
         ( "camel relocation" >:: fun _ ->
           let b = B.initial () in
           L.apply_move b L.White_camel (L.Camel_move (4, 4));
           assert_equal None (B.get b 3 3);
           assert_equal
             (Some (B.Neutral B.Camel))
             (B.get b 4 4) );
         ( "camel relocation when no camel no-ops" >:: fun _ ->
           let b = B.empty () in
           L.apply_move b L.White_camel (L.Camel_move (4, 4));
           assert_equal None (B.get b 4 4) );
         ( "apply_move with unrelated cmd" >:: fun _ ->
           let b = B.initial () in
           L.apply_move b L.White_move L.Help;
           assert_equal
             (Some (B.Colored (B.White, B.Pawn)))
             (B.get b 6 4) );
         ( "Black_move regular" >:: fun _ ->
           let b = B.initial () in
           L.apply_move b L.Black_move (L.Move ((1, 4), (3, 4)));
           assert_equal None (B.get b 1 4);
           assert_equal
             (Some (B.Colored (B.Black, B.Pawn)))
             (B.get b 3 4) );
         ( "Black_camel relocation" >:: fun _ ->
           let b = B.initial () in
           L.apply_move b L.Black_camel (L.Camel_move (4, 4));
           assert_equal None (B.get b 3 3);
           assert_equal
             (Some (B.Neutral B.Camel))
             (B.get b 4 4) );
         ( "Black_camel relocation no camel no-op" >:: fun _ ->
           let b = B.empty () in
           L.apply_move b L.Black_camel (L.Camel_move (4, 4));
           assert_equal None (B.get b 4 4) );
         ( "white king move clears both white rights" >:: fun _ ->
           let b = B.initial () in
           B.set b 7 5 None;
           B.set b 7 6 None;
           L.apply_move b L.White_move (L.Move ((7, 4), (7, 5)));
           let c = B.get_castling b in
           assert_bool "ks gone" (not c.white_kingside);
           assert_bool "qs gone" (not c.white_queenside) );
         ( "black king move clears both black rights" >:: fun _ ->
           let b = B.initial () in
           B.set b 0 5 None;
           B.set b 0 6 None;
           L.apply_move b L.Black_move (L.Move ((0, 4), (0, 5)));
           let c = B.get_castling b in
           assert_bool "ks gone" (not c.black_kingside);
           assert_bool "qs gone" (not c.black_queenside) );
       ]

(* ── describe_square / describe_valid / evaluate_input ────────────────── *)

let describe_tests =
  "describe"
  >::: [
         ( "describe_square empty" >:: fun _ ->
           let b = B.initial () in
           assert_equal "e4 is empty." (L.describe_square b 4 4) );
         ( "describe_square white king" >:: fun _ ->
           let b = B.initial () in
           assert_equal "e1 contains a white king."
             (L.describe_square b 7 4) );
         ( "describe_square black queen" >:: fun _ ->
           let b = B.initial () in
           assert_equal "d8 contains a black queen."
             (L.describe_square b 0 3) );
         ( "describe_square neutral camel" >:: fun _ ->
           let b = B.initial () in
           assert_equal "d5 contains a neutral camel."
             (L.describe_square b 3 3) );
         ( "describe_square neutral non-camel" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Neutral B.Knight));
           assert_equal "e4 contains a neutral knight."
             (L.describe_square b 4 4) );
         ( "all kind_name branches via describe_square" >:: fun _ ->
           let b = B.empty () in
           List.iter
             (fun (kind, _) ->
               B.set b 4 4 (Some (B.Colored (B.White, kind)));
               let _ = L.describe_square b 4 4 in
               ())
             [
               (B.King, "k");
               (B.Queen, "q");
               (B.Rook, "r");
               (B.Bishop, "b");
               (B.Knight, "n");
               (B.Pawn, "p");
               (B.Camel, "c");
             ] );
         ( "describe_valid out of bounds" >:: fun _ ->
           let b = B.initial () in
           assert_equal "Invalid coordinate." (L.describe_valid b 9 0 []) );
         ( "describe_valid empty square" >:: fun _ ->
           let b = B.initial () in
           assert_equal "No piece at e4." (L.describe_valid b 4 4 []) );
         ( "describe_valid with no targets" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Colored (B.White, B.King)));
           let s = L.describe_valid b 4 4 [] in
           assert_bool "mentions no legal" (String.length s > 0) );
         ( "describe_valid with targets" >:: fun _ ->
           let b = B.initial () in
           let s = L.describe_valid b 6 4 [ (4, 4) ] in
           assert_bool "mentions can move" (String.length s > 0) );
         ( "describe_valid neutral camel" >:: fun _ ->
           let b = B.initial () in
           let _ = L.describe_valid b 3 3 [ (4, 4) ] in
           () );
         ( "describe_valid neutral non-camel" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Neutral B.Knight));
           let _ = L.describe_valid b 4 4 [] in
           () );
         ( "describe_valid black piece" >:: fun _ ->
           let b = B.initial () in
           let _ = L.describe_valid b 0 0 [] in
           () );
         ( "evaluate_input help" >:: fun _ ->
           let b = B.initial () in
           let s = L.evaluate_input b "help" in
           assert_bool "non-empty" (String.length s > 0) );
         ( "evaluate_input clear" >:: fun _ ->
           let b = B.initial () in
           assert_equal "Cleared." (L.evaluate_input b "clear") );
         ( "evaluate_input identify" >:: fun _ ->
           let b = B.initial () in
           assert_equal "e4 is empty."
             (L.evaluate_input b "identify e4") );
         ( "evaluate_input valid -> empty" >:: fun _ ->
           let b = B.initial () in
           assert_equal "" (L.evaluate_input b "valid e2") );
         ( "evaluate_input move -> empty" >:: fun _ ->
           let b = B.initial () in
           assert_equal "" (L.evaluate_input b "move e2 e4") );
         ( "evaluate_input camel move -> empty" >:: fun _ ->
           let b = B.initial () in
           assert_equal "" (L.evaluate_input b "move e4") );
         ( "evaluate_input unknown" >:: fun _ ->
           let b = B.initial () in
           let s = L.evaluate_input b "garbage stuff" in
           assert_bool "non-empty" (String.length s > 0) );
       ]

(* ── Board basics ─────────────────────────────────────────────────────── *)

let board_tests =
  "board"
  >::: [
         ( "camel starts in center" >:: fun _ ->
           let b = B.initial () in
           assert_equal (Some (B.Neutral B.Camel)) (B.get b 3 3) );
         ( "white king e1" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Some (B.Colored (B.White, B.King)))
             (B.get b 7 4) );
         ( "black queen d8" >:: fun _ ->
           let b = B.initial () in
           assert_equal
             (Some (B.Colored (B.Black, B.Queen)))
             (B.get b 0 3) );
         ( "is_occupied true" >:: fun _ ->
           let b = B.initial () in
           assert_bool "e1" (B.is_occupied b 7 4) );
         ( "is_occupied false" >:: fun _ ->
           let b = B.initial () in
           assert_bool "e4 empty" (not (B.is_occupied b 4 4)) );
         ( "is_occupied raises out of bounds" >:: fun _ ->
           let b = B.initial () in
           assert_raises (Invalid_argument "Board.is_occupied") (fun () ->
               ignore (B.is_occupied b 8 0)) );
         ( "in_bounds boundaries" >:: fun _ ->
           assert_bool "(0,0)" (B.in_bounds 0 0);
           assert_bool "(7,7)" (B.in_bounds 7 7);
           assert_bool "(-1,0)" (not (B.in_bounds (-1) 0));
           assert_bool "(0,8)" (not (B.in_bounds 0 8)) );
         ( "empty has all-empty squares" >:: fun _ ->
           let b = B.empty () in
           let any = ref false in
           for r = 0 to 7 do
             for c = 0 to 7 do
               if B.get b r c <> None then any := true
             done
           done;
           assert_bool "all empty" (not !any) );
         ( "empty has all castling false" >:: fun _ ->
           let c = B.get_castling (B.empty ()) in
           assert_bool "wks" (not c.white_kingside);
           assert_bool "wqs" (not c.white_queenside);
           assert_bool "bks" (not c.black_kingside);
           assert_bool "bqs" (not c.black_queenside) );
         ( "initial has all castling true" >:: fun _ ->
           let c = B.get_castling (B.initial ()) in
           assert_bool "wks" c.white_kingside;
           assert_bool "wqs" c.white_queenside;
           assert_bool "bks" c.black_kingside;
           assert_bool "bqs" c.black_queenside );
         ( "set then get round-trips" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Neutral B.Camel));
           assert_equal (Some (B.Neutral B.Camel)) (B.get b 4 4) );
       ]

(* ── Render smoke tests (headless via SDL_VIDEODRIVER=dummy) ──────────── *)

let render_setup () =
  Unix.putenv "SDL_VIDEODRIVER" "dummy";
  match Sdl.init Sdl.Init.(video + events) with
  | Error _ -> None
  | Ok () -> (
      match
        Sdl.create_window ~w:Camel_chess.Render.window_width
          ~h:Camel_chess.Render.window_height "test" Sdl.Window.hidden
      with
      | Error _ ->
          Sdl.quit ();
          None
      | Ok window -> (
          match Sdl.create_renderer window with
          | Error _ ->
              Sdl.destroy_window window;
              Sdl.quit ();
              None
          | Ok renderer -> Some (window, renderer)))

let render_teardown window renderer =
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window;
  Sdl.quit ()

(* Drive every screen + draw branch once so render.ml gets coverage. *)
let drive_render renderer =
  let view = Camel_chess.Render.create renderer in
  let board = Camel_chess.Board.initial () in
  (* basic draw with defaults *)
  Camel_chess.Render.draw view board;
  (* draw with full state, selected (occupied + capture target highlight),
     check message + check_squares, flipped on/off, status that wraps *)
  Camel_chess.Render.draw view board ~input:"move e2 e4"
    ~status:"this is a fairly long status string that should wrap into multiple lines on the panel"
    ~turn:"WHITE - PIECE MOVE" ~check:"Check on White!" ~check_squares:[ (7, 4) ]
    ~hint:"TYPE EXIT OR RESTART." ~selected:(6, 4)
    ~targets:[ (4, 4); (5, 4); (1, 0) ]
    ~flipped:false;
  Camel_chess.Render.draw view board ~flipped:true ~selected:(0, 0)
    ~targets:[ (0, 1) ];
  (* very-long-word path in wrap_text: word longer than panel *)
  Camel_chess.Render.draw view board
    ~status:(String.make 200 'x')
    ~turn:""
    ~input:"";
  (* title / rules / multiplayer / host_waiting / client_connecting *)
  Camel_chess.Render.draw_title view;
  Camel_chess.Render.draw_title view ~input:"1" ~status:"TYPE 1, 2, OR 3";
  Camel_chess.Render.draw_rules view;
  Camel_chess.Render.draw_multiplayer_menu view;
  Camel_chess.Render.draw_multiplayer_menu view ~input:"1" ~status:"WAITING";
  Camel_chess.Render.draw_host_waiting view ~ip:"192.168.1.42" ~port:9876;
  Camel_chess.Render.draw_client_connecting view;
  Camel_chess.Render.draw_client_connecting view ~input:"127.0.0.1"
    ~status:"CONNECTING";
  Camel_chess.Render.destroy view

let render_tests =
  "render smoke"
  >::: [
         ( "drive every screen" >:: fun _ ->
           match render_setup () with
           | None -> skip_if true "SDL unavailable in this environment"
           | Some (window, renderer) ->
               (try drive_render renderer
                with e ->
                  render_teardown window renderer;
                  raise e);
               render_teardown window renderer );
       ]

let promotion_tests =
  "promotion"
  >::: [
         ( "pending_promotion empty" >:: fun _ ->
           let b = B.initial () in
           assert_equal None (L.pending_promotion b) );
         ( "pending_promotion white pawn at rank 8" >:: fun _ ->
           let b = B.empty () in
           B.set b 0 4 (Some (B.Colored (B.White, B.Pawn)));
           assert_equal (Some (0, 4)) (L.pending_promotion b) );
         ( "pending_promotion black pawn at rank 1" >:: fun _ ->
           let b = B.empty () in
           B.set b 7 4 (Some (B.Colored (B.Black, B.Pawn)));
           assert_equal (Some (7, 4)) (L.pending_promotion b) );
         ( "pending_promotion ignores pawn mid-board" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Colored (B.White, B.Pawn)));
           assert_equal None (L.pending_promotion b) );
         ( "promote_pawn to queen preserves color" >:: fun _ ->
           let b = B.empty () in
           B.set b 0 4 (Some (B.Colored (B.White, B.Pawn)));
           L.promote_pawn b (0, 4) B.Queen;
           assert_equal
             (Some (B.Colored (B.White, B.Queen)))
             (B.get b 0 4) );
         ( "promote_pawn to knight" >:: fun _ ->
           let b = B.empty () in
           B.set b 7 4 (Some (B.Colored (B.Black, B.Pawn)));
           L.promote_pawn b (7, 4) B.Knight;
           assert_equal
             (Some (B.Colored (B.Black, B.Knight)))
             (B.get b 7 4) );
         ( "promote_pawn to rook" >:: fun _ ->
           let b = B.empty () in
           B.set b 0 0 (Some (B.Colored (B.White, B.Pawn)));
           L.promote_pawn b (0, 0) B.Rook;
           assert_equal
             (Some (B.Colored (B.White, B.Rook)))
             (B.get b 0 0) );
         ( "promote_pawn to bishop" >:: fun _ ->
           let b = B.empty () in
           B.set b 0 0 (Some (B.Colored (B.White, B.Pawn)));
           L.promote_pawn b (0, 0) B.Bishop;
           assert_equal
             (Some (B.Colored (B.White, B.Bishop)))
             (B.get b 0 0) );
         ( "promote_pawn to king coerces to queen" >:: fun _ ->
           let b = B.empty () in
           B.set b 0 4 (Some (B.Colored (B.White, B.Pawn)));
           L.promote_pawn b (0, 4) B.King;
           assert_equal
             (Some (B.Colored (B.White, B.Queen)))
             (B.get b 0 4) );
         ( "promote_pawn no-op without a pawn" >:: fun _ ->
           let b = B.empty () in
           B.set b 0 4 (Some (B.Colored (B.White, B.Knight)));
           L.promote_pawn b (0, 4) B.Queen;
           assert_equal
             (Some (B.Colored (B.White, B.Knight)))
             (B.get b 0 4) );
         ( "parse_command promote q / queen" >:: fun _ ->
           assert_equal (L.Promote B.Queen) (L.parse_command "promote q");
           assert_equal (L.Promote B.Queen) (L.parse_command "promote queen") );
         ( "parse_command promote r / rook" >:: fun _ ->
           assert_equal (L.Promote B.Rook) (L.parse_command "promote r");
           assert_equal (L.Promote B.Rook) (L.parse_command "promote rook") );
         ( "parse_command promote b / bishop" >:: fun _ ->
           assert_equal (L.Promote B.Bishop) (L.parse_command "promote b");
           assert_equal (L.Promote B.Bishop)
             (L.parse_command "promote bishop") );
         ( "parse_command promote n / knight" >:: fun _ ->
           assert_equal (L.Promote B.Knight) (L.parse_command "promote n");
           assert_equal (L.Promote B.Knight)
             (L.parse_command "promote knight") );
         ( "parse_command promote unknown" >:: fun _ ->
           assert_equal L.Unknown (L.parse_command "promote x") );
         ( "evaluate_input promote returns empty" >:: fun _ ->
           let b = B.initial () in
           assert_equal "" (L.evaluate_input b "promote q") );
       ]

let en_passant_tests =
  "en_passant"
  >::: [
         ( "double-step sets en_passant target" >:: fun _ ->
           let b = B.initial () in
           L.apply_move b L.White_move (L.Move ((6, 4), (4, 4)));
           assert_equal (Some (5, 4)) (B.get_en_passant b) );
         ( "single step doesn't set en_passant" >:: fun _ ->
           let b = B.initial () in
           L.apply_move b L.White_move (L.Move ((6, 4), (5, 4)));
           assert_equal None (B.get_en_passant b) );
         ( "non-pawn move clears en_passant" >:: fun _ ->
           let b = B.initial () in
           B.set_en_passant b (Some (5, 4));
           L.apply_move b L.White_move (L.Move ((7, 6), (5, 5)));
           assert_equal None (B.get_en_passant b) );
         ( "camel relocation preserves en_passant" >:: fun _ ->
           let b = B.initial () in
           B.set_en_passant b (Some (5, 4));
           L.apply_move b L.White_camel (L.Camel_move (4, 4));
           assert_equal (Some (5, 4)) (B.get_en_passant b) );
         ( "white pawn sees en_passant capture in valid_moves" >:: fun _ ->
           let b = B.empty () in
           B.set b 3 4 (Some (B.Colored (B.White, B.Pawn)));
           B.set b 3 5 (Some (B.Colored (B.Black, B.Pawn)));
           B.set_en_passant b (Some (2, 5));
           let moves = L.valid_moves b 3 4 in
           assert_bool "diagonal-to-ep target"
             (List.mem (2, 5) moves) );
         ( "black pawn sees en_passant capture" >:: fun _ ->
           let b = B.empty () in
           B.set b 4 4 (Some (B.Colored (B.Black, B.Pawn)));
           B.set b 4 3 (Some (B.Colored (B.White, B.Pawn)));
           B.set_en_passant b (Some (5, 3));
           let moves = L.valid_moves b 4 4 in
           assert_bool "diagonal-to-ep target"
             (List.mem (5, 3) moves) );
         ( "no diagonal move when ep is unrelated" >:: fun _ ->
           let b = B.empty () in
           B.set b 3 4 (Some (B.Colored (B.White, B.Pawn)));
           B.set_en_passant b (Some (0, 0));
           let moves = L.valid_moves b 3 4 in
           assert_bool "no e-p" (not (List.mem (2, 5) moves)) );
         ( "diagonal blocked by own piece exercises fallthrough" >:: fun _ ->
           let b = B.empty () in
           B.set b 3 4 (Some (B.Colored (B.White, B.Pawn)));
           B.set b 2 5 (Some (B.Colored (B.White, B.Knight)));
           let moves = L.valid_moves b 3 4 in
           assert_bool "no own-square capture"
             (not (List.mem (2, 5) moves)) );
         ( "en_passant capture removes the captured pawn" >:: fun _ ->
           let b = B.empty () in
           (* Set up: white pawn just doubled to e5; it's black's turn and
              their pawn at d5 (we set up the squares manually). *)
           B.set b 3 4 (Some (B.Colored (B.White, B.Pawn)));
           B.set b 3 3 (Some (B.Colored (B.Black, B.Pawn)));
           B.set_en_passant b (Some (2, 4));
           L.apply_move b L.Black_move (L.Move ((3, 3), (2, 4)));
           assert_equal
             (Some (B.Colored (B.Black, B.Pawn)))
             (B.get b 2 4);
           assert_equal None (B.get b 3 4);
           (* en_passant should now be cleared *)
           assert_equal None (B.get_en_passant b) );
         ( "regular diagonal capture still works" >:: fun _ ->
           let b = B.empty () in
           B.set b 6 4 (Some (B.Colored (B.White, B.Pawn)));
           B.set b 5 5 (Some (B.Colored (B.Black, B.Knight)));
           L.apply_move b L.White_move (L.Move ((6, 4), (5, 5)));
           assert_equal
             (Some (B.Colored (B.White, B.Pawn)))
             (B.get b 5 5);
           assert_equal None (B.get_en_passant b) );
         ( "double-step blocked by camel doesn't set en_passant" >:: fun _ ->
           let b = B.empty () in
           B.set b 6 4 (Some (B.Colored (B.White, B.Pawn)));
           B.set b 5 4 (Some (B.Neutral B.Camel));
           let moves = L.valid_moves b 6 4 in
           assert_bool "no forward at all" (moves = []) );
         ( "Board.empty has en_passant=None" >:: fun _ ->
           assert_equal None (B.get_en_passant (B.empty ())) );
         ( "Board.set_en_passant + get round-trip" >:: fun _ ->
           let b = B.empty () in
           B.set_en_passant b (Some (4, 4));
           assert_equal (Some (4, 4)) (B.get_en_passant b) );
       ]

let suite =
  "all"
  >::: [
         board_tests;
         parse_tests;
         turn_tests;
         valid_moves_tests;
         attack_tests;
         validate_tests;
         apply_move_tests;
         describe_tests;
         castling_tests;
         promotion_tests;
         en_passant_tests;
         render_tests;
       ]

let _ = run_test_tt_main suite
