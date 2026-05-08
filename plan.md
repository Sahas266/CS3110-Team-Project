# Camel Chess — Plan

A chess variant where a neutral, immortal camel must be relocated each turn.

## Done

### Core data model
- [x] `Board.color`, `Board.kind`, `Board.piece` (Colored / Neutral) types
- [x] 8×8 board as `square array array`; `t` abstract in `.mli`
- [x] `empty`, `initial`, `get`, `set`, `in_bounds`, `is_occupied`
- [x] Standard back-rank + pawn setup for both colors
- [x] Neutral camel placed at d5 (row 3, col 3) on init

### Commands & rules (`lib/logic.ml`)
- [x] `parse_coordinate` (algebraic → row/col), case-insensitive
- [x] `parse_command` for `help`, `clear`, `identify <sq>`, `valid <sq>`, bare `<sq>`
- [x] `parse_command` for `move <from> <to>` (piece) and `move <to>` (camel)
- [x] `describe_square` / `describe_valid` text output
- [x] `valid_moves` for King, Queen, Rook, Bishop, Knight, Pawn (incl. double-step + diagonal capture), and Camel (any empty square)
- [x] Sliding pieces correctly blocked by neutral camel
- [x] `type turn = White_move | White_camel | Black_move | Black_camel`
- [x] `turn_color`, `advance_turn` (cycles W_move → W_camel → B_move → B_camel → W_move)
- [x] `prompt_for`, `turn_label` for user-facing strings
- [x] `find_camel` — locates neutral camel on the board
- [x] `is_valid_move` — phase match + color ownership + `valid_moves` membership
- [x] `apply_move` — mutates the board for piece or camel relocation
- [x] **Castling** (kingside + queenside): `Board.castling` rights record, `get_castling`/`set_castling`, `valid_moves` for King includes castling destinations only when occupancy/rights/check/transit-attack are all clear; `apply_move` moves the rook, clears the rights, blocks `is_attacked` from recursing through castling
- [x] **En passant**: `Board.get/set_en_passant` tracks the skipped square; pawn double-step sets it, any other piece move clears it, camel relocation preserves it across the moving side's camel phase; pawn `valid_moves` includes the en passant target as a diagonal capture; `apply_move` removes the captured pawn at `(sr, dc)` when the diagonal lands on the target
- [x] **Pawn promotion**: `pending_promotion` detects a pawn on its promotion rank; `promote_pawn` swaps it for Queen / Rook / Bishop / Knight while preserving color (any other kind coerces to Queen); a new `Promote of Board.kind` command parses `promote q/r/b/n` (or full names); `bin/main.ml` pauses turn advancement until the user picks a piece, syncs the choice to peers in multiplayer

### UI / rendering (TSDL)
- [x] Migrated from Notty to TSDL (commits 6d164c8, 2bcdac2)
- [x] SDL window, renderer, event loop
- [x] Resizable window with `render_set_logical_size` so the board scales
- [x] Text input, backspace, Enter, Escape
- [x] Board rendering with piece graphics
- [x] Modern-style chess piece art
- [x] Selection highlight (green) and valid-move dots / capture brackets
- [x] **File labels (A–H)** on top and bottom edges; **rank labels (1–8)** on left and right
- [x] **Turn indicator** line in info panel (gold) showing which color + phase
- [x] Info panel cleared from board labels, grown to fit hint line
- [x] Status line + window title + stdout transcript
- [x] Scaled-text helpers (`draw_text_scaled`, `center_text`) for headings
- [x] Title / main-menu screen with Singleplayer / Multiplayer / Rules options
- [x] Rules screen explaining variant + command syntax
- [x] Multiplayer mode-select screen (Host / Join)
- [x] Host-waiting screen (`draw_host_waiting`) showing local IP + port while listening
- [x] Client-connecting screen (`draw_client_connecting`) for entering a host IP
- [x] **Board flip**: in Solo mode the board rotates 180° so the side to move faces the player; Host fixed white-facing, Client fixed black-facing
- [x] Extra glyphs: `[`, `]`, `!`, `_`, `'`

### Multiplayer (TCP)
- [x] `Host` opens a TCP listener on port 9876, shows local IP, accepts one client (non-blocking poll)
- [x] `Client` connects to a host IP, transitions to game on success
- [x] Local IP detection with WSL fallback that shells out to PowerShell `Get-NetIPAddress`
- [x] Per-move sync: each side sends its move string, peer applies it via `apply_peer_move`
- [x] `is_my_turn` gating — input on the wrong side is rejected with "Wait for your turn."
- [x] `recv_buf` buffered line-based protocol (`try_read_line`)
- [x] Connection lifecycle: `create_server`, `poll_accept`, `connect_to`, `send_move`, `close_fd`

### Check / checkmate
- [x] `find_king`, `is_attacked`, `checks` in `Logic`
- [x] Red "Check on White/Black!" notice in info panel after each move
- [x] King's square painted red when in check (selection-green still wins)
- [x] **Win detection**: king capture ends the game and declares the winner
- [x] Game-over freeze: only `exit` and `restart` accepted while `winner` is set
- [x] `restart` command rebuilds a fresh game in the same mode

### Tests (`test/test_camel_chess.ml`)
- [x] OUnit suite wired through dune
- [x] Board init: camel at d5, white king at e1, black queen at d8
- [x] `is_occupied` happy + out-of-bounds path
- [x] `parse_coordinate` / `parse_command` basics
- [x] `evaluate_input` for `identify` on occupied + empty squares
- [x] Castling suite: kingside legal in clean fixture, blocked by occupancy on f1, blocked when king is in check, blocked when transit square is attacked, blocked when rights flag is off, `apply_move` correctly moves rook and clears both rights

### Project scaffolding
- [x] dune-project, lib/bin/test dune files
- [x] `camel_chess.opam`
- [x] INSTALL.md with tsdl + run instructions, including macOS section
- [x] gallery.yaml filled in (title, group, PM, description)
- [x] AUTHORS.md filled with team roster + GenAI acknowledgement
- [x] RepoURL.txt added
- [x] Per-unit warning flag set drafted in `lib/dune` and `bin/dune` (currently commented out)

---

## To Do

### Gameplay rules

> **Note:** In this variant, checkmate is defined as **capturing the king** — there is no "in check" state to escape from, and the game ends the instant a king is taken. This simplifies detection: no need to filter moves that leave the king in check, no need for check/checkmate/stalemate separation. Whichever side captures the opposing king wins.
>
> **Corollary:** The king is allowed to move into check (i.e. onto a square attacked by the opponent). It's simply a losing blunder — the opponent captures it next turn — rather than an illegal move. Players are responsible for their own king's safety.

- [ ] Captured-piece tracking
- [ ] Enforce camel-immortality defensively (today enforced only via `valid_moves` never returning a square occupied by a piece — confirm no path can replace the camel)
- [ ] Move history + `undo`

### Multiplayer follow-ups
- [ ] Network protocol extensions: resign, restart, graceful disconnect notice
- [ ] Surface connection drops / socket errors in the info panel rather than failing silently
- [ ] Configurable port (currently hard-coded to 9876)
- [ ] Multiplayer game-over: handle `restart` collaboratively (currently each side restarts independently)

### UI polish
- [ ] Show captured pieces / material count
- [ ] Highlight the last move (source + destination) across redraws
- [ ] On-screen help overlay (not just `help` text)
- [ ] Mouse input (click to select / move) in addition to typed commands
- [ ] DPI handling for the SDL window
- [ ] Visual feedback when a command is rejected (brief flash on the input box)

### Tests
- [ ] `valid_moves` per piece type (sliding blocked by own/enemy/camel, edges)
- [ ] Pawn: starting double-step, blocked, diagonal capture only on enemy
- [ ] Camel valid moves = all empty squares; cannot land on occupied
- [ ] `is_valid_move` per phase and color (phase mismatch, wrong color, camel in piece phase, neutral source)
- [ ] `apply_move` for capture and for camel relocation
- [ ] `advance_turn` cycle
- [ ] `find_camel` on a board with and without a camel
- [ ] `find_king`, `is_attacked`, `checks` (no check, single check, both kings in check, missing king)
- [ ] `detect_winner` on king-capture and on the opening position
- [ ] Parser: `move` variants, malformed input, extra whitespace
- [ ] 100% Bisect Coverage

### Docs / deliverables
- [ ] Flesh out `README.md` (currently only author list) — overview, rules, screenshots, build/run
- [ ] Doc comments on new `Logic` additions (`turn`, `is_valid_move`, `apply_move`, etc.)
- [ ] Demo video (gallery.yaml currently `N/A`)
- [ ] AUTHORS.md content review
- [ ] Note SDL2 system dep in INSTALL.md (`libsdl2-dev` on Ubuntu/WSL) in addition to `opam install tsdl`
- [ ] Decide whether to re-enable the commented-out warning flags and clean any warnings they surface

### Stretch
- [ ] Simple AI opponent (random legal move, then minimax)
- [ ] Save / load a game to disk
- [ ] PGN-style move log export
- [ ] Clock / time control
