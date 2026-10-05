open Leduc.Strategy
open Leduc

let g : G.game_state = {
  p1_card = { value = Deck.J ; suit = Deck.H };
  p2_card = { value = Deck.Q ; suit = Deck.S };
  board = None;
  round_history = P1BetP2Raise;
  game_history = Nothing;
}

let () =
  (*print_endline (show_game_tree full_tree)*)
  (* now to dump the infoset table *)
  let tbl = build_regret_table full_tree in
  let new_tbl = run_iterations 10000 tbl full_tree in
  let s = extract_strategy new_tbl in
  print_endline (show_strategy s)