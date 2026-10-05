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
  let i = build_regret_table full_tree in
  print_endline (print_table i)