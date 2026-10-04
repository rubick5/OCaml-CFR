open Leduc.Game
open Leduc

let g : game_state = {
  p1_card = { value = Deck.J ; suit = Deck.H };
  p2_card = { value = Deck.Q ; suit = Deck.S };
  board = None;
  round_history = P1BetP2Raise;
  game_history = Nothing;
}

let () =
  List.iter (fun (a, g) ->
    print_endline ("Action " ^ (show_action a) ^ " goes to:");
    print_endline (show_game_state g);
    print_endline "\n\n"
  ) (legal_steps g)