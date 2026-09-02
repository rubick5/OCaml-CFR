Random.self_init ();;

let s1: Strategy.p1_strategy = {
  start_weights = (1.0, 0.5, 0.2);
  facing_weights = (0.2, 0.2, 0.4);
}

let s2: Strategy.p2_strategy = {
  facing_bet_weights = (0.2, 0.3, 1.0);
  facing_check_weights = (0.1, 0.5, 0.9);
}
let cards : Game.game_cards = {
  p1_card = Game.Q;
  p2_card = Game.K;
}

let () =
  let e = Game.run_game cards (Strategy.strategy_to_p1 s1) (Strategy.strategy_to_p2 s2)
  in print_endline (Game.show_game_end_state e)