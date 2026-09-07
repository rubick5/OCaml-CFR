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
let infoset: Strategy.infoset = {
        player = Game.P1;
        player_card = Game.J;
        history = [];
      }


let () =
  match Strategy.full_tree with
    | Ok t ->
      let tbl = Strategy.build_regret_table t in
      let new_tbl = Strategy.run_iterations 100000 tbl t in
      let s = Strategy.extract_strategy new_tbl in
      print_endline (Strategy.show_strategy s)


    | Error s -> print_endline s


(*
let () =
  match Strategy.full_tree with
    | Ok t -> 
      print_endline (Strategy.show_game_tree t);
      let tbl = Strategy.build_regret_table t in
      print_endline (Strategy.print_table (tbl));

    | Error s -> print_endline s  *)