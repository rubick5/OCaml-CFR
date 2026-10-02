module G = Game

type infoset = {
  player : G.player;
  card_value : G.Deck.value; (* only card value because suit doesn't change the infoset *)
  round_history : G.round_history;
  game_history : G.full_round_history option;
}

type infoset_data = {
  regret : (G.action * float) list;
  strategy_sum : (G.action * float) list;
} [@@deriving show]