type round_history
type game_history
type game_state = {
  p1_card : Deck.card;
  p2_card : Deck.card;
  round_history : round_history;
  game_history : game_history;
} [@@deriving show]

type infoset [@@deriving show]
val infoset_from : game_state -> infoset


type action = Bet | Call | Fold | Check | Raise [@@deriving show]

val legal_steps : game_state -> (action * game_state) list