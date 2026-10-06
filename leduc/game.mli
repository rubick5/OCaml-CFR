(* Rules of Leduc hold'em: betting rounds, board deal, payoffs and infosets.
   Internal helpers (pot sizes, bet sizes, history accumulation) stay hidden. *)

type player = P1 | P2 [@@deriving show]

type action = Bet | Call | Fold | Check | Raise [@@deriving show]

(* ---- betting histories ---------------------------------------------- *)

(* a betting round that has finished, either matched or with a fold *)
type full_round_history =
  | P1P2Check
  | P1BetP2Call
  | P1BetP2Fold
  | P2BetP1Call
  | P2BetP1Fold
  | P2RaiseP1Call
  | P2RaiseP1Fold
  | P1RaiseP2Call
  | P1RaiseP2Fold
  [@@deriving show]

(* a betting round still in progress *)
type round_history =
  | Nothing
  | P1Check
  | P1Bet
  | P2Bet
  | P1BetP2Raise
  | P2BetP1Raise
  [@@deriving show]

(* the rounds finished so far *)
type game_history =
  | Nothing
  | OneRound of full_round_history
  | TwoRounds of full_round_history * full_round_history
  [@@deriving show]

(* which player folded to end the round, if any *)
val has_fold : full_round_history -> player option

(* whose turn it is in a round in progress *)
val turn : round_history -> player

val legal_actions : round_history -> action list

(* Left: the round continues. Right: the round is finished. *)
val apply_action :
  action -> round_history ->
  ((round_history, full_round_history) Either.t, string) Result.t

(* ---- game state ----------------------------------------------------- *)

type game_state = {
  p1_card : Deck.card;
  p2_card : Deck.card;
  board : Deck.card option;
  round_history : round_history;
  game_history : game_history;
} [@@deriving show]

val start_game : Deck.card -> Deck.card -> game_state

(* every legal action with the state it leads to; empty once the game is over *)
val legal_steps : game_state -> (action * game_state) list

(* every possible board card, each paired with its probability *)
val deal_board : game_state -> (float * game_state) list

(* None means it's a chop *)
val winning_player : Deck.card -> Deck.card -> Deck.card -> player option

(* None if the game hasn't terminated, otherwise the payoff to P1 *)
val game_payoff : game_state -> int option

(* ---- infosets ------------------------------------------------------- *)

(* what the acting player can see. private: only built by [infoset_from] *)
type infoset = private {
  player : player;
  card_value : Deck.value;
  board : Deck.value option;
  round_history : round_history;
  game_history : game_history;
} [@@deriving show]

val infoset_from : game_state -> infoset
