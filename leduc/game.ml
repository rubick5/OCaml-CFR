module Deck = struct
  type value = J | Q | K [@@deriving show]
  type suit = H | S [@@deriving show]
  type card = {
    value: value;
    suit : suit;
  } [@@deriving show]
  let size = 6
end

type round = One | Two [@@deriving show]


type full_round_history =
  P1P2Check |

  P1BetP2Call |
  P1BetP2Fold |

  P2BetP1Call |
  P2BetP1Fold |

  P2RaiseP1Call |
  P2RaiseP1Fold |

  P1RaiseP2Call |
  P1RaiseP2Fold [@@deriving show]
  

type round_history =
  Nothing |
  P1Check |
  P1Bet |
  P2Bet |
  P1BetP2Raise |
  P2BetP1Raise [@@deriving show]

type player = P1 | P2
type game_state = {
  p1_card : Deck.card;
  p2_card : Deck.card;
  round_history : round_history;
  game_history : full_round_history option;
} [@@deriving show]

type action = Bet | Call | Fold | Check | Raise

let legal_actions : round_history -> action list = function
  | Nothing -> [Bet ; Check]
  | P1Check -> [Bet ; Check]
  | P1Bet   -> [Call ; Raise ; Fold]
  | P2Bet -> [Call ; Raise ; Fold]
  | P1BetP2Raise -> [Call ; Fold]
  | P2BetP1Raise -> [Call ; Fold]

type either_history = (round_history, full_round_history) Either.t

let apply_action (a : action) (r : round_history) :
  (either_history, string) Result.t =
  match (r, a) with
    | (Nothing, Check) -> Ok (Left (P1Check))
    | (Nothing, Bet) -> Ok (Left (P1Bet))
    | (P1Check, Bet) -> Ok (Left (P2Bet))