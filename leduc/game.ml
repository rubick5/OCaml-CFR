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
  [@@deriving show]
type game_history =
  | Nothing
  | OneRound of full_round_history
  | TwoRounds of full_round_history * full_round_history
  [@@deriving show]

let add_full_round (nr : full_round_history) : game_history -> game_history =
  function
    | Nothing -> OneRound nr
    | OneRound r -> TwoRounds (r, nr)  
    | TwoRounds (r1, r2) -> TwoRounds (r1, r2)

type game_state = {
  p1_card : Deck.card;
  p2_card : Deck.card;
  round_history : round_history;
  game_history : game_history;
} [@@deriving show]

type action = Bet | Call | Fold | Check | Raise [@@deriving show]

let legal_actions : round_history -> action list = function
  | Nothing -> [Bet ; Check]
  | P1Check -> [Bet ; Check]
  | P1Bet   -> [Call ; Raise ; Fold]
  | P2Bet -> [Call ; Raise ; Fold]
  | P1BetP2Raise -> [Call ; Fold]
  | P2BetP1Raise -> [Call ; Fold]

let turn : round_history -> player = function
  | Nothing -> P1
  | P1Check -> P2
  | P1Bet -> P2
  | P2Bet -> P1
  | P1BetP2Raise -> P1
  | P2BetP1Raise -> P2

type either_history = (round_history, full_round_history) Either.t

let apply_action (a : action) (r : round_history) :
  (either_history, string) Result.t =
  match (r, a) with
    | (Nothing, Check) -> Ok (Left (P1Check))
    | (Nothing, Bet)   -> Ok (Left (P1Bet))
    | (P1Check, Bet)   -> Ok (Left (P2Bet)) 
    | (P1Check, Check) -> Ok(Right (P1P2Check))
    | (P1Bet, Call)    -> Ok(Right (P1BetP2Call))
    | (P1Bet, Raise)   -> Ok(Left (P1BetP2Raise))
    | (P1Bet, Fold)   -> Ok(Right (P1BetP2Fold))
    | (P2Bet, Call)   -> Ok(Right (P2BetP1Call))
    | (P2Bet, Raise)   -> Ok(Left (P2BetP1Raise))
    | (P2Bet, Fold)   -> Ok(Right (P2BetP1Fold))
    | (P1BetP2Raise, Call) -> Ok(Right (P2RaiseP1Call))
    | (P1BetP2Raise, Fold) -> Ok(Right (P2RaiseP1Fold))
    | (P2BetP1Raise, Call) -> Ok(Right (P1RaiseP2Call))
    | (P2BetP1Raise, Fold) -> Ok(Right (P1RaiseP2Fold))
    | _ -> Error "invalid action cannot be applied"


let legal_steps (gs: game_state) : (action * game_state) list =
  match gs.game_history with
    | TwoRounds _ -> []
    | _ ->
      List.map ( fun a ->
        let (new_rh, new_gh) =
          match apply_action a gs.round_history with
            | Ok (Left rh) -> (rh, gs.game_history)
            | Ok (Right frh) -> (Nothing, add_full_round frh gs.game_history)
            | Error e -> failwith "should never happen"
        in
        (a, { gs with round_history = new_rh; game_history = new_gh })
      ) (legal_actions gs.round_history)

type infoset = {
  player : player;
  card_value : Deck.value; (* only card value because suit doesn't change the infoset *)
  round_history : round_history;
  game_history : game_history;
} [@@deriving show]

let infoset_from (gs: game_state) : infoset =
  let card_value = match turn gs.round_history with
    | P1 -> gs.p1_card.value
    | P2 -> gs.p2_card.value
  in
  {
    player = turn gs.round_history;
    card_value;
    round_history = gs.round_history;
    game_history = gs.game_history;
  }