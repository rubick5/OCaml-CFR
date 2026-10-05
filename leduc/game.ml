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


type player = P1 | P2
  [@@deriving show]

let has_fold : full_round_history -> player option = function
  | P1P2Check -> None
  | P1BetP2Call -> None
  | P2BetP1Call -> None
  | P2RaiseP1Call -> None
  | P1RaiseP2Call -> None

  | P1BetP2Fold -> Some P2
  | P2BetP1Fold -> Some P1
  | P2RaiseP1Fold -> Some P1
  | P1RaiseP2Fold -> Some P2

type round_history =
  Nothing |
  P1Check |
  P1Bet |
  P2Bet |
  P1BetP2Raise |
  P2BetP1Raise [@@deriving show]


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
  board : Deck.card option;
  round_history : round_history;
  game_history : game_history;
} [@@deriving show]

let start_game (c1 : Deck.card) (c2 : Deck.card) = {
  p1_card = c1;
  p2_card = c2;
  board = None;
  round_history = Nothing;
  game_history = Nothing;
}

let own_card (p : player) (c1 : Deck.card) (c2 : Deck.card) = match p with
  | P1 -> c1
  | P2 -> c2

type action = Bet | Call | Fold | Check | Raise [@@deriving show]

let legal_actions : round_history -> action list = function
  | Nothing       -> [Bet ; Check]
  | P1Check       -> [Bet ; Check]
  | P1Bet         -> [Call ; Raise ; Fold]
  | P2Bet         -> [Call ; Raise ; Fold]
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
    | OneRound h when has_fold h != None -> []
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
  board : Deck.value option;
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
    board = Option.map (fun c -> Deck.(c.value)) gs.board;
    round_history = gs.round_history;
    game_history = gs.game_history;
  }

let deal_board (gs : game_state) : (float * game_state) list =
  let all_deals = List.filter_map (fun c ->
      if c = gs.p1_card || c = gs.p2_card then
        None
      else
        Some { gs with board = Some c }
    ) Deck.all_cards
  in
  let p = 1.0 /. (float_of_int (List.length all_deals)) in
  List.map (fun gs -> (p, gs)) all_deals

let pot_increase (bet_size : int) : full_round_history -> int = function
  | P1P2Check -> 0
  | P1BetP2Call -> bet_size * 2
  | P1BetP2Fold -> 0
  | P2BetP1Call -> bet_size * 2
  | P2BetP1Fold -> 0
  | P2RaiseP1Call -> bet_size * 4
  | P2RaiseP1Fold -> bet_size * 2
  | P1RaiseP2Call ->  bet_size * 4
  | P1RaiseP2Fold -> bet_size * 2

let blind_size = 1;;
let round1_bet_size = 2;;
let round2_bet_size = 4;;

(* None means it's a chop *)
let winning_player (p1c : Deck.card) (p2c : Deck.card) (b: Deck.card) : player option =
  if p1c.value = p2c.value then None
  else if p1c.value = b.value || (Deck.(p1c >: p2c) && p2c.value != b.value) then Some P1
  else Some P2

(*
If the game hasn't terminated, gives None
Otherwise, gives the positive payoff for P1
*)
let game_payoff (gs : game_state) : int option =
  Option.map (fun x -> x / 2) (
  match gs.game_history with
    | Nothing -> None
    | OneRound h -> 
      (match has_fold h with
        | Some P2 -> Some (blind_size * 2 + pot_increase round1_bet_size h)
        | Some P1 -> Some (-(blind_size * 2 + pot_increase round1_bet_size h))
        | None -> None)
    | TwoRounds (h1, h2) ->
      let sign =
      match has_fold h2 with
        | Some P2 -> 1
        | Some P1 -> -1
        | None -> (* we go to showdown *)
          match gs.board with
            | Some b ->
              (match winning_player gs.p1_card gs.p2_card b with
                | Some P1 -> 1
                | Some P2 -> -1
                | None -> 0)
            | None -> failwith "invalid state... no board but second round"
      in
      Some( sign * (
      2 * blind_size + pot_increase round1_bet_size h1 +
      pot_increase round2_bet_size h2)
      )
  )