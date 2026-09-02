type card = J | Q | K [@@deriving show]
type player = P1 | P2 [@@deriving show]

type first_decision = Bet | Check [@@deriving show]
type response = Fold | Call [@@deriving show]



type game_cards = {
  p1_card : card;
  p2_card : card;
} [@@deriving show]

type action = BetOrCheck of first_decision
  | FoldOrCall of response
  | CardsDealt of game_cards

type game_sequence =
  | P1CheckP2Check
  | P1BetP2Fold
  | P1BetP2Call
  | P2BetP1Call
  | P2BetP1Fold [@@deriving show]

type game_end_state = {
  game_cards : game_cards;
  game_sequence : game_sequence;
} [@@deriving show]

let game_end_results (g: game_end_state) : int * int =
  let p1_higher_card = g.game_cards.p1_card > g.game_cards.p2_card in
  match (g.game_sequence) with
    | P2BetP1Fold -> (-1, +1)
    | P1BetP2Fold -> (+1, -1)
    | P1BetP2Call | P2BetP1Call -> 
      if p1_higher_card then
        (+2, -2)
      else
        (-2, +2)
    | P1CheckP2Check ->
      if p1_higher_card then
        (+1, -1)
      else
        (-1, +1)

type turn2_game_state = P1NoBet of game_cards
  | P1Bet of game_cards [@@deriving show]

type turn3_game_state = GameEnd of game_end_state
  | P2Bet of game_cards [@@deriving show]

type p1 = {
  start : card -> first_decision;
  face_bet : card -> response;
}

type p2 = {
  face_check : card -> first_decision;
  face_bet : card -> response;
}


let turn_one (g : game_cards) (p1 : p1) : turn2_game_state =
  match p1.start (g.p1_card) with
    | Bet -> P1Bet g
    | Check -> P1NoBet g

let turn_two (g: turn2_game_state) (p2: p2) : turn3_game_state =
  match g with
    | P1Bet game_cards -> (
      match p2.face_bet game_cards.p2_card with
        | Fold -> GameEnd { game_cards = game_cards; game_sequence = P1BetP2Fold }
        | Call -> GameEnd { game_cards = game_cards; game_sequence = P1BetP2Call  }
      )

    | P1NoBet game_cards -> (
      match p2.face_check game_cards.p2_card with
        | Bet -> P2Bet game_cards
        | Check -> GameEnd { game_cards = game_cards; game_sequence = P1CheckP2Check }
    )

let turn_three (g: turn3_game_state) (p1: p1): game_end_state =
  match g with
    | GameEnd g -> g
    | P2Bet game_cards -> (
      { game_cards = game_cards;
        game_sequence = (
          match p1.face_bet game_cards.p1_card with
            | Fold -> P2BetP1Fold
            | Call -> P2BetP1Call
        );
      }
    )

let run_game (g : game_cards) (p1 : p1) (p2 : p2): game_end_state =
  let turn2 = turn_one g p1 in
  let turn3 = turn_two turn2 p2 in
  let finished = turn_three turn3 p1 in
    finished