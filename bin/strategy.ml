module G = Game
open Result.Syntax

type p1_strategy = {
  (* Each int represents the chance to take the 'aggressive' action 
  (i.e. bet or call) for J, Q, K in order *)
  start_weights : float * float * float;
  facing_weights : float * float * float;
}

let first_from_weights (ws: float * float * float) (c : G.card) : G.first_decision =
  let (jprob, qprob, kprob) = ws in
  let prob = (
    match c with
      | G.J -> jprob
      | G.Q -> qprob
      | G.K -> kprob
  ) in
    if (Random.float 1.0 < prob) then G.Bet else G.Check

let response_from_weights (ws: float * float * float) (c : G.card) : G.response =
  let (jprob, qprob, kprob) = ws in
  let prob = (
    match c with
      | G.J -> jprob
      | G.Q -> qprob
      | G.K -> kprob
  ) in
    if (Random.float 1.0 < prob) then G.Call else G.Fold


let strategy_to_p1 (p: p1_strategy) : G.p1 = {
  start = first_from_weights p.start_weights ;
  face_bet = response_from_weights p.facing_weights
}

type p2_strategy = {
  facing_check_weights : float * float * float;
  facing_bet_weights : float * float * float;
}

let strategy_to_p2 (p : p2_strategy) : G.p2 = {
  face_check = first_from_weights p.facing_check_weights ;
  face_bet = response_from_weights p.facing_bet_weights
}

type action = Bet | Check | Call | Fold

(* Note that histories list their actions most-recent-first... *)
type history = action list

type infoset = {
  player : G.player;
  player_card : G.card;
  history : history;
}

(* we will separately store a map from infoset id to strategy sum and regret sum *)
type infoset_data = {
  regret : float;
  strategy_sum : float;
}

type node =
  | PlayerChoice of history * G.player * infoset * (action * node) list
  | Terminal of history * int

type deal = { prob : float; cards : G.game_cards; subtree : node }
type game_tree = deal list


(* assumes the history has been legal so far *)
let legal_actions (h: history) : action list =
  match h with
    | [] -> [Bet; Check]
    | (Bet :: _) -> [Call; Fold]
    | (Check :: []) -> [Bet; Check]
    | _ -> []

let is_terminal : history -> bool = function
  | (Call :: _) -> true
  | (Fold :: _) -> true
  | (Check :: Check :: []) -> true
  | _ -> false

let game_seq_from_history : history -> (G.game_sequence, string) Result.t =
  function
    | (Check :: Check :: []) -> Ok G.P1CheckP2Check
    | (Fold :: Bet :: []) -> Ok G.P1BetP2Fold
    | (Call :: Bet :: []) -> Ok G.P1BetP2Call
    | (Call :: Bet :: Check :: []) -> Ok G.P2BetP1Call
    | (Fold :: Bet :: Check :: []) -> Ok G.P2BetP1Fold
    | _ -> Error "could not convert history to game sequence"

let gen_subtree (cs: G.game_cards) : node -> (node, string) Result.t = function
  | Terminal (h, p) -> Ok (Terminal (h, p))
  | PlayerChoice (h, player, infoset, _) ->
    let actions = legal_actions h in
    let children = List.concat_map (fun a ->
      if not (is_terminal (a :: h)) then
        let op = G.other_player player in
        let new_infoset = { 
          player = op;
          player_card = G.own_card cs op;
          history = a :: h;
        } in
        Ok (PlayerChoice (a :: h, op, new_infoset, []))
      else (*it is terminal...*)
        let* game_seq = game_seq_from_history (a :: h) in
        Ok (Terminal (a :: h, fst (G.game_end_results { game_cards = cs;
        game_sequence = game_seq } ) ))
    ) actions
    in Ok (PlayerChoice (h, player, infoset, children))



(*
(* pre: this is called on leaf nodes once the card deal has already been enumerated *)
let gen_children : node -> node = function
    | CardDeal (h, _) ->
      let children =
        List.map (fun a -> PlayerChoice (h, G.P1, [])) (legal_actions h)
      in CardDeal (h, children)

    | PlayerChoice (h, p, _) -> (
      match legal_actions h with
      (* TODO : CALCULATE PAYOFF *)
        | [] -> PlayerChoice (h, p, [Terminal (h, 6.9)])
        | pub_actions -> (
          let children =
            List.map (fun a -> PlayerChoice (h, G.other_player p, [])) pub_actions
          in PlayerChoice ((h), p, children)
        )
    )
    | Terminal (h, payoff) -> Terminal (h, payoff)
    *)
let h : (infoset, infoset_data) Hashtbl.t = Hashtbl.create 16
