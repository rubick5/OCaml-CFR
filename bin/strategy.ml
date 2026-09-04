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

type action = Bet | Check | Call | Fold [@@deriving show]

(* Note that histories list their actions most-recent-first... *)
type history = action list [@@deriving show]

type infoset = {
  player : G.player;
  player_card : G.card;
  history : history;
} [@@deriving show]

(* we will separately store a map from infoset id to strategy sum and regret sum *)
type infoset_data = {
  regret : float;
  strategy_sum : float;
} [@@deriving show]

type node =
  | PlayerChoice of history * G.player * infoset * (action * node) list
  | Terminal of history * int
  [@@deriving show]

type deal = { prob : float; cards : G.game_cards; subtree : node } [@@deriving show]
type game_tree = deal list [@@deriving show]


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

let rec traverse (f: 'a -> ('b, 'e) result) (xs: 'a list) : ('b list, 'e) result =
  match xs with
    | [] -> Ok []
    | (x :: xs) ->
      let* y = f x in
      let* ys = traverse f xs in
      Ok (y :: ys)

let rec build (p: G.player) (h: history) (cs: G.game_cards) : (node, string) Result.t =
  if is_terminal h then
    let* g_seq = game_seq_from_history h in
    let (payoff, _) = G.game_end_results { game_cards = cs; game_sequence = g_seq } in
    Ok (Terminal (h, payoff))
  else
    let infoset = {
      player = p;
      player_card = G.own_card cs p;
      history = h;
    } in
    let* children = traverse (fun a ->
      let* n = build (G.other_player p) (a :: h) cs in
      Ok ((a, n))
    ) (legal_actions h) in
    Ok (PlayerChoice (h, p, infoset, children))

let full_tree : (game_tree, string) Result.t = traverse (fun cs ->
  let* sub = build G.P1 [] cs in
    Ok {
      prob = 1.0 /. 6.0;
      cards = cs;
      subtree = sub;
    }
  )
  G.all_game_cards


let h : (infoset, infoset_data) Hashtbl.t = Hashtbl.create 16
