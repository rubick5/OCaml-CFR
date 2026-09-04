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
  regret : (action * float) list;
  strategy_sum : (action * float) list;
} [@@deriving show]

let fresh_info_data (actions: action list) : infoset_data = 
  let l = List.map (fun a -> (a, 0.0)) actions in
  {
    regret = l;
    strategy_sum = l;
  }

type node =
  | PlayerChoice of history * G.player * infoset * (action * node) list
  | Terminal of history * int
  [@@deriving show]

type deal = { prob : float; cards : G.game_cards; subtree : node } [@@deriving show]
type game_tree = deal list [@@deriving show]

let rec node_for_each (f: node -> unit) (n: node) : unit =
  f n;
  match n with
    | PlayerChoice (_, _, _,  ans) ->
      List.iter (node_for_each f) (List.map snd ans)
    | Terminal _ -> ()


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


let build_regret_table (g: game_tree): (infoset, infoset_data) Hashtbl.t =
  let tbl = Hashtbl.create 16 in
  let use_node = function
    | PlayerChoice (h, _, infoset, _) -> Hashtbl.replace tbl infoset (fresh_info_data (legal_actions h))
    | Terminal _ -> ()
  in
  List.iter (fun { subtree; _ } ->
    node_for_each use_node subtree
  ) g;
  tbl

type table_dump = (infoset * infoset_data) list [@@deriving show]
let dump (tbl: (infoset, infoset_data) Hashtbl.t) : table_dump =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  |> List.sort (fun (a, _) (b, _) -> compare a b)
let print_table tbl = show_table_dump (dump tbl)


let regret_match (regrets : (action * float) list) : (action * float) list =
  let nums = List.map (fun (a, r) -> (a, Float.max 0.0 r)) regrets in
  let sum = List.fold_right (fun (_, r) acc -> acc +. r) nums 0.0 in
  if sum > 0.0 then
    List.map (fun (a, r) -> (a, r /. sum)) nums
  else
    List.map (fun (a, _) -> (a, 1.0 /. (float (List.length regrets)))) nums