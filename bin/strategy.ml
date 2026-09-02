open Game

type p1_strategy = {
  (* Each int represents the chance to take the 'aggressive' action 
  (i.e. bet or call) for J, Q, K in order *)
  start_weights : float * float * float;
  facing_weights : float * float * float;
}

let first_from_weights (ws: float * float * float) (c : card) : first_decision =
  let (jprob, qprob, kprob) = ws in
  let prob = (
    match c with
      | J -> jprob
      | Q -> qprob
      | K -> kprob
  ) in
    if (Random.float 1.0 < prob) then Bet else Check

let response_from_weights (ws: float * float * float) (c : card) : response =
  let (jprob, qprob, kprob) = ws in
  let prob = (
    match c with
      | J -> jprob
      | Q -> qprob
      | K -> kprob
  ) in
    if (Random.float 1.0 < prob) then Call else Fold


let strategy_to_p1 (p: p1_strategy) : p1 = {
  start = first_from_weights p.start_weights ;
  face_bet = response_from_weights p.facing_weights
}

type p2_strategy = {
  facing_check_weights : float * float * float;
  facing_bet_weights : float * float * float;
}

let strategy_to_p2 (p : p2_strategy) : p2 = {
  face_check = first_from_weights p.facing_check_weights ;
  face_bet = response_from_weights p.facing_bet_weights
}

type infoset_id = {
  player : player;
  player_card : card;
  history: action list;
}

type node = {
  (* The float here gives the chance of going from our node to theirs?? *)
  (* action is the action that takes us from our current state to them *)
  (* storing action here allows us to traverse the tree using action history *)
  children : (action * node * float) list;
  history : action list;
 infoset_id: infoset_id;
}

(* we will separately store a map from infoset id to strategy sum and regret sum *)
type infoset_data = {
  regret : float;
  strategy_sum : float;
}

let h : (infoset_id, infoset_data) Hashtbl.t = Hashtbl.create 16