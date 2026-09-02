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