open Strategy
module G = Game

type info_action_table = ((G.infoset * G.action), float) Hashtbl.t

let rec eval_traversal
  (player: G.player)
  (s : strategy)
  (p_opp : float)
  (node: node)
  (tbl : info_action_table)
  : float =
  match node with
    | PlayerChoice (gs, ans) when G.turn gs.round_history = player ->
      let infoset = G.infoset_from gs in
      List.map (fun (a, n) ->
        let after_val = eval_traversal player s p_opp n tbl in
        let cur = Hashtbl.find tbl (infoset, a) in
        Hashtbl.replace tbl (infoset, a) (cur +. after_val *. p_opp);
        after_val
      ) ans |> List.fold_left max Float.neg_infinity
    | PlayerChoice (gs, ans) ->
      let infoset = G.infoset_from gs in
      List.map (fun (a, n) ->
        let p = action_key a (action_key infoset s) in
        p *. eval_traversal player s (p_opp *. p) n tbl
      ) ans |> List.fold_left (+.) 0.0
    | Chance pns ->
      List.map (fun (p, n) ->
        p *. (eval_traversal player s (p_opp *. p) n tbl)
      ) pns |> List.fold_left (+.) 0.0
    | Terminal (_, payoff) -> 
      let p = float_of_int payoff in
      match player with
        | P1 -> p
        | P2 -> -. p
