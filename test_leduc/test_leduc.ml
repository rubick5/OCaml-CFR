(* test_leduc.ml *)
open Leduc.Game
module D = Leduc.Deck

(* ---- tiny harness ---------------------------------------------------- *)

let failures = ref 0

let check name cond =
  if not cond then (incr failures; Printf.printf "FAIL: %s\n" name)

(* Stdlib.compare so this still works if you shadow polymorphic (=) later *)
let check_eq name pp expected actual =
  if Stdlib.compare expected actual <> 0 then begin
    incr failures;
    Printf.printf "FAIL: %s\n  expected %s\n  got      %s\n"
      name (pp expected) (pp actual)
  end

let pp_int_opt = function None -> "None" | Some n -> "Some " ^ string_of_int n
let pp_player_opt = function None -> "None" | Some p -> "Some " ^ show_player p

(* ---- fixtures -------------------------------------------------------- *)

let c v s : D.card = D.{ value = v; suit = s }
let jh = c D.J D.H  and js = c D.J D.S
let qh = c D.Q D.H  and qs = c D.Q D.S
let kh = c D.K D.H  and ks = c D.K D.S

(* annotations needed because [Nothing] belongs to two types *)
let all_rh : round_history list =
  [Nothing; P1Check; P1Bet; P2Bet; P1BetP2Raise; P2BetP1Raise]

let all_frh : full_round_history list =
  [P1P2Check; P1BetP2Call; P1BetP2Fold; P2BetP1Call; P2BetP1Fold;
   P2RaiseP1Call; P2RaiseP1Fold; P1RaiseP2Call; P1RaiseP2Fold]

let all_actions = [Bet; Call; Fold; Check; Raise]

let state ~p1 ~p2 ~board ~gh : game_state =
  { p1_card = p1; p2_card = p2; board;
    round_history = (Nothing : round_history); game_history = gh }

(* ---- showdown -------------------------------------------------------- *)

let () =
  (* no pairs: high card wins *)
  check_eq "showdown K vs Q on J -> P1" pp_player_opt
    (Some P1) (winning_player kh qh js);
  check_eq "showdown Q vs K on J -> P2" pp_player_opt
    (Some P2) (winning_player qh kh js);

  (* pairing the board beats any high card *)
  check_eq "showdown J vs K on J -> P1 (pair)" pp_player_opt
    (Some P1) (winning_player jh ks js);
  check_eq "showdown K vs J on J -> P2 (pair)" pp_player_opt
    (Some P2) (winning_player ks jh js);
  check_eq "showdown Q vs K on Q -> P1 (pair)" pp_player_opt
    (Some P1) (winning_player qh ks qs);

  (* same value, no pair: chop *)
  check_eq "showdown Q vs Q on J -> chop" pp_player_opt
    None (winning_player qh qs js);
  check_eq "showdown K vs K on J -> chop" pp_player_opt
    None (winning_player kh ks js);

  (* suit must be irrelevant *)
  check "showdown ignores suit"
    (Stdlib.compare (winning_player kh qh js) (winning_player ks qs jh) = 0)

(* ---- legal_actions vs apply_action ----------------------------------- *)

let () =
  List.iter (fun r ->
    check ("legal_actions non-empty for " ^ show_round_history r)
      (match legal_actions r with [] -> false | _ -> true);
    List.iter (fun a ->
      let legal = List.mem a (legal_actions r) in
      let accepted = match apply_action a r with Ok _ -> true | Error _ -> false in
      let name = Printf.sprintf "apply_action/legal_actions agree: %s + %s"
                   (show_round_history r) (show_action a) in
      if legal then check name accepted else check name (not accepted)
    ) all_actions
  ) all_rh

(* every round terminates, and all 9 endings are reachable *)
let () =
  let seen = ref [] in
  let rec walk (r : round_history) depth =
    if depth > 4 then (incr failures; print_endline "FAIL: round did not terminate")
    else
      List.iter (fun a ->
        match apply_action a r with
        | Ok (Either.Left r') -> walk r' (depth + 1)
        | Ok (Either.Right frh) ->
          if not (List.mem frh !seen) then seen := frh :: !seen
        | Error _ -> incr failures
      ) (legal_actions r)
  in
  walk Nothing 0;
  check_eq "all 9 round endings reachable" string_of_int 9 (List.length !seen);
  List.iter (fun frh ->
    check ("reachable: " ^ show_full_round_history frh) (List.mem frh !seen)
  ) all_frh

(* ---- payoffs: round-1 folds ------------------------------------------ *)
(* payoff to P1 = net chips. On a fold the winner gains exactly what the
   folder committed (ante + wagers); the uncalled excess comes back. *)

let () =
  let cases = [
    ("P2 folds to P1's bet",   P1BetP2Fold,   1);   (* P2 in for ante only  *)
    ("P1 folds to P2's bet",   P2BetP1Fold,  -1);
    ("P1 folds to P2's raise", P2RaiseP1Fold, -3);  (* ante 1 + bet 2       *)
    ("P2 folds to P1's raise", P1RaiseP2Fold, 3);
  ] in
  List.iter (fun (name, h, expect) ->
    let gs = state ~p1:kh ~p2:qh ~board:None ~gh:(OneRound h) in
    check_eq ("round-1 fold: " ^ name) pp_int_opt (Some expect) (game_payoff gs)
  ) cases

(* a matched round 1 is NOT terminal *)
let () =
  List.iter (fun h ->
    let gs = state ~p1:kh ~p2:qh ~board:None ~gh:(OneRound h) in
    match has_fold h with
    | None ->
      check ("not terminal after matched round 1: " ^ show_full_round_history h)
        (match game_payoff gs with None -> true | Some _ -> false)
    | Some _ ->
      check ("no legal steps after round-1 fold: " ^ show_full_round_history h)
        (match legal_steps gs with [] -> true | _ -> false)
  ) all_frh

(* ---- payoffs: showdowns ---------------------------------------------- *)
(* K vs Q on J: P1 wins on high card, no pairs involved.
   Each player's total = ante 1 + round-1 wagers + round-2 wagers. *)

let () =
  let cases = [
    ("check/check, check/check", P1P2Check,     P1P2Check,      1);
    ("check/check, bet/call",    P1P2Check,     P1BetP2Call,    5);
    ("bet/call, bet/call",       P1BetP2Call,   P1BetP2Call,    7);
    ("raise/call, raise/call",   P2RaiseP1Call, P1RaiseP2Call, 13);
  ] in
  List.iter (fun (name, h1, h2, expect) ->
    let win = state ~p1:kh ~p2:qh ~board:(Some js) ~gh:(TwoRounds (h1, h2)) in
    let lose = state ~p1:qh ~p2:kh ~board:(Some js) ~gh:(TwoRounds (h1, h2)) in
    check_eq ("showdown P1 wins: " ^ name) pp_int_opt (Some expect) (game_payoff win);
    check_eq ("showdown P1 loses: " ^ name) pp_int_opt (Some (-expect)) (game_payoff lose)
  ) cases;

  (* chop pays nothing whatever the betting was *)
  let chop = state ~p1:qh ~p2:qs ~board:(Some js)
               ~gh:(TwoRounds (P1BetP2Call, P1BetP2Call)) in
  check_eq "chop pays 0" pp_int_opt (Some 0) (game_payoff chop)

(* ---- payoffs: round-2 folds ------------------------------------------ *)

let () =
  let cases = [
    ("checked round 1, P2 folds round 2", P1P2Check,   P1BetP2Fold,    1);
    ("bet/call round 1, P1 folds to raise", P1BetP2Call, P2RaiseP1Fold, -7);
  ] in
  List.iter (fun (name, h1, h2, expect) ->
    let gs = state ~p1:kh ~p2:qh ~board:(Some js) ~gh:(TwoRounds (h1, h2)) in
    check_eq ("round-2 fold: " ^ name) pp_int_opt (Some expect) (game_payoff gs)
  ) cases

(* ---- aggregate bound ------------------------------------------------- *)
(* max commitment is 1 + 4 + 8 = 13, so no payoff may exceed that, and 13
   must actually be attainable. *)

let () =
  let matched = List.filter (fun h -> has_fold h = None) all_frh in
  let worst = ref 0 in
  List.iter (fun h1 ->
    List.iter (fun h2 ->
      let gs = state ~p1:kh ~p2:qh ~board:(Some js) ~gh:(TwoRounds (h1, h2)) in
      match game_payoff gs with
      | Some p ->
        check (Printf.sprintf "|payoff| <= 13 for %s / %s"
                 (show_full_round_history h1) (show_full_round_history h2))
          (abs p <= 13);
        if abs p > !worst then worst := abs p
      | None ->
        incr failures;
        Printf.printf "FAIL: TwoRounds %s / %s should be terminal\n"
          (show_full_round_history h1) (show_full_round_history h2)
    ) all_frh
  ) matched;
  check_eq "max payoff is exactly 13" string_of_int 13 !worst

(* ---- infosets -------------------------------------------------------- *)

let () =
  (* suits are strategically irrelevant: same infoset *)
  let a = state ~p1:kh ~p2:qh ~board:None ~gh:Nothing in
  let b = state ~p1:ks ~p2:qs ~board:None ~gh:Nothing in
  check "infoset ignores suit"
    (Stdlib.compare (infoset_from a) (infoset_from b) = 0);

  (* different hole card: different infoset *)
  let c1 = state ~p1:kh ~p2:qh ~board:None ~gh:Nothing in
  let c2 = state ~p1:jh ~p2:qh ~board:None ~gh:Nothing in
  check "infoset distinguishes hole card"
    (Stdlib.compare (infoset_from c1) (infoset_from c2) <> 0);

  (* the board is public in round 2: must change the infoset *)
  let r2 board = state ~p1:kh ~p2:qh ~board:(Some board)
                   ~gh:(OneRound P1P2Check) in
  check "infoset distinguishes board card"
    (Stdlib.compare (infoset_from (r2 js)) (infoset_from (r2 qs)) <> 0);

  (* infoset player agrees with turn *)
  List.iter (fun r ->
    let gs = { (state ~p1:kh ~p2:qh ~board:None ~gh:Nothing) with round_history = r } in
    check ("infoset player matches turn for " ^ show_round_history r)
      (Stdlib.compare (infoset_from gs).player (turn r) = 0)
  ) all_rh

(* ---------------------------------------------------------------------- *)

let () =
  if !failures = 0 then print_endline "all tests passed"
  else (Printf.printf "\n%d failure(s)\n" !failures; exit 1)