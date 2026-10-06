module G = Game
open Result.Syntax

type infoset_data = {
  regret : (G.action * float) list;
  strategy_sum : (G.action * float) list;
} [@@deriving show]

let fresh_info_data (actions: G.action list) : infoset_data =
  let l = List.map (fun a -> (a, 0.0)) actions in
  {
    regret = l;
    strategy_sum = l;
  }

type node =
  | PlayerChoice of G.game_state * (G.action * node) list
  | Chance of (float * node) list
  | Terminal of G.game_state * int [@@deriving show]

let rec node_for_each (f : node -> unit) (n : node) : unit =
  f n;
  match n with
    | PlayerChoice (_, ans) -> List.iter (node_for_each f) (List.map snd ans)
    | Chance (rns) -> List.iter (node_for_each f) (List.map snd rns)
    | Terminal _ -> ()

let rec traverse (f : 'a -> ('b, 'e) result) (xs : 'a list) : ('b list, 'e) result =
  match xs with
    | [] -> Ok []
    | (x :: xs) ->
      let* y = f x in (* let* is like v <- f x in haskell do notation *)
      let* ys = traverse f xs in
      Ok (y :: ys)

let rec build (gs: G.game_state) : (node, string) Result.t =
  match G.game_payoff gs with
    | Some payoff -> Ok (Terminal (gs, payoff))
    | None ->
      match (gs.game_history, gs.round_history) with
        | (OneRound _, Nothing) when gs.board = None -> (* we need to deal the board *)
          let* children = traverse (fun (r, gs) ->
            let* node = build gs in
              Ok (r, node)
          ) (G.deal_board gs) in
          Ok (Chance (children))
        | _ ->
          let* children : (G.action * node) list = traverse (fun (a, gs) ->
            let* node = build gs in
              Ok ((a, node))
          ) (G.legal_steps gs) in
          Ok (PlayerChoice (gs, children))


type deal = {
  p1_card : Deck.card;
  p2_card : Deck.card;
  node : node;
} [@@deriving show]

type game_tree = deal list [@@deriving show]

let full_tree = List.concat_map (fun c1 ->
  List.filter_map (fun c2 ->
      if c1 = c2 then None else
      Some ({
        p1_card = c1;
        p2_card = c2;
        node =
          match build (G.start_game c1 c2) with
            | Ok n -> n
            | Error s -> failwith s
      })
    ) Deck.all_cards
  ) Deck.all_cards

type regret_table = (G.infoset, infoset_data) Hashtbl.t

let build_regret_table (g : game_tree) : regret_table =
  let tbl = Hashtbl.create 16 in
  let use_node = function
    | PlayerChoice (gs, ans) ->
      Hashtbl.replace tbl (G.infoset_from gs) (fresh_info_data (List.map fst ans))
    | Chance _ -> ()
    | Terminal _ -> ()
    in
  List.iter (fun { node } ->
    node_for_each use_node node
  ) g;
  tbl

type table_dump = (G.infoset * infoset_data) list [@@deriving show]
let dump (tbl: (G.infoset, infoset_data) Hashtbl.t) : table_dump =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  |> List.sort (fun (a, _) (b, _) -> compare a b)
let print_table tbl = show_table_dump (dump tbl)


let regret_match (regrets : (G.action * float) list) : (G.action * float) list =
  let nums = List.map (fun (a, r) -> (a, Float.max 0.0 r)) regrets in
  let sum = List.fold_right (fun (_, r) acc -> acc +. r) nums 0.0 in
  if sum > 0.0 then
    List.map (fun (a, r) -> (a, r /. sum)) nums
  else
    List.map (fun (a, _) -> (a, 1.0 /. (float (List.length regrets)))) nums


let rec action_key (a : 'b) : (('b * 'a) list) -> 'a = function
  | ((a2, f) :: rest) -> if a2 = a then f else action_key a rest
  | [] -> failwith "called get_pi with an action that doesn't exist"

let rec combine_regrets (r1 : (G.action * float) list) (r2 : (G.action * float) list) : (G.action * float) list =
  let rec go (a : G.action) (f : float) : (G.action * float) list -> (G.action * float) list = function
    | [] -> []
    | ((a2, f2) :: rest) -> if a = a2 then (a, f +. f2) :: rest else (a2, f2) :: (go a f rest)
  in
  match r1 with
    | [] -> r2
    | ((a, f) :: afs) -> combine_regrets afs (go a f r2)


let rec traverse_game_tree
  (pi1 : float)
  (pi2 : float)
  (pic : float)
  (n : node)
  (tbl : regret_table)
  (write_tbl : regret_table)
  : float =
  match n with
    | PlayerChoice (gs, ans) ->
      let i = G.infoset_from gs in
      let i_data = Hashtbl.find tbl i in
      let s = regret_match (i_data.regret) in
      let p = G.turn gs.round_history in
      let results = List.map (fun (a, n2) ->
        let (new_pi1, new_pi2) = match p with
          | P1 -> ((pi1 *. (action_key a s)), pi2)
          | P2 -> (pi1, (pi2 *. (action_key a s)))
        in
        (a, traverse_game_tree new_pi1 new_pi2 pic n2 tbl write_tbl)
      ) ans in
      let node_val = List.map (fun (a, sa) ->
          sa *. (action_key a results)
        ) s |> List.fold_left (+.) 0.0 in
      let (pii, piii_signed) = match p with
        | P1 -> (pi1, pi2 *. pic)
        | P2 -> (pi2, -.pi1 *. pic)
      in
      let r_cont = List.map (fun (a, v) ->
        (a, piii_signed *. (v -. node_val))
      ) results in
      let strat_cont = List.map (fun (a, sa) ->
        (a, pii *. sa)
      ) s in
      let write_tbl_data = Hashtbl.find write_tbl i in
      let new_entry = {
        regret = combine_regrets write_tbl_data.regret r_cont;
        strategy_sum = combine_regrets write_tbl_data.strategy_sum strat_cont;
      } in
      (* WARNING: side effects here *)
      Hashtbl.replace write_tbl i new_entry;
      (* SIDE EFFECTS END *)
      node_val
    | Chance rns ->
      let results = List.map (fun (r, n2) ->
        traverse_game_tree pi1 pi2 (pic *. r) n2 tbl write_tbl
      ) rns in
      (List.fold_left (+.) 0.0 results) /. (float_of_int (List.length results))
    | Terminal (_, payoff) -> float payoff


let run_iteration (tbl: regret_table) (gt: game_tree) : regret_table =
  let write_tbl = Hashtbl.copy tbl in
  let prob = 1.0 /. (float_of_int (List.length full_tree)) in
  List.iter (fun deal ->
    let _ = traverse_game_tree 1.0 1.0 prob deal.node tbl write_tbl
    in () (*Printf.printf "%f\n" v *)
  ) gt;
  write_tbl


let rec run_iterations (n: int) (tbl: regret_table) (gt: game_tree) : regret_table =
  if n = 0 then tbl else run_iterations (n - 1) (run_iteration tbl gt) gt


type strategy = (G.infoset * ((G.action * float) list)) list [@@deriving show]
let extract_strategy (tbl : regret_table) : strategy =
  Seq.map (fun (k, v) ->
    let strat_sum_sum = List.fold_left (fun acc (a, f) -> f +. acc) 0.0 v.strategy_sum in
    let normalised =
      if strat_sum_sum > 0.0 then
        List.map (fun (a, v) -> (a, v /. strat_sum_sum)) v.strategy_sum
      else
        List.map (fun (a, _) -> (a, 1.0 /. float_of_int (List.length v.strategy_sum))) v.strategy_sum
    in
    (k, normalised)
  ) (Hashtbl.to_seq tbl)
  |> List.of_seq