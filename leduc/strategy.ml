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
  | Terminal of G.game_state * int

let rec traverse (f : 'a -> ('b, 'e) result) (xs : 'a list) : ('b list, 'e) result =
  match xs with
    | [] -> Ok []
    | (x :: xs) ->
      let* y = f x in (* let* is like v <- f x in haskell do notation *)
      let* ys = traverse f xs in
      Ok (y :: ys)

let build (gs: G.game_state) : (node, string) Result.t =
  failwith "TODO"