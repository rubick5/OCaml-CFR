# cfr

Counterfactual Regret Minimization (CFR) written from scratch in OCaml. It solves
two small poker games:

- **Kuhn poker**: 3 cards, one betting round, 12 information sets.
- **Leduc hold'em**: 6 cards (J, Q, K in two suits), two betting rounds with a
  public board card dealt between them, 288 information sets.

For each game the program builds the full game tree and runs vanilla CFR on it.
It then prints the average strategy, which converges towards a Nash equilibrium.

## Layout

```
bin/            Kuhn poker: game rules, tree, CFR, entry point (executable `cfr`)
leduc/          Leduc hold'em library
  deck.ml         cards, suits, card comparison
  game.ml         betting rounds, legal actions, board deal, payoffs, infosets
  game.mli        public interface of game.ml (hides pot/bet-size helpers)
  strategy.ml     game tree, regret table, CFR traversal, average strategy
  evaluation.ml   best-response evaluation (work in progress)
bin_leduc/      Leduc entry point (executable `leduc`)
test_leduc/     tests for the Leduc rules and payoffs
```

## Running

You need OCaml, dune and `ppx_deriving`.

```bash
dune build
```

```bash
dune exec leduc
```

```bash
dune exec cfr
```

```bash
dune test
```

`leduc` runs 10,000 CFR iterations and prints the average strategy for every
infoset. `cfr` runs 100,000 iterations of Kuhn poker. It also prints the value of
every deal on every iteration, so redirect its output to a file. The `*.log`
files in the repo root (gitignored) are saved runs: `full_tree.log` is the
printed Leduc tree, `regret_table.log` is the empty regret table, and
`full_run*.log` are solved strategies.

## How it works

The same design is used for both games:

1. **Model the rules as types.** Betting histories are variants
   (`P1BetP2Raise`, `P2RaiseP1Fold`, …), so an illegal sequence can't be
   represented. `legal_steps` maps a state to its `(action, next state)` pairs,
   and `game_payoff` returns `Some payoff` only when the state is terminal.
2. **Build the whole tree up front.** The `build` function recurses over
   `legal_steps` and returns a tree of `PlayerChoice`, `Chance` (Leduc board
   deal) and `Terminal` nodes. The tree is built once for each private deal.
   Errors are passed through `Result` with a `traverse` helper (like Haskell's
   `traverse`/`mapM`).
3. **Key regrets by infoset.** An infoset contains only what the acting player
   can see: their own card value, the public board card and the betting so far.
   It never contains the opponent's card or any suit. A `Hashtbl` maps each
   infoset to its cumulative regrets and strategy sums.
4. **Run CFR.** Each iteration walks every deal's subtree. It carries the reach
   probabilities for P1, P2 and chance, takes the current strategy from regret
   matching, and adds counterfactual regrets and reach-weighted strategy sums to
   the table. Reads come from the table as it stood at the start of the
   iteration and writes go to a copy, so both players update at the same time
   (vanilla CFR, not CFR+ and not alternating updates).
5. **Take the average strategy.** The final strategy is the normalized strategy
   sum, not the last iterate. The average is what converges to equilibrium.

Payoffs are zero-sum and given from P1's side; P2's regrets are negated. In
Leduc both players ante 1, the bet is 2 in round one and 4 in round two, and
each round allows at most one raise. A board pair wins; otherwise the higher
card wins; equal cards split the pot.

## Development history

### Kuhn poker (2–7 September 2026)

- **First version**: Kuhn poker as a direct simulation. Each player is a record
  of functions (`start`, `face_bet`, …) and the game is played as explicit
  turns (`turn_one` → `turn_two` → `turn_three`). The weighted strategies in
  `Strategy` (`p1_strategy`, `p2_strategy`) come from this stage. The code was
  then split into several files.
- **Moving to CFR**: game histories became action lists (most recent first), with
  `legal_actions` and `is_terminal` rules and a way to map a history back to a
  `game_sequence` for scoring. The full tree is built from these.
  Writing `traverse` made the `Result`-based tree construction simple.
- **Regret table and regret matching**: a hashtable from infoset to data. The
  data was changed from arrays to association lists, keyed by action. One bug
  fixed here: the uniform fallback in regret matching divided by the sum of
  regrets instead of the number of actions.
- **"All done"**: full CFR traversal, 100,000 iterations, and average strategy
  extraction.

### Leduc hold'em (2–6 October 2026)

Leduc is a separate library rather than a modified Kuhn. It needs a second
round, raises, a chance node for the board and real pot accounting.

- **Rule types first**: `round_history` (a round in progress),
  `full_round_history` (a finished round, of which there are 9 kinds) and
  `game_history` (zero, one or two finished rounds). `apply_action` returns
  either a new round in progress or a finished round. `legal_steps` uses it to
  produce the next states.
- **Encapsulation, undone, then restored**: a `game.mli` was added to make the
  history types abstract and hide helper functions. It was moved to `temp/` so
  the tests could reach the internals. It was later brought back into `leduc/`.
  The history and state types are concrete there, because the tests and
  `strategy.ml` match on them. Pot and bet-size helpers stay hidden, and
  `infoset` is a `private` record that only `infoset_from` can build.
  The test suite in `test_leduc/` covers
  showdown ranking, agreement between `legal_actions` and `apply_action`,
  reachability of all 9 round endings, payoffs for folds and showdowns in both
  rounds, the 13-chip maximum, and infoset correctness (suit ignored, board
  included). It found several bugs. One fix: a fold in round one must end the
  game, so those states have no legal steps.
- **Tree building**: `PlayerChoice` and `Terminal` nodes first, then a `Chance`
  node for dealing the board once round one finishes without a fold.
- **CFR on Leduc**: the Kuhn traversal was reused with a `Chance` case added,
  where the chance reach is multiplied by the deal probability. Infosets are
  now taken from the game state with `infoset_from`. A run of 10,000 iterations
  produces a strategy over all 288 infosets.
- **Evaluation (in progress)**: `evaluation.ml` started a best-response
  calculation to measure how exploitable the strategy is. Right now it takes the
  max at each tree node, so the best responder can effectively see the
  opponent's card. A correct best response has to choose one action per
  *infoset*, by summing counterfactual values over all nodes in the infoset
  first. This is the next thing to fix.

## Known gaps / next steps

- Fix `eval_traversal` to compute a per-infoset best response. Then report
  exploitability, and plot it against the iteration count.
- `dune-project` and `cfr.opam` still contain the dune template placeholders.
- `lib/` and `test/test_cfr.ml` are empty. The Kuhn code is all in `bin/`, and
  its `Strategy` module still holds the unused simulation-era types.
- The file named `'` in the repo root is a stray partial copy of
  `leduc/strategy.ml` and can be deleted.
- Possible speedups: CFR+, alternating updates, or array-based infoset storage
  in place of association lists.
