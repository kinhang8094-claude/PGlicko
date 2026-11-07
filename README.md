# PGlicko

Glicko and **Pairwise Glicko (P-Glicko)** rating systems for head-to-head competitions.

P-Glicko gives every pair of players a pairwise strength β<sub>ij</sub> and a pairwise
deviation b<sub>ij</sub>, alongside the usual Glicko rating μ<sub>i</sub> and rating deviation
σ<sub>i</sub>. Win probabilities can therefore be **intransitive**: A can be favoured over
B, B over C and C over A, as in rock–paper–scissors, style match-ups in sports, or
strategy match-ups in games.

## Installation

```r
# install.packages("remotes")
remotes::install_github("kinhang8094-claude/PGlicko", ref = "r-package",
                        build_vignettes = TRUE)
```

After the branch is merged into `kh-w/PGlicko`, use
`remotes::install_github("kh-w/PGlicko", build_vignettes = TRUE)`.

## Quick start

```r
library(PGlicko)
data(rps_league)          # simulated league: a rock-paper-scissors cycle plus a ladder
head(rps_league)
#>   Season Player1  Player2 Game_Result
#> 1      1    Rock    Paper           0
#> ...

# 1. Fit the initial deviations by maximum likelihood (first 80% of games)
fit <- pglicko_fit(rps_league, c_rd = 0)
fit                       # parameters plus test log loss, Brier score, AUC, accuracy

# 2. Rate everyone with the fitted parameters
p <- pglicko_run(rps_league, fit$par[["init_sd"]], fit$par[["init_pair_sd"]], c_rd = 0)
p$ratings                 # one row per player
p$pairs                   # one row per pair that has played
pair_matrix(p, players = c("Rock", "Paper", "Scissors"))

# 3. Predict new games
predict(p, data.frame(Player1 = "Rock", Player2 = "Scissors"))

# 4. Continue as new periods arrive
# p_next <- pglicko_run(new_games, init_sd, init_pair_sd, start = p)
```

For plain Glicko, use `glicko_fit()` / `glicko_run()`. The full guide, with the
model equations, is in the vignette: `vignette("PGlicko")`.

## Data format

One row per game:

| column        | meaning                                          |
|---------------|--------------------------------------------------|
| `Season`      | rating period (any sortable values)              |
| `Player1`     | player name or ID                                |
| `Player2`     | opponent                                         |
| `Game_Result` | 1 = Player1 won, 0 = Player1 lost, 0.5 = draw     |

`as_match_data()` renames and checks other layouts.

## Functions

| function | purpose |
|---|---|
| `glicko_run()`, `pglicko_run()` | rate all games period by period; walk-forward predictions; continue with `start` |
| `glicko_fit()`, `pglicko_fit()` | maximum-likelihood fit of σ<sub>0</sub> (and b<sub>0</sub>), with test-game measures |
| `glicko_loglik()`, `pglicko_loglik()` | the likelihood objective (and test AUC / accuracy) |
| `predict()` | win probabilities for new games from a run's final state |
| `pair_matrix()` | the pairwise strengths, deviations or game counts as a players × players matrix |
| `win_probability()`, `glicko_g()` | the model formulas |
| `as_match_data()` | convert and validate game data |
| `rps_league`, `rps_truth` | example data and their true probabilities |

Every function has a help page with examples, for example `?pglicko_run`.

## Model settings

* `c_rd`: Glicko's RD growth per period, σ ← min(√(σ² + c²), `rd_max`). The default
  is 63.2. Use 0 when strengths do not change over time (see the vignette, section 8).
* `rd_max`: cap on all deviations. Default 350.
* `train_frac`: share of games, in row order, used for fitting. Default 0.8.
* `prediction`:
  * `"lagged_mean"` (the default for the likelihood, as in the paper): 6-period mean
    ratings;
  * `"current"`: ratings at the start of the period.

## Memory

Pairwise values are stored only for pairs that have played, so memory grows with the
number of pairs that meet, not with the square of the number of players.

## Research scripts

The original scripts are kept in the repository root and are not part of the
package:

* `Glicko_functions_optimized_v4.R`
* `PGlicko_functions_optimized_v3.R`
* `Train_all_models.R`
* `Prob_recovery_all_models__for_simulations_only_.R`

The package reproduces the scripts' likelihoods, predictions and fitted parameters
exactly. The vignette's last section maps the script functions to the package
functions.

## License

MIT
