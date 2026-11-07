#' Run Glicko or P-Glicko ratings over a set of games
#'
#' Rates every player in `data` period by period with fixed parameters. Before each
#' period's games are used, they are predicted from the ratings at the start of the
#' period (a walk-forward prediction); then all of the period's updates are applied
#' together. `glicko_run()` uses the Glicko system; `pglicko_run()` adds a pairwise
#' strength and a pairwise deviation for every pair of players.
#'
#' @param data Match data: a data frame with columns `Season`, `Player1`, `Player2`,
#'   `Game_Result` (see [as_match_data()]). Games with the same `Season` form one rating
#'   period; periods are processed in sorted order.
#' @param init_sd Initial rating deviation \eqn{\sigma_0} of every new player. Estimate
#'   it with [glicko_fit()] or [pglicko_fit()].
#' @param init_pair_sd Initial pairwise deviation \eqn{b_0} of every new pair (P-Glicko).
#' @param rd_max Maximum rating deviation (also caps pairwise deviations). Default 350.
#' @param c_rd Glicko's \eqn{c}: how much a player's rating deviation grows per rating
#'   period, \eqn{\sigma \leftarrow \min(\sqrt{\sigma^2 + c^2}, \textrm{rd\_max})}. It is
#'   applied at the start of each period to every player who has played before. Use 0 to
#'   switch growth off. Default 63.2.
#' @param init_rating Rating of every new player. Default 1500.
#' @param start Optional result of an earlier `glicko_run()` / `pglicko_run()` call to
#'   continue from (for example from training into test periods). Its players keep their
#'   ratings, deviations, counts and pairwise values; players and pairs that are new in
#'   `data` start from `init_rating`, `init_sd` and `init_pair_sd`. The model of `start`
#'   must match the function.
#' @param keep_history If `TRUE`, also return every player's rating and deviation at the
#'   end of every period (two periods x players matrices).
#'
#' @return An object of class `pglicko_ratings`, a list with
#' \describe{
#'   \item{model}{`"Glicko"` or `"P-Glicko"`.}
#'   \item{ratings}{Data frame, one row per player: `Player`, `Rating`, `SD` (rating
#'     deviation), `Games`, `Wins`, `Losses` (draws count in `Games` only), `Lag`
#'     (periods since the player last played) and `Active` (has played at least once).}
#'   \item{pairs}{P-Glicko only: a compact pair table with one row per pair of players
#'     that has played, with columns `Player1`, `Player2`, `PairStrength` (Player1's
#'     pairwise strength over Player2; Player2's over Player1 is its negative), `PairSD`
#'     and `Games`. Use [pair_matrix()] for a players x players matrix.}
#'   \item{predictions}{Numeric vector, one per row of `data` in the original order: the
#'     probability that Player1 wins, computed before that period's update.}
#'   \item{periods}{The rating periods processed, in order.}
#'   \item{history}{If `keep_history = TRUE`: list with matrices `rating` and `sd`
#'     (periods x players).}
#'   \item{params}{The parameters used.}
#' }
#'
#' @details
#' Within a rating period the update follows Glickman's Glicko system: the period's games
#' are treated as simultaneous, so every game is evaluated with the ratings from the start
#' of the period.
#'
#' P-Glicko (Wong et al.) gives each pair of players \eqn{(i, j)} a pairwise strength
#' \eqn{\beta_{ij}} with deviation \eqn{b_{ij}}, both updated with Glicko-style formulas
#' from the pair's own games. A player's expected score against an opponent uses
#' \eqn{\mu_i + \beta_{ij} - \mu_j}, and the combined deviation includes \eqn{b_{ij}}.
#' Win probabilities therefore need not be transitive.
#'
#' Pairwise values are stored only for pairs that have played (the `pairs` table), so
#' memory grows with the number of pairs that meet rather than with the square of the
#' number of players.
#'
#' @seealso [glicko_fit()], [pglicko_fit()] to estimate `init_sd` / `init_pair_sd`;
#'   [predict.pglicko_ratings()] to predict new games; [pair_matrix()];
#'   `vignette("PGlicko")`.
#'
#' @examples
#' data(rps_league)
#'
#' # Glicko
#' g <- glicko_run(rps_league, init_sd = 150)
#' g
#' head(g$ratings)
#'
#' # P-Glicko, keeping the per-period history
#' p <- pglicko_run(rps_league, init_sd = 150, init_pair_sd = 120, keep_history = TRUE)
#' head(p$pairs[order(-abs(p$pairs$PairStrength)), ])
#'
#' # Continue with later games: same result as one run over everything
#' early <- rps_league[rps_league$Season <= 30, ]
#' late  <- rps_league[rps_league$Season > 30, ]
#' p1 <- pglicko_run(early, init_sd = 150, init_pair_sd = 120)
#' p2 <- pglicko_run(late,  init_sd = 150, init_pair_sd = 120, start = p1)
#' full <- pglicko_run(rps_league, init_sd = 150, init_pair_sd = 120)
#' m <- match(full$ratings$Player, p2$ratings$Player)
#' all.equal(p2$ratings$Rating[m], full$ratings$Rating)
#' @name rating_runs
NULL

#' @rdname rating_runs
#' @export
glicko_run <- function(data, init_sd, rd_max = 350, c_rd = 63.2, init_rating = 1500,
                       start = NULL, keep_history = FALSE) {
  .run(data, "Glicko", init_sd, NA_real_, rd_max, c_rd, init_rating, start, keep_history)
}

#' @rdname rating_runs
#' @export
pglicko_run <- function(data, init_sd, init_pair_sd, rd_max = 350, c_rd = 63.2,
                        init_rating = 1500, start = NULL, keep_history = FALSE) {
  .run(data, "P-Glicko", init_sd, init_pair_sd, rd_max, c_rd, init_rating, start, keep_history)
}

.run <- function(data, model, init_sd, init_pair_sd, rd_max, c_rd, init_rating, start, keep_history) {
  data <- .check_matches(data)
  .check_params(init_sd, init_pair_sd, model, rd_max, c_rd)
  if (!is.null(start)) {
    if (!inherits(start, "pglicko_ratings")) stop("`start` must be the result of glicko_run() or pglicko_run().")
    if (start$model != model) stop("`start` comes from a ", start$model, " run, not ", model, ".")
  }
  e <- .engine(data, init_sd, init_pair_sd, pairwise = model == "P-Glicko", rd_max = rd_max,
               c_rd = c_rd, init_rating = init_rating, start = start, prediction = "current",
               keep_history = keep_history)
  out <- list(model = model, ratings = e$ratings, pairs = e$pairs, predictions = e$wp,
              periods = e$periods, history = e$history,
              params = list(init_sd = init_sd, init_pair_sd = if (model == "P-Glicko") init_pair_sd,
                            rd_max = rd_max, c_rd = c_rd, init_rating = init_rating))
  if (is.null(out$history)) out$history <- NULL
  class(out) <- "pglicko_ratings"
  out
}

.check_params <- function(init_sd, init_pair_sd, model, rd_max, c_rd) {
  ok <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x > 0
  if (!ok(init_sd)) stop("`init_sd` must be a single positive number.")
  if (model == "P-Glicko" && !ok(init_pair_sd)) stop("`init_pair_sd` must be a single positive number.")
  if (!ok(rd_max)) stop("`rd_max` must be a single positive number.")
  if (!(is.numeric(c_rd) && length(c_rd) == 1 && is.finite(c_rd) && c_rd >= 0)) {
    stop("`c_rd` must be a single number >= 0.")
  }
  invisible(TRUE)
}
