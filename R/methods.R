#' Predict games from a rating run
#'
#' Probability that `Player1` beats `Player2` for new games, using the ratings,
#' deviations and (for P-Glicko) pairwise values at the end of a rating run. The
#' ratings are not updated. A period of RD growth is applied first if `grow = TRUE`,
#' as it would be at the start of the next rating period.
#'
#' @param object A `pglicko_ratings` object from [glicko_run()] or [pglicko_run()].
#' @param newdata Data frame with columns `Player1` and `Player2` (other columns are
#'   ignored). Players not in `object` get the initial rating and deviation of the run,
#'   and pairs that have not played get pairwise strength 0 and the initial pairwise
#'   deviation.
#' @param grow If `TRUE` (default), apply one period of rating-deviation growth to
#'   players who have played, as at the start of the next period. With `FALSE`, use the
#'   deviations at the end of the last period.
#' @param ... Not used.
#'
#' @return Numeric vector of probabilities, one per row of `newdata`.
#'
#' @details This uses [win_probability()] with the current ratings. Note that
#'   [glicko_run()] / [pglicko_run()] predict each game from the start of its own period,
#'   before that period's RD growth; `grow = FALSE` matches that convention.
#'
#' @examples
#' data(rps_league)
#' p <- pglicko_run(rps_league, init_sd = 150, init_pair_sd = 120)
#' predict(p, data.frame(Player1 = c("Rock", "Paper", "Scissors"),
#'                       Player2 = c("Scissors", "Rock", "Paper")))
#' @export
predict.pglicko_ratings <- function(object, newdata, grow = TRUE, ...) {
  if (!all(c("Player1", "Player2") %in% names(newdata))) stop("`newdata` needs columns Player1 and Player2.")
  p1 <- as.character(newdata$Player1); p2 <- as.character(newdata$Player2)
  R <- object$ratings; pr <- object$params
  sdv <- R$SD
  if (grow) sdv[R$Active] <- pmin(sqrt(sdv[R$Active]^2 + pr$c_rd^2), pr$rd_max)
  i <- match(p1, R$Player); j <- match(p2, R$Player)
  mu_i <- ifelse(is.na(i), pr$init_rating, R$Rating[i]); mu_j <- ifelse(is.na(j), pr$init_rating, R$Rating[j])
  sd_i <- ifelse(is.na(i), pr$init_sd, sdv[i]);          sd_j <- ifelse(is.na(j), pr$init_sd, sdv[j])
  ps <- numeric(length(p1)); psd <- numeric(length(p1))
  if (object$model == "P-Glicko") {
    pp <- .pair_lookup(object$pairs, p1, p2, pr$init_pair_sd)
    ps <- pp$strength; psd <- pp$sd
  }
  win_probability(mu_i, sd_i, mu_j, sd_j, ps, psd)
}

# Pairwise strength (from p1's side) and deviation for each (p1, p2); defaults for unseen pairs
.pair_lookup <- function(pairs, p1, p2, default_sd) {
  k_tab <- paste(pairs$Player1, pairs$Player2, sep = "\r")
  fwd <- match(paste(p1, p2, sep = "\r"), k_tab); rev <- match(paste(p2, p1, sep = "\r"), k_tab)
  strength <- ifelse(!is.na(fwd), pairs$PairStrength[fwd], ifelse(!is.na(rev), -pairs$PairStrength[rev], 0))
  sd <- ifelse(!is.na(fwd), pairs$PairSD[fwd], ifelse(!is.na(rev), pairs$PairSD[rev], default_sd))
  list(strength = strength, sd = sd)
}

#' Pairwise values as a players x players matrix
#'
#' Turns the compact pair table of a P-Glicko run into a square matrix, for display or
#' for code that needs one. Memory grows with the square of the number of players, so
#' use the `pairs` table itself for large competitions.
#'
#' @param object A P-Glicko `pglicko_ratings` object.
#' @param what `"strength"` (default): entry `[i, j]` is i's pairwise strength over j
#'   (antisymmetric). `"sd"`: pairwise deviations (symmetric). `"games"`: number of
#'   games between the pair.
#' @param players Players for the rows and columns, in this order. Default: every rated
#'   player, in the order of `object$ratings`.
#' @param fill Value for pairs that have not played. Default 0 for `"strength"` and
#'   `"games"`, and the run's initial pairwise deviation for `"sd"`. The diagonal is `NA`.
#'
#' @return A numeric matrix with `players` as row and column names.
#' @examples
#' data(rps_league)
#' p <- pglicko_run(rps_league, init_sd = 150, init_pair_sd = 120)
#' round(pair_matrix(p, players = c("Rock", "Paper", "Scissors")), 1)
#' @export
pair_matrix <- function(object, what = c("strength", "sd", "games"), players = NULL, fill = NULL) {
  what <- match.arg(what)
  if (!inherits(object, "pglicko_ratings") || object$model != "P-Glicko") {
    stop("`object` must be a P-Glicko run (from pglicko_run()).")
  }
  if (is.null(players)) players <- object$ratings$Player
  players <- as.character(players)
  if (is.null(fill)) fill <- if (what == "sd") object$params$init_pair_sd else 0
  M <- matrix(fill, length(players), length(players), dimnames = list(players, players))
  pr <- object$pairs
  i <- match(pr$Player1, players); j <- match(pr$Player2, players)
  ok <- !is.na(i) & !is.na(j)
  val <- switch(what, strength = pr$PairStrength, sd = pr$PairSD, games = pr$Games)[ok]
  M[cbind(i[ok], j[ok])] <- val
  M[cbind(j[ok], i[ok])] <- if (what == "strength") -val else val
  diag(M) <- NA
  M
}

#' @export
print.pglicko_ratings <- function(x, n = 10, ...) {
  cat(sprintf("%s ratings: %d players, %d rating periods (%s to %s)\n", x$model, nrow(x$ratings),
              length(x$periods), format(x$periods[1]), format(x$periods[length(x$periods)])))
  cat(sprintf("Parameters: init_sd = %s%s, rd_max = %s, c_rd = %s, init_rating = %s\n",
              format(x$params$init_sd), if (x$model == "P-Glicko") paste0(", init_pair_sd = ", format(x$params$init_pair_sd)) else "",
              format(x$params$rd_max), format(x$params$c_rd), format(x$params$init_rating)))
  r <- x$ratings[x$ratings$Active, ]
  r <- r[order(-r$Rating), c("Player", "Rating", "SD", "Games", "Wins", "Losses", "Lag")]
  cat(sprintf("\nTop %d of %d players who have played:\n", min(n, nrow(r)), nrow(r)))
  r$Rating <- round(r$Rating, 1); r$SD <- round(r$SD, 1)
  print(utils::head(r, n), row.names = FALSE)
  if (x$model == "P-Glicko") {
    cat(sprintf("\n%d pairs have pairwise values (see $pairs or pair_matrix()).\n", nrow(x$pairs)))
  }
  invisible(x)
}

#' @export
print.pglicko_fit <- function(x, ...) {
  cat(sprintf("%s fit by maximum likelihood (%d evaluations)\n", x$model, nrow(x$trace)))
  cat(sprintf("  %s = %.4f\n", names(x$par), x$par), sep = "")
  cat(sprintf("  training log-likelihood: %.4f on %d games\n", x$loglik, x$test$n_train))
  if (x$test$n_test > 0) {
    cat(sprintf("  test games: %d | log loss %.5f | Brier %.5f | AUC %.4f | accuracy %.4f\n",
                x$test$n_test, x$test$logloss, x$test$brier, x$test$auc, x$test$accuracy))
  }
  if (!is.null(x$convergence) && x$convergence != 0) cat("  optimiser did not report convergence (code ", x$convergence, ")\n", sep = "")
  invisible(x)
}
