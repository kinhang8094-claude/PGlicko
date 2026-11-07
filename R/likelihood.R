#' Log-likelihood of Glicko or P-Glicko parameters
#'
#' Runs the rating system over all of `data` with the given initial deviations and
#' returns the log-likelihood of the results in the training share of the games. This
#' is the objective that [glicko_fit()] and [pglicko_fit()] maximise. Optionally it also
#' returns the AUC and accuracy on the remaining (test) games.
#'
#' @param par For `glicko_loglik()`: the initial rating deviation \eqn{\sigma_0}. For
#'   `pglicko_loglik()`: `c(init_sd, init_pair_sd)`. Non-positive values give `-Inf`.
#' @inheritParams rating_runs
#' @param train_frac Share of games, in row order, used for the log-likelihood (the
#'   training games). The rest are the test games. Default 0.8. Use 1 to use every game.
#' @param prediction How each game is predicted:
#'   * `"lagged_mean"` (default): the method of the P-Glicko paper's empirical study.
#'     Each player's rating is the mean of their ratings at the end of the previous
#'     `window` periods; deviations and pairwise values are those at the end of the
#'     previous period. Games in the first `window` periods are given probability 0.5.
#'   * `"current"`: ratings, deviations and pairwise values at the start of the game's
#'     period, as in [glicko_run()] / [pglicko_run()].
#' @param window Number of previous periods averaged when `prediction = "lagged_mean"`.
#'   Default 6.
#' @param metric What to return: `"loglik"` (default), `"auc"`, `"accuracy"`, or
#'   `"all"` for all three.
#'
#' @return For `metric = "loglik"`: the log-likelihood (natural log) of the training
#'   games, \eqn{\sum \log p^{y} (1-p)^{1-y}}. For `"auc"` / `"accuracy"`: that measure
#'   on the test games (`NA` if there are none). For `"all"`: a named vector with
#'   `auc`, `accuracy` and `loglik`.
#'
#' @details
#' The whole data set is rated; the split only decides which games count towards the
#' likelihood and which towards the test measures. Predictions never use the game being
#' predicted or any later game, so test measures are out of sample.
#'
#' AUC is the probability that a randomly chosen game Player1 won got a higher predicted
#' probability than a randomly chosen game Player1 lost (ties count half). Accuracy is
#' the share of test games where the predicted probability is above 0.5 and Player1 won,
#' or below or equal to 0.5 and Player1 lost. Draws (0.5) are left out of both.
#'
#' The AUC direction is fixed (a higher predicted probability should mean a win), so an
#' AUC below 0.5 means the predictions are worse than chance. This differs from
#' `pROC::auc()` with its default `direction = "auto"`, which picks whichever direction
#' gives an AUC of at least 0.5 and so reports \eqn{1 - \textrm{AUC}} in that case.
#'
#' @seealso [glicko_fit()], [pglicko_fit()]
#' @examples
#' data(rps_league)
#' glicko_loglik(150, rps_league)
#' pglicko_loglik(c(150, 120), rps_league, metric = "all")
#'
#' # The likelihood surface over the initial deviation
#' sds <- c(50, 100, 150, 200, 300)
#' sapply(sds, glicko_loglik, data = rps_league)
#' @name loglik
NULL

#' @rdname loglik
#' @export
glicko_loglik <- function(par, data, rd_max = 350, c_rd = 63.2, train_frac = 0.8,
                          prediction = c("lagged_mean", "current"), window = 6,
                          metric = c("loglik", "auc", "accuracy", "all"), init_rating = 1500) {
  .loglik(par, data, "Glicko", rd_max, c_rd, train_frac, match.arg(prediction), window,
          match.arg(metric), init_rating)
}

#' @rdname loglik
#' @export
pglicko_loglik <- function(par, data, rd_max = 350, c_rd = 63.2, train_frac = 0.8,
                           prediction = c("lagged_mean", "current"), window = 6,
                           metric = c("loglik", "auc", "accuracy", "all"), init_rating = 1500) {
  .loglik(par, data, "P-Glicko", rd_max, c_rd, train_frac, match.arg(prediction), window,
          match.arg(metric), init_rating)
}

.loglik <- function(par, data, model, rd_max, c_rd, train_frac, prediction, window, metric,
                    init_rating, checked = FALSE) {
  if (!checked) data <- .check_matches(data)
  if (!(is.numeric(train_frac) && length(train_frac) == 1 && train_frac > 0 && train_frac <= 1)) {
    stop("`train_frac` must be in (0, 1].")
  }
  need <- if (model == "P-Glicko") 2 else 1
  if (length(par) != need) stop(model, " needs ", need, " parameter(s) in `par`.")
  bad <- c(auc = NA_real_, accuracy = NA_real_, loglik = -Inf)
  if (any(!is.finite(par)) || any(par <= 0)) {
    return(if (metric == "all") bad else bad[[metric]])
  }
  e <- .engine(data, par[1], if (need == 2) par[2] else NA_real_, pairwise = need == 2,
               rd_max = rd_max, c_rd = c_rd, init_rating = init_rating,
               prediction = prediction, window = as.integer(window))
  ev <- .evaluate(e$wp, data$Game_Result, train_frac)
  out <- c(auc = ev$auc, accuracy = ev$accuracy, loglik = ev$loglik)
  if (metric == "all") out else out[[metric]]
}

# Training log-likelihood and test measures for predictions wp of results y
.evaluate <- function(wp, y, train_frac) {
  n <- length(y); train_end <- floor(train_frac * n)
  tr <- seq_len(train_end)
  loglik <- sum(log(wp[tr]^y[tr] * (1 - wp[tr])^(1 - y[tr])))
  te <- if (train_end < n) (train_end + 1L):n else integer(0)
  te <- te[y[te] != 0.5]
  if (length(te)) {
    auc <- .auc(wp[te], y[te])
    acc <- mean((wp[te] > 0.5) == (y[te] == 1))
    ll_te <- -mean(log(ifelse(y[te] == 1, wp[te], 1 - wp[te])))
    brier <- mean((wp[te] - y[te])^2)
  } else auc <- acc <- ll_te <- brier <- NA_real_
  list(loglik = loglik, auc = auc, accuracy = acc, test_logloss = ll_te, test_brier = brier,
       n_train = length(tr), n_test = length(te))
}

# AUC (Mann-Whitney): P(score of a random positive > score of a random negative), ties half
.auc <- function(p, y) {
  pos <- y == 1; n1 <- sum(pos); n0 <- sum(!pos)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(p)
  (sum(r[pos]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
