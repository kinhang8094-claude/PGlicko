#' Fit Glicko or P-Glicko by maximum likelihood
#'
#' Estimates the initial rating deviation (Glicko), or the initial rating deviation and
#' initial pairwise deviation (P-Glicko), by maximising the log-likelihood of the
#' training games, then reports how the fitted model does on the test games.
#'
#' @inheritParams loglik
#' @param interval `glicko_fit()`: search interval for \eqn{\sigma_0}. Default `c(1, 1000)`.
#' @param start `pglicko_fit()`: starting values `c(init_sd, init_pair_sd)`. Default
#'   `c(300, 300)`, as in the original training script.
#' @param method,lower,upper,control `pglicko_fit()`: passed to [stats::optim()].
#'   Default method `"Nelder-Mead"` with `maxit = 500`. For a bounded search use
#'   `method = "L-BFGS-B"` with `lower` / `upper`.
#' @param objective What to maximise: `"loglik"` (default) or `"auc"` (test AUC, as in
#'   some of the original scripts; use with care, it optimises on the test games).
#' @param verbose If `TRUE`, print every evaluation.
#'
#' @return An object of class `pglicko_fit`, a list with
#' \describe{
#'   \item{model}{`"Glicko"` or `"P-Glicko"`.}
#'   \item{par}{Named vector of fitted parameters: `init_sd` (and `init_pair_sd`).}
#'   \item{loglik}{Training log-likelihood at the fitted parameters.}
#'   \item{test}{List of test-game measures at the fitted parameters: `auc`,
#'     `accuracy`, `logloss` (mean negative log-likelihood per game), `brier`, and
#'     the numbers of training and test games.}
#'   \item{trace}{Data frame with one row per evaluation of the objective: the
#'     parameters, training log-likelihood, test AUC and test accuracy. This replaces
#'     the global `record` object of the original scripts.}
#'   \item{convergence, counts, message}{From the optimiser.}
#'   \item{settings}{The fixed settings used (`rd_max`, `c_rd`, `train_frac`, ...).}
#' }
#' Use [glicko_run()] / [pglicko_run()] with the fitted parameters to get the ratings.
#'
#' @details
#' `glicko_fit()` uses [stats::optimize()] over `interval`; `pglicko_fit()` uses
#' [stats::optim()]. The objective is [glicko_loglik()] / [pglicko_loglik()] with the
#' same settings.
#'
#' @seealso [glicko_loglik()], [rating_runs]
#' @examples
#' data(rps_league)
#' fg <- glicko_fit(rps_league)
#' fp <- pglicko_fit(rps_league, start = c(150, 150), control = list(maxit = 100))
#' fg
#' fp
#' # Test log loss: lower is better
#' c(Glicko = fg$test$logloss, PGlicko = fp$test$logloss)
#'
#' # Ratings with the fitted parameters
#' r <- pglicko_run(rps_league, fp$par[["init_sd"]], fp$par[["init_pair_sd"]])
#' @name fitting
NULL

#' @rdname fitting
#' @export
glicko_fit <- function(data, interval = c(1, 1000), rd_max = 350, c_rd = 63.2, train_frac = 0.8,
                       prediction = c("lagged_mean", "current"), window = 6,
                       objective = c("loglik", "auc"), init_rating = 1500, verbose = FALSE) {
  prediction <- match.arg(prediction); objective <- match.arg(objective)
  data <- .check_matches(data)
  tr <- .tracer("Glicko", data, rd_max, c_rd, train_frac, prediction, window, init_rating, objective, verbose)
  o <- stats::optimize(function(s) tr$f(s), interval = interval, maximum = TRUE)
  .finish_fit("Glicko", c(init_sd = o$maximum), tr, data, rd_max, c_rd, train_frac, prediction,
              window, init_rating, objective, convergence = 0L, counts = c(`function` = nrow(tr$trace())),
              message = NULL)
}

#' @rdname fitting
#' @export
pglicko_fit <- function(data, start = c(300, 300), rd_max = 350, c_rd = 63.2, train_frac = 0.8,
                        prediction = c("lagged_mean", "current"), window = 6,
                        objective = c("loglik", "auc"), method = "Nelder-Mead",
                        lower = -Inf, upper = Inf, control = list(maxit = 500),
                        init_rating = 1500, verbose = FALSE) {
  prediction <- match.arg(prediction); objective <- match.arg(objective)
  data <- .check_matches(data)
  if (length(start) != 2) stop("`start` must be c(init_sd, init_pair_sd).")
  tr <- .tracer("P-Glicko", data, rd_max, c_rd, train_frac, prediction, window, init_rating, objective, verbose)
  control$fnscale <- -1
  o <- stats::optim(start, function(p) tr$f(p), method = method, lower = lower, upper = upper,
                    control = control)
  .finish_fit("P-Glicko", c(init_sd = o$par[1], init_pair_sd = o$par[2]), tr, data, rd_max, c_rd,
              train_frac, prediction, window, init_rating, objective, o$convergence, o$counts, o$message)
}

# Objective that records every evaluation
.tracer <- function(model, data, rd_max, c_rd, train_frac, prediction, window, init_rating,
                    objective, verbose) {
  rec <- list()
  f <- function(par) {
    v <- .loglik(par, data, model, rd_max, c_rd, train_frac, prediction, window, "all",
                 init_rating, checked = TRUE)
    rec[[length(rec) + 1L]] <<- c(par, v)
    if (verbose) message(sprintf("%s  loglik %.4f  auc %.4f  acc %.4f",
                                 paste(signif(par, 6), collapse = ", "), v[["loglik"]], v[["auc"]], v[["accuracy"]]))
    out <- if (objective == "loglik") v[["loglik"]] else v[["auc"]]
    if (is.na(out)) -Inf else out
  }
  trace <- function() {
    nm <- c(if (model == "P-Glicko") c("init_sd", "init_pair_sd") else "init_sd",
            "auc", "accuracy", "loglik")
    m <- do.call(rbind, rec)
    if (is.null(m)) m <- matrix(numeric(0), 0, length(nm))
    colnames(m) <- nm
    as.data.frame(m)
  }
  list(f = f, trace = trace)
}

.finish_fit <- function(model, par, tr, data, rd_max, c_rd, train_frac, prediction, window,
                        init_rating, objective, convergence, counts, message) {
  e <- .engine(data, par[1], if (model == "P-Glicko") par[2] else NA_real_,
               pairwise = model == "P-Glicko", rd_max = rd_max, c_rd = c_rd,
               init_rating = init_rating, prediction = prediction, window = as.integer(window))
  ev <- .evaluate(e$wp, data$Game_Result, train_frac)
  out <- list(model = model, par = par, loglik = ev$loglik,
              test = list(auc = ev$auc, accuracy = ev$accuracy, logloss = ev$test_logloss,
                          brier = ev$test_brier, n_train = ev$n_train, n_test = ev$n_test),
              trace = tr$trace(), convergence = convergence, counts = counts, message = message,
              settings = list(rd_max = rd_max, c_rd = c_rd, train_frac = train_frac,
                              prediction = prediction, window = window, objective = objective,
                              init_rating = init_rating))
  class(out) <- "pglicko_fit"
  out
}
