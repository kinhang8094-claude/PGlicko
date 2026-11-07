test_that("loglik equals the sum over training games", {
  d <- small_league(30, n_periods = 15)
  e <- PGlicko:::.engine(d, 120, 90, pairwise = TRUE, prediction = "lagged_mean")
  ntr <- floor(0.8 * nrow(d))
  y <- d$Game_Result[1:ntr]; w <- e$wp[1:ntr]
  expect_equal(pglicko_loglik(c(120, 90), d), sum(y * log(w) + (1 - y) * log(1 - w)))
  ec <- PGlicko:::.engine(d, 120, pairwise = FALSE, prediction = "current")
  expect_equal(glicko_loglik(120, d, prediction = "current", train_frac = 1),
               sum(d$Game_Result * log(ec$wp) + (1 - d$Game_Result) * log(1 - ec$wp)))
  expect_equal(ec$wp, glicko_run(d, 120)$predictions)
})

test_that("lagged-mean predictions give 0.5 in the first window periods", {
  d <- small_league(31, n_periods = 10)
  e <- PGlicko:::.engine(d, 120, pairwise = FALSE, prediction = "lagged_mean", window = 6)
  expect_true(all(e$wp[d$Season <= 6] == 0.5))
  expect_false(any(e$wp[d$Season > 6] == 0.5))
})

test_that("metrics and invalid parameters", {
  d <- small_league(32, n_periods = 15)
  m <- pglicko_loglik(c(100, 80), d, metric = "all")
  expect_named(m, c("auc", "accuracy", "loglik"))
  expect_true(m[["auc"]] >= 0 && m[["auc"]] <= 1)
  expect_equal(glicko_loglik(-1, d), -Inf)
  expect_equal(pglicko_loglik(c(100, 0), d), -Inf)
  expect_true(is.na(glicko_loglik(100, d, train_frac = 1, metric = "auc")))
})

test_that("AUC is the Mann-Whitney statistic with a fixed direction", {
  p <- c(0.9, 0.8, 0.3, 0.2, 0.6); y <- c(1, 1, 0, 0, 0)
  expect_equal(PGlicko:::.auc(p, y), 1)
  expect_equal(PGlicko:::.auc(1 - p, y), 0)
  expect_equal(PGlicko:::.auc(c(0.5, 0.5), c(1, 0)), 0.5)
})

test_that("fits find the maximum and return a trace", {
  d <- small_league(33, n_periods = 15)
  fg <- glicko_fit(d)
  grid <- sapply(c(0.5, 0.9, 1.1, 1.5) * fg$par[["init_sd"]], glicko_loglik, data = d)
  expect_true(all(fg$loglik >= grid - 1e-6))
  expect_s3_class(fg, "pglicko_fit")
  expect_true(nrow(fg$trace) > 3)
  fp <- pglicko_fit(d, start = c(150, 150), control = list(maxit = 150))
  expect_named(fp$par, c("init_sd", "init_pair_sd"))
  expect_equal(fp$loglik, pglicko_loglik(fp$par, d))
  expect_true(fp$loglik >= pglicko_loglik(c(150, 150), d))
  expect_output(print(fp), "P-Glicko")
})
