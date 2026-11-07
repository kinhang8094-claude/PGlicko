test_that("as_match_data renames and checks", {
  x <- data.frame(w = c(2, 1), h = c("a", "b"), a = c("b", "c"), r = c(1, 0.5))
  m <- as_match_data(x, "w", "h", "a", "r")
  expect_named(m, c("Season", "Player1", "Player2", "Game_Result"))
  expect_equal(as_match_data(x), m)
})

test_that("invalid data are rejected", {
  ok <- data.frame(Season = 1, Player1 = "A", Player2 = "B", Game_Result = 1)
  expect_error(glicko_run(transform(ok, Game_Result = 2), 100), "Game_Result must")
  expect_error(glicko_run(transform(ok, Player2 = "A"), 100), "themself")
  expect_error(glicko_run(transform(ok, Player1 = NA), 100), "missing")
  expect_error(glicko_run(ok[, 1:3], 100), "Game_Result")
  expect_error(glicko_run(ok, -5), "init_sd")
  expect_error(pglicko_run(ok, 100, NA), "init_pair_sd")
})

test_that("bundled data are consistent", {
  expect_equal(dim(rps_league), c(1680L, 4L))
  expect_true(all(table(rps_league$Season) == 28))
  expect_equal(nrow(rps_truth), 28L)
})

test_that("a large sparse competition runs and stores only pairs that met", {
  set.seed(40)
  n <- 3000
  d <- data.frame(Season = rep(1:5, each = 2000),
                  Player1 = paste0("p", sample(n, 10000, TRUE)),
                  Player2 = paste0("p", sample(n, 10000, TRUE)),
                  Game_Result = rbinom(10000, 1, 0.5))
  d <- d[d$Player1 != d$Player2, ]
  r <- pglicko_run(d, 150, 100)
  expect_lte(nrow(r$pairs), nrow(d))
  expect_equal(sum(r$pairs$Games), nrow(d))
})
