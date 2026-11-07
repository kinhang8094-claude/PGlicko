test_that("glicko_run matches a reference implementation of the equations", {
  d <- small_league(1, draws = TRUE)
  for (c_rd in c(0, 63.2)) {
    r <- glicko_run(d, init_sd = 120, c_rd = c_rd)
    ref <- ref_run(d, 120, c_rd = c_rd)
    expect_equal(r$ratings$Rating, unname(ref$mu[r$ratings$Player]), tolerance = 1e-10)
    expect_equal(r$ratings$SD, unname(ref$sg[r$ratings$Player]), tolerance = 1e-10)
    expect_equal(r$predictions, ref$preds, tolerance = 1e-10)
  }
})

test_that("pglicko_run matches a reference implementation of the equations", {
  d <- small_league(2, draws = TRUE)
  r <- pglicko_run(d, init_sd = 100, init_pair_sd = 80)
  ref <- ref_run(d, 100, 80, pairwise = TRUE)
  expect_equal(r$ratings$Rating, unname(ref$mu[r$ratings$Player]), tolerance = 1e-10)
  expect_equal(r$ratings$SD, unname(ref$sg[r$ratings$Player]), tolerance = 1e-10)
  expect_equal(r$predictions, ref$preds, tolerance = 1e-10)
  k <- paste(pmin(r$pairs$Player1, r$pairs$Player2), pmax(r$pairs$Player1, r$pairs$Player2), sep = "\r")
  sgn <- ifelse(r$pairs$Player1 < r$pairs$Player2, 1, -1)
  expect_equal(r$pairs$PairStrength, sgn * unlist(ref$beta[k], use.names = FALSE), tolerance = 1e-10)
  expect_equal(r$pairs$PairSD, unlist(ref$bsd[k], use.names = FALSE), tolerance = 1e-10)
})

test_that("P-Glicko with zero pairwise deviation equals Glicko", {
  d <- small_league(3)
  g <- glicko_run(d, 150)
  # a tiny pair SD keeps the pairwise strengths at (almost) zero
  p <- pglicko_run(d, 150, 1e-8)
  expect_equal(p$ratings$Rating, g$ratings$Rating, tolerance = 1e-6)
  expect_equal(p$predictions, g$predictions, tolerance = 1e-8)
})

test_that("pair table is antisymmetric and counts games", {
  d <- small_league(4)
  p <- pglicko_run(d, 100, 80)
  key <- paste(pmin(d$Player1, d$Player2), pmax(d$Player1, d$Player2))
  expect_equal(nrow(p$pairs), length(unique(key)))
  expect_equal(sum(p$pairs$Games), nrow(d))
  M <- pair_matrix(p)
  expect_equal(M, -t(M))
  S <- pair_matrix(p, "sd")
  expect_equal(S, t(S))
  expect_equal(sum(pair_matrix(p, "games"), na.rm = TRUE), 2 * nrow(d))
})

test_that("player counts and lag are right", {
  d <- data.frame(Season = c(1, 1, 2, 3), Player1 = c("A", "B", "A", "C"),
                  Player2 = c("B", "C", "C", "A"), Game_Result = c(1, 0.5, 0, 1))
  r <- glicko_run(d, 100)
  rt <- r$ratings[match(c("A", "B", "C"), r$ratings$Player), ]
  expect_equal(rt$Games, c(3, 2, 3))
  expect_equal(rt$Wins, c(1, 0, 2))
  expect_equal(rt$Losses, c(2, 1, 0))
  expect_equal(rt$Lag, c(0, 2, 0))
  expect_true(all(rt$Active))
})

test_that("RD growth is capped and skips players who have not played", {
  d <- data.frame(Season = 1:3, Player1 = "A", Player2 = "B", Game_Result = 1)
  r <- glicko_run(d, 340, rd_max = 350, c_rd = 500)
  expect_true(all(r$ratings$SD <= 350))
  # new player C joining in period 3 starts from init_sd, without growth
  d2 <- rbind(d, data.frame(Season = 3, Player1 = "C", Player2 = "A", Game_Result = 0))
  p3 <- glicko_run(d2, 100, c_rd = 63.2)
  ref <- ref_run(d2, 100, c_rd = 63.2)
  expect_equal(p3$ratings$SD, unname(ref$sg[p3$ratings$Player]), tolerance = 1e-10)
})

test_that("history has one row per period", {
  d <- small_league(5)
  r <- pglicko_run(d, 100, 80, keep_history = TRUE)
  expect_equal(dim(r$history$rating), c(8L, nrow(r$ratings)))
  expect_equal(unname(r$history$rating[8, ]), r$ratings$Rating)
  expect_equal(unname(r$history$sd[8, ]), r$ratings$SD)
})

test_that("results do not depend on the row order within a period", {
  d <- small_league(6)
  set.seed(9); d2 <- d[order(d$Season, runif(nrow(d))), ]
  a <- pglicko_run(d, 100, 80); b <- pglicko_run(d2, 100, 80)
  expect_equal(a$ratings, b$ratings)
  ka <- paste(a$pairs$Player1, a$pairs$Player2); kb <- paste(b$pairs$Player1, b$pairs$Player2)
  sa <- a$pairs$PairStrength; sb <- b$pairs$PairStrength
  # same pairs (possibly listed in the other orientation)
  ua <- paste(pmin(a$pairs$Player1, a$pairs$Player2), pmax(a$pairs$Player1, a$pairs$Player2))
  ub <- paste(pmin(b$pairs$Player1, b$pairs$Player2), pmax(b$pairs$Player1, b$pairs$Player2))
  va <- ifelse(a$pairs$Player1 < a$pairs$Player2, sa, -sa)
  vb <- ifelse(b$pairs$Player1 < b$pairs$Player2, sb, -sb)
  expect_equal(va[order(ua)], vb[order(ub)])
})

test_that("numeric player IDs and unsorted seasons work", {
  d <- small_league(7)
  d$Player1 <- as.integer(sub("P", "", d$Player1)) * 10
  d$Player2 <- as.integer(sub("P", "", d$Player2)) * 10
  r <- pglicko_run(d, 100, 80)
  expect_type(r$ratings$Player, "character")
  rev_d <- d[nrow(d):1, ]
  r2 <- pglicko_run(rev_d, 100, 80)
  expect_equal(r2$ratings$Rating[match(r$ratings$Player, r2$ratings$Player)], r$ratings$Rating)
  expect_equal(r2$predictions, rev(r$predictions))
})
