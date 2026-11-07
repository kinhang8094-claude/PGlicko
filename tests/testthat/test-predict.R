test_that("predict uses the final state, with and without RD growth", {
  d <- small_league(20)
  p <- pglicko_run(d, 100, 80)
  nd <- data.frame(Player1 = c("P1", "P2"), Player2 = c("P3", "P1"))
  r <- p$ratings; pr <- p$pairs
  manual <- function(a, b, grow) {
    i <- match(a, r$Player); j <- match(b, r$Player)
    si <- r$SD[i]; sj <- r$SD[j]
    if (grow) { si <- min(sqrt(si^2 + 63.2^2), 350); sj <- min(sqrt(sj^2 + 63.2^2), 350) }
    k <- which(pr$Player1 == a & pr$Player2 == b); k2 <- which(pr$Player1 == b & pr$Player2 == a)
    bt <- if (length(k)) pr$PairStrength[k] else if (length(k2)) -pr$PairStrength[k2] else 0
    bs <- if (length(k)) pr$PairSD[k] else if (length(k2)) pr$PairSD[k2] else 80
    win_probability(r$Rating[i], si, r$Rating[j], sj, bt, bs)
  }
  expect_equal(predict(p, nd, grow = FALSE), mapply(manual, nd$Player1, nd$Player2, FALSE),
               ignore_attr = TRUE)
  expect_equal(predict(p, nd), mapply(manual, nd$Player1, nd$Player2, TRUE), ignore_attr = TRUE)
})

test_that("predict handles unknown players and is consistent in both orientations", {
  d <- small_league(21)
  p <- pglicko_run(d, 100, 80)
  pp <- predict(p, data.frame(Player1 = c("P1", "P2", "New"), Player2 = c("P2", "P1", "Other")))
  expect_equal(pp[1] + pp[2], 1)
  expect_equal(pp[3], 0.5)
  g <- glicko_run(d, 100)
  gg <- predict(g, data.frame(Player1 = c("P1", "P2"), Player2 = c("P2", "P1")))
  expect_equal(sum(gg), 1)
})

test_that("win_probability and glicko_g", {
  expect_equal(glicko_g(0), 1)
  expect_true(all(diff(glicko_g(c(0, 50, 100, 350))) < 0))
  expect_equal(win_probability(1500, 80, 1500, 80), 0.5)
  expect_equal(win_probability(1600, 0, 1500, 0), 1 / (1 + 10^(-100 / 400)))
  expect_equal(win_probability(1500, 50, 1500, 50, 100, 30) +
               win_probability(1500, 50, 1500, 50, -100, 30), 1)
})
