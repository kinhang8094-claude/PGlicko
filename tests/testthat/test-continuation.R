test_that("continuing a run equals one run over all games", {
  d <- small_league(10, n_players = 9, n_periods = 12)
  # a player who only appears in the later part
  d <- rbind(d, data.frame(Season = 12, Player1 = "Late", Player2 = "P1", Game_Result = 1))
  for (model in c("glicko", "pglicko")) {
    run <- if (model == "glicko") function(x, s = NULL) glicko_run(x, 120, start = s)
           else function(x, s = NULL) pglicko_run(x, 120, 90, start = s)
    full <- run(d)
    a <- run(d[d$Season <= 5, ]); b <- run(d[d$Season > 5, ], a)
    m <- match(full$ratings$Player, b$ratings$Player)
    expect_equal(b$ratings[m, -1], full$ratings[, -1], ignore_attr = TRUE)
    expect_equal(b$predictions, full$predictions[d$Season > 5])
    if (model == "pglicko") {
      M1 <- pair_matrix(full); M2 <- pair_matrix(b, players = rownames(M1))
      expect_equal(M2, M1)
    }
  }
})

test_that("start must match the model", {
  d <- small_league(11)
  g <- glicko_run(d, 100)
  expect_error(pglicko_run(d, 100, 80, start = g), "Glicko")
  expect_error(glicko_run(d, 100, start = list()), "start")
})
