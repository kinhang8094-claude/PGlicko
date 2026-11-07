# Simulated league with intransitive (rock-paper-scissors) matchups.
# Run from the package root:  Rscript data-raw/rps_league.R
set.seed(2026)
strength <- c(Rock = 0, Paper = 0, Scissors = 0, Ace = 150, Bolt = 60, Cruz = 0, Dune = -60, Echo = -150)
players  <- names(strength)
# Pairwise effects (rating points): each of Rock, Paper, Scissors beats the next one
pair_effect <- matrix(0, 8, 8, dimnames = list(players, players))
cyc <- rbind(c("Rock", "Scissors"), c("Scissors", "Paper"), c("Paper", "Rock"))
for (k in seq_len(nrow(cyc))) { pair_effect[cyc[k, 1], cyc[k, 2]] <- 250; pair_effect[cyc[k, 2], cyc[k, 1]] <- -250 }
prs <- t(combn(players, 2))
true_p <- function(i, j) 1 / (1 + 10^(-(strength[i] - strength[j] + pair_effect[cbind(i, j)]) / 400))
rps_league <- do.call(rbind, lapply(1:60, function(s) {
  flip <- runif(nrow(prs)) < 0.5                       # random order of the two players
  p1 <- ifelse(flip, prs[, 2], prs[, 1]); p2 <- ifelse(flip, prs[, 1], prs[, 2])
  data.frame(Season = s, Player1 = p1, Player2 = p2,
             Game_Result = rbinom(nrow(prs), 1, true_p(p1, p2)), stringsAsFactors = FALSE)
}))
rownames(rps_league) <- NULL
rps_truth <- data.frame(Player1 = prs[, 1], Player2 = prs[, 2],
                        strength_diff = unname(strength[prs[, 1]] - strength[prs[, 2]]),
                        pair_effect = pair_effect[prs], prob = unname(true_p(prs[, 1], prs[, 2])),
                        stringsAsFactors = FALSE)
save(rps_league, file = "data/rps_league.rda", compress = "xz")
save(rps_truth, file = "data/rps_truth.rda", compress = "xz")
