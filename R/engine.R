# Internal rating engine shared by Glicko and P-Glicko.
#
# All games of a rating period are processed together, using the ratings,
# rating deviations (RDs) and pairwise values at the start of the period:
#   1. RD growth for every player who has already played:
#        sd <- min(sqrt(sd^2 + c^2), rd_max)
#   2. Player updates (Glicko; eqs 16-21 of the P-Glicko paper when pairwise = TRUE)
#   3. Pairwise updates (P-Glicko only; eqs 28-32): all games of an unordered pair
#      in the period are pooled; beta_ji = -beta_ij and b_ji = b_ij.
# Pairwise values are stored once per pair of players that meets.

.Q <- log(10) / 400
.K <- 3 * .Q^2 / pi^2

.engine <- function(data, init_sd, init_pair_sd = NA_real_, pairwise = TRUE,
                    rd_max = 350, c_rd = 63.2, init_rating = 1500, start = NULL,
                    prediction = c("current", "lagged_mean"), window = 6L,
                    keep_history = FALSE) {

  prediction <- match.arg(prediction)
  p_x <- sort(unique(data$Season))
  TT  <- length(p_x)
  period_labels <- as.character(p_x)

  # ---- players: sorted when starting fresh; otherwise the start object's
  #      players followed by new players in order of first appearance
  if (is.null(start)) {
    Players <- sort(unique(c(data$Player1, data$Player2)))
    np <- length(Players)
    mu <- rep(init_rating, np); sg <- rep(init_sd, np); debut <- logical(np)
    games <- win <- loss <- lag <- integer(np)
  } else {
    R0 <- start$ratings
    first_seen <- unique(unlist(lapply(split(data, factor(data$Season, levels = p_x)),
                                       function(d) unique(c(d$Player1, d$Player2)))))
    new_pl  <- setdiff(first_seen, R0$Player)
    Players <- c(R0$Player, new_pl)
    np <- length(Players); nn <- length(new_pl)
    mu    <- c(R0$Rating, rep(init_rating, nn))
    sg    <- c(R0$SD, rep(init_sd, nn))
    debut <- c(R0$Active, rep(FALSE, nn))
    games <- c(as.integer(R0$Games),  integer(nn)); win <- c(as.integer(R0$Wins), integer(nn))
    loss  <- c(as.integer(R0$Losses), integer(nn)); lag <- c(as.integer(R0$Lag),  integer(nn))
  }

  a_all <- match(data$Player1, Players)
  b_all <- match(data$Player2, Players)
  s_all <- data$Game_Result
  t_all <- match(data$Season, p_x)
  rows  <- split(seq_along(t_all), factor(t_all, levels = seq_len(TT)))

  # ---- pair table (P-Glicko): beta from the lower-index player's side, and b
  if (pairwise) {
    lo_all <- pmin(a_all, b_all); hi_all <- pmax(a_all, b_all)
    key_all <- lo_all * (np + 1) + hi_all
    pkey <- unique(key_all)
    beta <- numeric(length(pkey)); bsd <- rep(init_pair_sd, length(pkey))
    pgames <- integer(length(pkey))
    if (!is.null(start) && !is.null(start$pairs) && nrow(start$pairs) > 0) {
      i1 <- match(start$pairs$Player1, Players); i2 <- match(start$pairs$Player2, Players)
      sk <- pmin(i1, i2) * (np + 1) + pmax(i1, i2)
      sv <- ifelse(i1 <= i2, start$pairs$PairStrength, -start$pairs$PairStrength)
      extra <- !(sk %in% pkey)
      pkey <- c(pkey, sk[extra]); beta <- c(beta, numeric(sum(extra)))
      bsd <- c(bsd, numeric(sum(extra))); pgames <- c(pgames, integer(sum(extra)))
      m <- match(sk, pkey); beta[m] <- sv; bsd[m] <- start$pairs$PairSD
      pgames[m] <- as.integer(start$pairs$Games)
    }
    pid_all <- match(key_all, pkey)
    sgn_all <- ifelse(a_all < b_all, 1, ifelse(a_all > b_all, -1, 0))
  }

  keep_ts <- keep_history || prediction == "lagged_mean"
  if (keep_ts) {
    rating_ts <- matrix(init_rating, TT, np, dimnames = list(period_labels, Players))
    sd_ts     <- matrix(init_sd,     TT, np, dimnames = list(period_labels, Players))
  }

  n <- length(s_all)
  g_beta <- g_b <- numeric(n)
  if (prediction == "current") g_mua <- g_mub <- g_sga <- g_sgb <- numeric(n)
  q <- .Q; K <- .K

  for (t in seq_len(TT)) {
    r <- rows[[t]]
    a <- a_all[r]; b <- b_all[r]; s <- s_all[r]
    if (pairwise) {
      k <- pid_all[r]
      bet <- sgn_all[r] * beta[k]; bsdv <- bsd[k]
      g_beta[r] <- bet; g_b[r] <- bsdv
    } else {
      bet <- 0; bsdv <- 0
    }
    if (prediction == "current") { g_mua[r] <- mu[a]; g_mub[r] <- mu[b]; g_sga[r] <- sg[a]; g_sgb[r] <- sg[b] }

    # 1. RD growth for players who have already played
    sg[debut] <- pmin(sqrt(sg[debut]^2 + c_rd^2), rd_max)
    mu0 <- mu; sg0 <- sg

    # Bookkeeping (Game_Result = 1 means Player1 won)
    who   <- c(a, b)
    act   <- sort(unique(who))
    games <- games + tabulate(who, np)
    win   <- win   + tabulate(c(a[s == 1], b[s == 0]), np)
    loss  <- loss  + tabulate(c(a[s == 0], b[s == 1]), np)
    debut[act] <- TRUE
    idle  <- setdiff(which(debut), act)
    lag[idle] <- lag[idle] + 1L
    lag[act]  <- 0L

    # 2. Player updates: Player1's side and Player2's side of every game
    G1 <- 1 / sqrt(1 + K * (sg0[b]^2 + bsdv^2))
    E1 <- 1 / (1 + 10^(-G1 * (mu0[a] + bet - mu0[b]) / 400))
    G2 <- 1 / sqrt(1 + K * (sg0[a]^2 + bsdv^2))
    E2 <- 1 / (1 + 10^(-G2 * (mu0[b] - bet - mu0[a]) / 400))
    v  <- rowsum(cbind(c(G1^2 * E1 * (1 - E1), G2^2 * E2 * (1 - E2)),   # 1 / (q^2 delta^2)
                       c(G1 * (s - E1),        G2 * (1 - s - E2))),      # score residuals
                 who)                                                    # rows in order of `act`
    sg[act] <- pmin(rd_max, sqrt(1 / (1 / sg0[act]^2 + q^2 * v[, 1])))
    mu[act] <- mu0[act] + q * sg[act]^2 * v[, 2]

    # 3. Pairwise updates (P-Glicko): unordered pairs, all games of the period pooled
    if (pairwise) {
      lo  <- pmin(a, b); hi <- pmax(a, b)
      s_lo <- ifelse(a == lo, s, 1 - s)                    # result from `lo`'s side
      key <- lo * (np + 1) + hi
      uk  <- sort(unique(key))
      w   <- rowsum(cbind(1, s_lo), match(key, uk))        # games and sum of scores per pair
      pl  <- uk %/% (np + 1); ph <- uk %% (np + 1)
      kk  <- match(uk, pkey)
      G   <- 1 / sqrt(1 + K * (sg0[pl]^2 + sg0[ph]^2))
      E   <- 1 / (1 + 10^(-G * (mu0[pl] + beta[kk] - mu0[ph]) / 400))
      b_new    <- pmin(rd_max, sqrt(1 / (1 / bsd[kk]^2 + q^2 * w[, 1] * G^2 * E * (1 - E))))
      beta[kk] <- beta[kk] + q * b_new^2 * G * (w[, 2] - w[, 1] * E)
      bsd[kk]  <- b_new
      pgames[kk] <- pgames[kk] + as.integer(w[, 1])
    }

    if (keep_ts) { rating_ts[t, ] <- mu; sd_ts[t, ] <- sg }
  }

  # ---- predictions (probability that Player1 wins, made before the period's updates)
  if (prediction == "lagged_mean") {
    wp <- rep(0.5, n)
    ok <- which(t_all > window)
    if (length(ok) > 0) {
      tt <- t_all[ok]; i1 <- a_all[ok]; i2 <- b_all[ok]
      cs <- rbind(0, apply(rating_ts, 2, cumsum))
      mu_i <- (cs[cbind(tt, i1)] - cs[cbind(tt - window, i1)]) / window
      mu_j <- (cs[cbind(tt, i2)] - cs[cbind(tt - window, i2)]) / window
      wp[ok] <- .win_prob(mu_i, sd_ts[cbind(tt - 1, i1)], mu_j, sd_ts[cbind(tt - 1, i2)],
                          g_beta[ok], g_b[ok])
    }
  } else {
    wp <- .win_prob(g_mua, g_sga, g_mub, g_sgb, g_beta, g_b)
  }

  ratings <- data.frame(Player = Players, Rating = mu, SD = sg, Games = games, Wins = win,
                        Losses = loss, Lag = lag, Active = debut, stringsAsFactors = FALSE)
  pairs <- NULL
  if (pairwise) {
    pk_lo <- pkey %/% (np + 1); pk_hi <- pkey %% (np + 1)
    pairs <- data.frame(Player1 = Players[pk_lo], Player2 = Players[pk_hi],
                        PairStrength = beta, PairSD = bsd, Games = pgames,
                        stringsAsFactors = FALSE)
  }
  out <- list(ratings = ratings, pairs = pairs, wp = wp, periods = p_x)
  if (keep_history) out$history <- list(rating = rating_ts, sd = sd_ts)
  out
}

# Win probability (eq. 33 for Glicko, eq. 34 for P-Glicko)
.win_prob <- function(mu_i, sd_i, mu_j, sd_j, pair_strength = 0, pair_sd = 0) {
  1 / (1 + 10^(-glicko_g(sqrt(sd_i^2 + sd_j^2 + pair_sd^2)) * (mu_i + pair_strength - mu_j) / 400))
}
