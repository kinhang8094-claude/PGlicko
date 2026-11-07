# Straightforward reference implementation written from the equations, one game and
# one pair at a time, with named lists. Used to check the vectorised engine.
ref_run <- function(data, init_sd, init_pair_sd = NA, pairwise = FALSE,
                    rd_max = 350, c_rd = 63.2, init_rating = 1500) {
  q <- log(10) / 400
  g <- function(s) 1 / sqrt(1 + 3 * q^2 * s^2 / pi^2)
  mu <- list(); sg <- list(); played <- list(); beta <- list(); bsd <- list()
  key <- function(a, b) paste(sort(c(a, b)), collapse = "\r")
  getb <- function(a, b) {             # beta of a over b, and b_ab
    k <- key(a, b)
    if (is.null(beta[[k]])) return(c(0, init_pair_sd))
    s <- if (a < b) 1 else -1
    c(s * beta[[k]], bsd[[k]])
  }
  preds <- numeric(nrow(data))
  for (t in sort(unique(data$Season))) {
    r <- which(data$Season == t)
    d <- data[r, ]
    for (p in unique(c(d$Player1, d$Player2))) if (is.null(mu[[p]])) {
      mu[[p]] <- init_rating; sg[[p]] <- init_sd; played[[p]] <- FALSE
    }
    for (k in seq_along(r)) {          # predictions from the start of the period
      a <- d$Player1[k]; b <- d$Player2[k]
      pb <- if (pairwise) getb(a, b) else c(0, 0)
      preds[r[k]] <- 1 / (1 + 10^(-g(sqrt(sg[[a]]^2 + sg[[b]]^2 + pb[2]^2)) *
                                    (mu[[a]] + pb[1] - mu[[b]]) / 400))
    }
    for (p in names(mu)) if (played[[p]]) sg[[p]] <- min(sqrt(sg[[p]]^2 + c_rd^2), rd_max)
    mu0 <- mu; sg0 <- sg
    beta0 <- beta; bsd0 <- bsd
    getb0 <- function(a, b) {
      k <- key(a, b)
      if (is.null(beta0[[k]])) return(c(0, init_pair_sd))
      c((if (a < b) 1 else -1) * beta0[[k]], bsd0[[k]])
    }
    for (p in unique(c(d$Player1, d$Player2))) {
      v <- 0; delta <- 0
      for (k in seq_len(nrow(d))) {
        if (d$Player1[k] == p) { o <- d$Player2[k]; s <- d$Game_Result[k] }
        else if (d$Player2[k] == p) { o <- d$Player1[k]; s <- 1 - d$Game_Result[k] }
        else next
        pb <- if (pairwise) getb0(p, o) else c(0, 0)
        G <- g(sqrt(sg0[[o]]^2 + pb[2]^2))
        E <- 1 / (1 + 10^(-G * (mu0[[p]] + pb[1] - mu0[[o]]) / 400))
        v <- v + G^2 * E * (1 - E); delta <- delta + G * (s - E)
      }
      sg[[p]] <- min(rd_max, sqrt(1 / (1 / sg0[[p]]^2 + q^2 * v)))
      mu[[p]] <- mu0[[p]] + q * sg[[p]]^2 * delta
      played[[p]] <- TRUE
    }
    if (pairwise) {
      pk <- unique(mapply(key, d$Player1, d$Player2))
      for (kk in pk) {
        pr <- strsplit(kk, "\r")[[1]]; i <- pr[1]; j <- pr[2]   # i < j
        b0 <- getb0(i, j)
        G <- g(sqrt(sg0[[i]]^2 + sg0[[j]]^2))
        E <- 1 / (1 + 10^(-G * (mu0[[i]] + b0[1] - mu0[[j]]) / 400))
        rows <- which((d$Player1 == i & d$Player2 == j) | (d$Player1 == j & d$Player2 == i))
        s <- ifelse(d$Player1[rows] == i, d$Game_Result[rows], 1 - d$Game_Result[rows])
        bn <- min(rd_max, sqrt(1 / (1 / b0[2]^2 + q^2 * length(s) * G^2 * E * (1 - E))))
        beta[[kk]] <- b0[1] + q * bn^2 * G * sum(s - E)
        bsd[[kk]] <- bn
      }
    }
  }
  list(mu = unlist(mu), sg = unlist(sg), preds = preds, beta = beta, bsd = bsd)
}

small_league <- function(seed = 1, n_players = 6, n_periods = 8, per = 10, draws = FALSE) {
  set.seed(seed)
  pl <- paste0("P", seq_len(n_players))
  str <- setNames(rnorm(n_players, 0, 150), pl)
  out <- do.call(rbind, lapply(seq_len(n_periods), function(t) {
    pr <- t(replicate(per, sample(pl, 2)))
    p <- 1 / (1 + 10^(-(str[pr[, 1]] - str[pr[, 2]]) / 400))
    res <- as.numeric(runif(per) < p)
    if (draws) res[runif(per) < 0.15] <- 0.5
    data.frame(Season = t, Player1 = pr[, 1], Player2 = pr[, 2], Game_Result = res,
               stringsAsFactors = FALSE)
  }))
  out
}
