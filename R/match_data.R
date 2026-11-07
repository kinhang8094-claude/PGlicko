#' Prepare match data for PGlicko
#'
#' Converts a data frame of head-to-head results into the standard form used by every
#' function in the package: one row per game, with columns `Season`, `Player1`,
#' `Player2` and `Game_Result`.
#'
#' @param x A data frame with one row per game.
#' @param season,player1,player2,result Columns of `x` holding the rating period, the
#'   two players and the result. Each can be a column name or a column number. The
#'   defaults take the first four columns in that order, which is the layout of the
#'   original P-Glicko scripts (`c("Time", "A", "B", "match_result")` and similar).
#'
#' @return A data frame with columns
#' \describe{
#'   \item{Season}{The rating period. Any type that sorts in time order: integer,
#'     numeric, `Date`, or ordered text such as `"2021-03"`. Games with the same value
#'     are rated together, as one period.}
#'   \item{Player1, Player2}{The two players, as character. Numeric IDs are converted to
#'     text, so `101` and `"101"` are the same player.}
#'   \item{Game_Result}{1 if Player1 won, 0 if Player2 won, 0.5 for a draw.}
#' }
#' Row order is kept. Functions that split data into training and test games
#' ([glicko_loglik()], [pglicko_loglik()], the `*_fit()` functions) use the row order,
#' so rows should be in time order.
#'
#' @details The data are checked: no missing values, a player never plays themself, and
#'   results are 0, 0.5 or 1. Invalid data give an error that says what is wrong.
#'
#' @seealso [rps_league] for an example data set.
#' @examples
#' raw <- data.frame(Time = c(1, 1, 2), A = c("Ann", "Bob", "Ann"),
#'                   B = c("Bob", "Cat", "Cat"), match_result = c(1, 0, 1))
#' as_match_data(raw)
#'
#' # Columns can also be picked by name, in any order
#' raw2 <- data.frame(winner_first = c(1, 0), week = c(1, 2),
#'                    home = c("X", "Y"), away = c("Y", "X"))
#' as_match_data(raw2, season = "week", player1 = "home", player2 = "away",
#'               result = "winner_first")
#' @export
as_match_data <- function(x, season = 1, player1 = 2, player2 = 3, result = 4) {
  if (!is.data.frame(x)) stop("`x` must be a data frame.")
  pick <- function(col, what) {
    if (is.character(col)) {
      if (!col %in% names(x)) stop("Column '", col, "' (", what, ") not found in `x`.")
      return(x[[col]])
    }
    if (!is.numeric(col) || col < 1 || col > ncol(x)) stop("Invalid column for ", what, ".")
    x[[col]]
  }
  out <- data.frame(Season = pick(season, "season"),
                    Player1 = as.character(pick(player1, "player1")),
                    Player2 = as.character(pick(player2, "player2")),
                    Game_Result = as.numeric(pick(result, "result")),
                    stringsAsFactors = FALSE)
  .check_matches(out)
  out
}

# Validate (and lightly normalise) match data; returns the data with character players.
.check_matches <- function(data) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  need <- c("Season", "Player1", "Player2", "Game_Result")
  miss <- setdiff(need, names(data))
  if (length(miss)) {
    stop("`data` needs columns ", paste(need, collapse = ", "), " (missing: ",
         paste(miss, collapse = ", "), "). Use as_match_data() to convert other layouts.")
  }
  if (nrow(data) == 0) stop("`data` has no games.")
  data <- data[, need]
  data$Player1 <- as.character(data$Player1)
  data$Player2 <- as.character(data$Player2)
  if (anyNA(data)) stop("`data` contains missing values.")
  if (any(data$Player1 == data$Player2)) stop("A player can't play themself (Player1 == Player2).")
  if (!all(data$Game_Result %in% c(0, 0.5, 1))) {
    stop("Game_Result must be 1 (Player1 won), 0 (Player2 won) or 0.5 (draw).")
  }
  data
}
