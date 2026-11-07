#' PGlicko: Glicko and Pairwise Glicko rating systems
#'
#' Rates players in head-to-head competitions with the Glicko system, and with
#' P-Glicko, which adds a pairwise strength and a pairwise rating deviation for every
#' pair of players so that win probabilities can be intransitive.
#'
#' @section Main functions:
#' \describe{
#'   \item{[as_match_data()]}{Put match results into the standard layout.}
#'   \item{[glicko_run()], [pglicko_run()]}{Rate every player with given parameters,
#'     with walk-forward predictions; continue a run with new games via `start`.}
#'   \item{[glicko_fit()], [pglicko_fit()]}{Estimate the initial deviations by maximum
#'     likelihood and report test-set performance.}
#'   \item{[glicko_loglik()], [pglicko_loglik()]}{The objective behind the fits.}
#'   \item{[predict.pglicko_ratings()]}{Win probabilities for new games.}
#'   \item{[pair_matrix()]}{Pairwise values as a players x players matrix.}
#'   \item{[win_probability()], [glicko_g()]}{The underlying formulas.}
#' }
#'
#' @section The model in brief:
#' Games are grouped into rating periods. At the start of each period, the rating
#' deviation of every player who has played before grows,
#' \eqn{\sigma \leftarrow \min(\sqrt{\sigma^2 + c^2}, \sigma_{max})}. Then all of the
#' period's games update ratings and deviations simultaneously, as in Glicko. In
#' P-Glicko each pair \eqn{(i, j)} also has a pairwise strength \eqn{\beta_{ij}}
#' (\eqn{\beta_{ji} = -\beta_{ij}}) and a pairwise deviation \eqn{b_{ij}}, updated from
#' that pair's games; the expected score of i against j uses
#' \eqn{\mu_i + \beta_{ij} - \mu_j}. See `vignette("PGlicko")` for the equations.
#'
#' @section Memory:
#' Pairwise values are kept in a compact table with one row per pair that has played,
#' never in a players x players matrix, so large competitions (tens of thousands of
#' players) fit in memory.
#'
#' @references
#' Glickman, M. E. (1999). Parameter estimation in large dynamic paired comparison
#' experiments. *Journal of the Royal Statistical Society: Series C*, 48(3), 377-394.
#'
#' Wong, K. H. et al. Extending the Glicko system to allow intransitivity (P-Glicko).
#' Manuscript.
#'
#' @keywords internal
"_PACKAGE"

#' Simulated league with rock-paper-scissors matchups
#'
#' A simulated league of 8 players over 60 rating periods. In every period each pair
#' plays once (28 games per period, 1,680 in total). Three players, Rock, Paper and
#' Scissors, have the same overall strength but an intransitive cycle: Rock beats
#' Scissors, Scissors beats Paper and Paper beats Rock, each with probability 0.81.
#' The other five players differ only in overall strength. True win probabilities are
#' in [rps_truth].
#'
#' @format A data frame with 1,680 rows and 4 columns, in time order:
#' \describe{
#'   \item{Season}{Rating period, 1 to 60.}
#'   \item{Player1, Player2}{The two players (random order).}
#'   \item{Game_Result}{1 if Player1 won, 0 if Player2 won.}
#' }
#' @source Simulated; see `data-raw/rps_league.R` in the package source.
#' @examples
#' data(rps_league)
#' head(rps_league)
#' table(table(rps_league$Season))   # 28 games in every period
"rps_league"

#' True win probabilities of the simulated league
#'
#' The probabilities used to simulate [rps_league]:
#' \eqn{P(i \textrm{ beats } j) = 1 / (1 + 10^{-(s_i - s_j + c_{ij})/400})}, with overall
#' strengths \eqn{s} and pairwise effects \eqn{c_{ij}} (250 rating points within the
#' rock-paper-scissors cycle, 0 otherwise).
#'
#' @format A data frame with one row per pair of players (28 rows):
#' \describe{
#'   \item{Player1, Player2}{The pair.}
#'   \item{strength_diff}{\eqn{s_1 - s_2}, the difference in overall strength.}
#'   \item{pair_effect}{\eqn{c_{12}}, Player1's pairwise advantage over Player2.}
#'   \item{prob}{True probability that Player1 beats Player2.}
#' }
#' @source Simulated; see `data-raw/rps_league.R` in the package source.
#' @examples
#' data(rps_truth)
#' rps_truth[rps_truth$pair_effect != 0, ]
"rps_truth"
