#' Glicko g function
#'
#' The Glicko attenuation factor \eqn{g(\sigma) = 1 / \sqrt{1 + 3 q^2 \sigma^2 / \pi^2}},
#' with \eqn{q = \ln(10) / 400}. It shrinks a rating difference towards zero when the
#' ratings involved are uncertain.
#'
#' @param sigma Numeric vector of rating deviations (standard deviations, on the rating scale).
#'
#' @return Numeric vector of the same length as `sigma`, with values in (0, 1].
#'
#' @details In the P-Glicko paper the function is written \eqn{g(\sigma^2)}; this
#'   function takes the deviation itself, not its square.
#'
#' @seealso [win_probability()]
#' @examples
#' glicko_g(c(0, 50, 350))
#' @export
glicko_g <- function(sigma) {
  1 / sqrt(1 + 3 * .Q^2 * sigma^2 / pi^2)
}

#' Win probability under Glicko or P-Glicko
#'
#' Probability that player i beats player j, given both players' ratings and rating
#' deviations, and for P-Glicko the pair's pairwise strength and pairwise deviation:
#' \deqn{P(i \textrm{ beats } j) = \frac{1}{1 + 10^{-g\left(\sqrt{\sigma_i^2 + \sigma_j^2 + b_{ij}^2}\right)
#'   (\mu_i + \beta_{ij} - \mu_j) / 400}}}
#' With `pair_strength = 0` and `pair_sd = 0` this is the Glicko formula (eq. 33 of
#' the paper); otherwise it is the P-Glicko formula (eq. 34).
#'
#' @param rating_i,rating_j Ratings \eqn{\mu_i}, \eqn{\mu_j}.
#' @param sd_i,sd_j Rating deviations \eqn{\sigma_i}, \eqn{\sigma_j}.
#' @param pair_strength Pairwise strength \eqn{\beta_{ij}} of i over j (P-Glicko); 0 for Glicko.
#' @param pair_sd Pairwise deviation \eqn{b_{ij}} (P-Glicko); 0 for Glicko.
#'
#' @return Numeric vector of probabilities. Arguments are recycled.
#'
#' @seealso [predict.pglicko_ratings()] to get these probabilities from a rating run.
#' @examples
#' # Glicko: equal ratings give 0.5; a 200-point edge with low uncertainty
#' win_probability(1500, 50, 1500, 50)
#' win_probability(1700, 50, 1500, 50)
#'
#' # P-Glicko: a +80 pairwise strength can make the lower-rated player the favourite
#' win_probability(1450, 50, 1500, 50, pair_strength = 80, pair_sd = 40)
#' @export
win_probability <- function(rating_i, sd_i, rating_j, sd_j, pair_strength = 0, pair_sd = 0) {
  .win_prob(rating_i, sd_i, rating_j, sd_j, pair_strength, pair_sd)
}
