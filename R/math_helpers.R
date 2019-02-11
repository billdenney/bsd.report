#' Compute the Emax or its inverse.
#'
#' @details The equation for the Emax function is
#'
#'   \deqn{effect = e0 + emax * x^{hill}/(ex50^{hill} + x^{hill})}
#'
#' @param x The input paramter (often a concentration)
#' @param effect The effect
#' @param e0 Baseline effect
#' @param emax Maximum effect (at the limit as \code{x} approaches \code{Inf})
#' @param ex50 Amount of x yielding 50% of the maximum effect
#' @param hill The hill slope (also known as the sigmoidicity)
#' @return \code{effect} for \code{emax_fun} or \code{x} for \code{inverse_emax}
#' @export
emax_fun <- function(x, e0, emax, ex50, hill=1) {
  e0 + emax*x^hill/(ex50^hill + x^hill)
}
#' @describeIn emax_fun Inverse emax
#' @export
inverse_emax <- function(effect, e0, emax, ex50, hill=1) {
  current_effect <- effect - e0
  (current_effect*(ex50^hill)/(emax*(1-current_effect/emax)))^(1/hill)
}
