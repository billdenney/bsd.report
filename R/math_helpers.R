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

#' Perform a cumulative sum resetting to zero whenever a \code{reset} value is
#' found.
#'
#' @param x The vector to sum over
#' @param reset The value when found in \code{x} to reset the sum to zero at.
#'   (Note that the value at \code{x \%in\% reset} is zero.
#' @return A vector the same length as \code{x}
#' @export
cumsum_reset <- function(x, reset=NA) {
  count <- 0
  ret <- rep(NA_integer_, length(x))
  for (i in seq_along(x)) {
    count <- count + x[i]
    if (x[i] %in% reset) {
      count <- 0
    }
    ret[i] <- count
  }
  ret
}
