#' An NA-equivalent value for logLik
#' @export
NA_logLik_ <- structure(NA_real_, df=NA_integer_, class="logLik")

#' Extract Log-Likelihood for NA-like objects
#'
#' See \code{stats::logLik()} for use
#' @inheritParams stats::logLik
#' @return a logLik object
#' @seealso \code{\link{NA_logLik_}}
#' @export
#' @importFrom stats logLik
logLik.NULL <- function(object, ...) {
  NA_logLik_
}

#' @describeIn logLik.NULL logLik for NA returns an NA object
#' @export
logLik.logical <- function(object, ...) {
  stopifnot("length must be 1"=length(object) == 1)
  stopifnot("logLik on a logical value must be NA"=is.na(object))
  NA_logLik_
}

#' @describeIn logLik.NULL logLik for try-error returns an NA object
#' @export
"logLik.try-error" <- function(object, ...) {
  NA_logLik_
}
