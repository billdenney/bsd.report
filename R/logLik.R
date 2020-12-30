#' An NA-equivalent value for logLik
#' @seealso \code{\link{logLik.xpose_data}}
#' @export
NA_logLik_ <- structure(NA_real_, df=NA_integer_, class="logLik")

#' Extract Log-Likelihood
#' 
#' See \code{stats::logLik()} for use
#' @inheritParams stats::logLik
#' @return a logLik object
#' @seealso \code{\link{NA_logLik_}}
#' @export
#' @importFrom xpose get_prm
#' @importFrom stats logLik
logLik.xpose_data <- function(object, ...) {
  structure(
    -0.5*as.numeric(rev(object$summary$value[object$summary$label == "ofv"])[1]),
    df=sum(!xpose::get_prm(object)$fixed),
    class="logLik"
  )
}

#' @describeIn logLik.xpose_data logLik for NULL returns an NA object
#' @export
logLik.NULL <- function(object, ...) {
  NA_logLik_
}

#' @describeIn logLik.xpose_data logLik for NA returns an NA object
#' @export
logLik.logical <- function(object, ...) {
  stopifnot("length must be 1"=length(object) == 1)
  stopifnot("logLik on a logical value must be NA"=is.na(object))
  NA_logLik_
}

#' @describeIn logLik.xpose_data logLik for try-error returns an NA object
#' @export
"logLik.try-error" <- function(object, ...) {
  NA_logLik_
}
