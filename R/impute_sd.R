#' Check the input of impute_sd functions
#' @inheritParams impute_sd
#' @noRd
impute_sd_check_input <- function(point, var1, var2, n, vartype, allow_na_vartype=FALSE) {
  stopifnot(all(c(length(var1), length(var2), length(n)) == length(point)))
  stopifnot(length(vartype) %in% c(1, length(point)))
  if (!allow_na_vartype) {
    stopifnot(all(!is.na(vartype)))
  }
}

#' Impute standard deviation from measures of dispersion.
#'
#' @details \code{impute_sd} selects the imputation method based on
#'   \code{vartype}.
#'
#' @param point The point estimate (measurement of central tendency)
#' @param var1,var2 The first and second measures of dispersion (`var2` is not
#'   required for all imputation methods)
#' @param n The number of observations contributing to the measurement
#' @param vartype The type of dispersion (case insensitive; see Details)
#' @return A vector of imputed standard deviations
#' @references Cochrane Handbook, version 5.1
#'   \url{https://handbook-5-1.cochrane.org/chapter_7/7_7_3_data_extraction_for_continuous_outcomes.htm}
#' @export
impute_sd <- function(point, var1, var2, n, vartype) {
  impute_sd_check_input(point, var1, var2, n, vartype, allow_na_vartype=TRUE)
  input_data <-
    data.frame(
      point=point,
      var1=var1,
      var2=var2,
      n=n,
      vartype=vartype,
      sd=NA_real_,
      stringsAsFactors=FALSE
    )
  for (current_vartype in unique(toupper(vartype))) {
    current_mask <- input_data$vartype %in% current_vartype
    impute_sd_FUN <-
      if (current_vartype %in% "SD") {
        impute_sd_sd
      } else if (current_vartype %in% c("SE", "SEM")) {
        impute_sd_se
      } else if (current_vartype %in% c("CV", "%CV", "CV%")) {
        impute_sd_cv
      } else if (grepl(pattern="^([0-9]+)% CI", x=current_vartype, ignore.case=TRUE)) {
        impute_sd_ci
      } else if (current_vartype %in% "IQR") {
        impute_sd_iqr
      } else if (current_vartype %in% "RANGE") {
        impute_sd_range
      } else if (current_vartype %in% NA) {
        message(
          sum(current_mask), " NA vartype ",
          ngettext(n=sum(current_mask), msg1="value is", msg2="values are"),
          " imputed as NA."
        )
        NA
      } else {
        stop("Unrecognized vartype")
      }
    if (!identical(NA, impute_sd_FUN)) {
      input_data$sd[current_mask] <-
        with(
          input_data[current_mask,],
          impute_sd_FUN(point, var1, var2, n, vartype)
        )
    }
  }
  input_data$sd
}

#' @rdname impute_sd
#' @details \code{impute_sd_sd()} imputes the standard deviation from itself
#'   (not really imputation).  \code{var1} is the standard deviation;
#'   \code{var2} must be \code{NA}; \code{vartype} must be "SD" (ignoring case);
#'   and \code{n} is ignored.
#' @export
impute_sd_sd <- function(point, var1, var2, n, vartype) {
  impute_sd_check_input(point, var1, var2, n, vartype)
  if (!all(toupper(vartype) %in% "SD")) {
    stop("vartype must be 'SD'")
  }
  if (!all(is.na(var2))) {
    stop("var2 must not be given for SD")
  }
  var1
}

#' @rdname impute_sd
#' @details \code{impute_sd_cv()} imputes the standard deviation from the
#'   coefficient of variation (CV).  \code{var1} is the CV; \code{var2} must be
#'   \code{NA}; \code{vartype} must be "CV", "\%CV", or "CV\%" (ignoring case);
#'   and \code{n} is ignored.
#' @export
impute_sd_cv <- function(point, var1, var2, n, vartype) {
  impute_sd_check_input(point, var1, var2, n, vartype)
  if (!all(toupper(vartype) %in% c("CV", "%CV", "CV%"))) {
    stop("vartype must be 'CV', '%CV', or 'CV%'")
  }
  if (!all(is.na(var2))) {
    stop("var2 must not be given for CV")
  }
  maybe_fraction <- !is.na(var1) & var1 < 1
  if (any(maybe_fraction)) {
    warning(
      "CV is usually given as a percent.  ",
      sum(maybe_fraction),
      " values < 1 have been provided which may be fraction instead of percent."
    )
  }
  point * var1/100
}

#' @rdname impute_sd
#' @details \code{impute_sd_se()} imputes the standard deviation from the
#'   standard error (SE).  \code{var1} is the standard error of the mean;
#'   \code{var2} must be \code{NA}; \code{vartype} must be "SE" or "SEM"
#'   (ignoring case); and \code{n} is required.  Imputation assumes a sample,
#'   not population, SE.
#' @export
impute_sd_se <- function(point, var1, var2, n, vartype) {
  impute_sd_check_input(point, var1, var2, n, vartype)
  if (!all(toupper(vartype) %in% c("SE", "SEM"))) {
    stop("vartype must be 'SE' or 'SEM'")
  }
  if (!all(is.na(var2))) {
    stop("var2 must not be given for SE")
  }
  var1*sqrt(n - 1)
}

#' @rdname impute_sd
#' @details \code{impute_sd_ci()} imputes the standard deviation from the
#'   confidence interval (CI).  \code{var1} is the lower bound of the CI;
#'   \code{var2} is the upper bound of the CI; \code{vartype} must match the
#'   regular expression "^([0-9]+)\% CI$" (ignoring case); and \code{n} is
#'   required.  If only an upper or lower bound of the CI is available, set the
#'   other to \code{NA}.  Imputation assumes a t-distribution.
#' @export
impute_sd_ci <- function(point, var1, var2, n, vartype) {
  impute_sd_check_input(point, var1, var2, n, vartype)
  pattern_vartype <- "^([0-9]+)% CI$"
  if (!all(grepl(pattern=pattern_vartype, x=vartype, ignore.case=TRUE))) {
    stop("vartype must match the regular expression: ", pattern_vartype)
  }
  ci_frac <-
    as.numeric(
      gsub(pattern=pattern_vartype, replacement="\\1", x=vartype, ignore.case=TRUE)
    )/100
  lower_ci <- point - var1
  upper_ci <- var2 - point
  if (any(ci_frac <= 0 | ci_frac >= 1)) {
    stop("The value for the confidence interval percent must be an integer between 0 and 100, exclusive.")
  }
  if (!all(is.na(lower_ci) | lower_ci > 0)) {
    stop("For CI, `var1` must be <= `point`.")
  }
  if (!all(is.na(upper_ci) | upper_ci > 0)) {
    stop("For CI, `var2` must be >= `point`.")
  }
  qval <- qt(p=1-(1-ci_frac)/2, df=n)
  lower_sd <- lower_ci/qval
  upper_sd <- upper_ci/qval
  # average the lower and upper CI
  rowMeans(x=cbind(lower_sd, upper_sd), na.rm=TRUE)
}

#' @rdname impute_sd
#' @details \code{impute_sd_iqr()} imputes the standard deviation from the
#'   inter-quartile range (IQR).  \code{var1} is the lower bound of the IQR;
#'   \code{var2} is the upper bound of the IQR; \code{vartype} must be "IQR"
#'   (ignoring case); and \code{n} is required.  Imputation assumes a
#'   t-distribution.
#' @export
impute_sd_iqr <- function(point, var1, var2, n, vartype) {
  # Ref Cochrane Handbook version 5.1, section 7.7.3.5
  # (https://handbook-5-1.cochrane.org/chapter_7/7_7_3_5_mediansand_interquartile_ranges.htm)
  impute_sd_check_input(point, var1, var2, n, vartype)
  if (!all(toupper(vartype) %in% "IQR")) {
    stop("vartype must be 'IQR'")
  }
  if (!all(var1 <= point)) {
    stop("For IQR, `var1` must be <= `point`.")
  } else if (!all(var2 >= point)) {
    stop("For IQR, `var2` must be >= `point`.")
  }
  ret <- (var2 - var1)/(2*qt(p=0.75, df=n))
  mask_bad_distribution_point_edge <-
    (!is.na(var1) & !is.na(point) & var1 == point) |
    (!is.na(var2) & !is.na(point) & var2 == point)
  if (any(mask_bad_distribution_point_edge)) {
    warning(
      "For IQR, `point` equals `var1` or `var2` for ",
      sum(mask_bad_distribution_point_edge),
      " values.  The distributional assumption of being a t-distribution is not accurate; returning NA for those values."
    )
    ret[mask_bad_distribution_point_edge] <- NA_real_
  }
  ret
}

#' @rdname impute_sd
#' @details \code{impute_sd_range()} imputes the standard deviation from the
#'   range.  \code{var1} is the lower bound of the range; \code{var2} is the
#'   upper bound of the range; \code{vartype} must be "RANGE" (ignoring case);
#'   and \code{n} is required.  Imputation is performed by computing \code{(var2
#'   - var2)/4}.  This should be the last choice of imputation methods.
#' @export
impute_sd_range <- function(point, var1, var2, n, vartype) {
  impute_sd_check_input(point, var1, var2, n, vartype)
  # Ref Cochrane Handbook version 5.1, section 7.7.3.6
  # (https://handbook-5-1.cochrane.org/chapter_7/7_7_3_6_ranges.htm)
  warning("Range is not a robust method for SD imputation.")
  if (!all(toupper(vartype) %in% "RANGE")) {
    stop("vartype must be 'RANGE'")
  }
  if (!all(var1 <= point)) {
    stop("For range, `var1` must be <= `point`.")
  } else if (!all(var2 >= point)) {
    stop("For range, `var2` must be >= `point`.")
  }
  ret <- (var2 - var1)/4
  mask_bad_distribution_point_edge <-
    (!is.na(var1) & !is.na(point) & var1 == point) |
    (!is.na(var2) & !is.na(point) & var2 == point)
  if (any(mask_bad_distribution_point_edge)) {
    warning(
      "For range, `point` equals `var1` or `var2` for ",
      sum(mask_bad_distribution_point_edge),
      " values.  The distributional assumption is not accurate; returning NA for those values."
    )
    ret[mask_bad_distribution_point_edge] <- NA_real_
  }
  ret
}
