#' Print the sessionInfo in a way that is usable in knitr/rmarkdown reports
#'
#' @param x The output of sessionInfo() (if missing, it is generated)
#' @param ... Ignored
#' @return A knitr \code{asis_output()} version of the session info.
#' @importFrom utils sessionInfo
#' @export
knit_print.sessionInfo <- function(x, ...) {
  if (missing(x))
    x <- utils::sessionInfo()
  packagename <- function(x) {
    sha <- ifelse(
      "GithubSHA1" %in% names(x),
      paste(", GithubSHA1:", soft_hyphenate(x$GithubSHA1)),
      ""
    )
    sprintf("%s(%s%s)", x$Package, x$Version, sha)
  }
  otherpkg <- sapply(x$otherPkgs, FUN=packagename)
  namespacepkg <- sapply(x$loadedOnly, FUN=packagename)
  ret <-
    c(paste("*", c(x$R.version$version.string,
                   paste("Platform:", x$platform))),
      paste("* Locale:", paste(strsplit(x$locale, ";")[[1]], collapse=", ")),
      "* Packages",
      paste("    * Base:", paste(x$basePkgs, collapse=", ")),
      paste("    * Attached:", paste(otherpkg, collapse="; ")),
      paste("    * Namespaces (not attached):", paste(namespacepkg, collapse="; ")))
  knitr::asis_output(paste(ret, collapse="\n"))
}

soft_hyphenate <- function(x, character_width = 8) {
  pattern <- sprintf("(.{%d})", character_width)
  ret <- gsub(x = x, pattern = pattern, replacement = "\\1\u00ad")
  # Drop hyphenation at the end in case there are exactly 8 characters
  gsub(x = ret, pattern = "\u00ad$", replacement = "")
}
