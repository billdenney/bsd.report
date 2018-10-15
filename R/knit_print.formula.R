#' Generate LaTeX math from an R formula
#'
#' @param x A formula
#' @param inline Should the formula be generated for use inline or as a separate
#'   line? (In LaTeX notation, with "$" or "$$".)
#' @param replacements A named list where whenever the list name is seen as the
#'   text for any complete part of the equation, it is replaced with the value
#'   (which should be a LaTeX equation fragment).
#' @param ... Currently ignored
#' @param width Approximate number of characters for splitting to multiple
#'   lines.  (Not yet implemented.)
#' @return A knitr \code{asis_output()} formula
#' @importFrom knitr asis_output
#' @examples
#' knit_print(a~b)
#' # The same with an equal sign
#' knit_print(a~b, replacements=list("\\sim"="="))
#' knit_print(a~b/c)
#' @export
knit_print.formula <- function(x, inline=TRUE, replacements=list(), ..., width=80) {
  ret <- knit_print_helper_formula(x, ..., replacements=replacements)
  # Handle the outer-most equation
  if (inline) {
    ret <- paste0("$", ret, "$")
  } else {
    ret <- paste0("$$", ret, "$$")
  }
  knitr::asis_output(
    paste(ret, collapse="\n"),
    cacheable=TRUE
  )
}

knit_print_helper_formula <- function(x, ...) {
  UseMethod("knit_print_helper_formula")
}

knit_print_helper_formula.formula <- function(x, ...) {
  if (!inherits(x, "formula")) {
    stop('x must be a formula.')
  }
  ret <-
    if (length(x) < 2) {
      stop("Unknown formula type (length < 2), please provide this as an example to improve the package") # nocov
    } else if (length(x) > 3) {
      stop("Unknown formula type (length > 3), please provide this as an example to improve the package") # nocov
    } else if (length(x) == 2) {
      # One-sided formula
      paste(knit_print_helper_formula(x[[1]], ...),
            knit_print_helper_formula(x[[2]], ...))
    } else if (length(x) == 3) {
      # Two-sided formula
      paste(knit_print_helper_formula(x[[2]], ...),
            knit_print_helper_formula(x[[1]], ...),
            knit_print_helper_formula(x[[3]], ...))
    }
  knit_print_helper_formula_replacements(ret, ...)
}

knit_print_helper_formula.name <- function(x, ...) {
  if (!is.name(x)) {
    stop("x must be a name.")
  }
  ret <- as.character(x)
  # Remap common names
  ret <-
    switch(
      EXPR=ret,
      "~"="\\sim",
      "%in%"=" \\in ",
      "!="=" \\neq ",
      "*"=" \\times ",
      "<="="\\leq",
      ">="="\\geq",
      "=="="\\equiv",
      latex_clean_equation_string(ret, ...)
    )
  knit_print_helper_formula_replacements(ret, ...)
}

knit_print_helper_formula.call <- function(x, ...) {
  if (!is.call(x)) {
    stop("x must be a call.")
  }
  if (length(x) == 3) {
    if (as.character(x[[1]]) %in% c("-", "+", "^", "~", ":", "*",
                                    "<", "<=", ">", ">=", "==", "!=", "|", "||", "&", "&&",
                                    "<-", "->") |
        grepl("^\\%.+\\%$", as.character(x[[1]]))) {
      ret <- sprintf("{%s}%s{%s}",
                     knit_print_helper_formula(x[[2]], ...),
                     knit_print_helper_formula(x[[1]], ...),
                     knit_print_helper_formula(x[[3]], ...))
    } else if (as.character(x[[1]]) == "/") {
      ret <- sprintf("\\frac{%s}{%s}",
                     knit_print_helper_formula(x[[2]], ...),
                     knit_print_helper_formula(x[[3]], ...))
    } else if (as.character(x[[1]]) == "%%") {
      ret <- sprintf("{%s} \\bmod {%s}",
                     knit_print_helper_formula(x[[2]], ...),
                     knit_print_helper_formula(x[[3]], ...))
    } else if (as.character(x[[1]]) %in% c("[", "[[")) {
      # if (as.character(x[[1]]) == "[[") {
      #   message("Treating [[ the same as [ in formula generation as a subscript.")
      # }
      ret <- sprintf("{%s}_{%s}",
                     knit_print_helper_formula(x[[2]], ...),
                     knit_print_helper_formula(x[[3]], ...))
    } else {
      ret <- knit_print_helper_formula.function_call(x, ...)
    }
  } else if (length(x) == 2) {
    if (as.character(x[[1]]) %in% "(") {
      ret <- sprintf("\\left(%s\\right)",
                     knit_print_helper_formula(x[[2]], ...))
    } else if (as.character(x[[1]]) %in% c("-", "~", "!")) {
      ret <- sprintf("%s%s",
                     knit_print_helper_formula(x[[1]], ...),
                     knit_print_helper_formula(x[[2]], ...))
    } else {
      # This appears to be how function calls are treated
      ret <- knit_print_helper_formula.function_call(x, ...)
    }
  } else if (length(x) == 1) {
    # A function call without an argument
    ret <- sprintf("%s()",
                   knit_print_helper_formula(x[[1]], ...))
  } else {
    # Calls with other length are (always?) function calls
    ret <- knit_print_helper_formula.function_call(x, ...)
  }
  knit_print_helper_formula_replacements(ret, ...)
}

`knit_print_helper_formula.(` <- function(x, ...) {
  # parentheses appear to be handled differently
  ret <-
    sprintf("\\left( %s \\right)",
            knit_print_helper_formula(x[[2]], ...))
  knit_print_helper_formula_replacements(ret, ...)
}

knit_print_helper_formula.character <- function(x, ...) {
  if (!is.character(x)) {
    stop("x must be a character.")
  } else if (length(x) != 1) {
    stop("Cannot handle class character in a formula unless it is a scalar.  Length is ", length(x))
  }
  ret <- sprintf("\\textrm{``%s''}", x)
  ret <- gsub("%", "\\%", ret, fixed=TRUE)
  knit_print_helper_formula_replacements(ret, ...)
}

knit_print_helper_formula.numeric <- function(x, format_numeric="%g", ...) {
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  } else if (length(x) != 1) {
    stop("Cannot handle class numeric in a formula unless it is a scalar.  Length is ", length(x))
  }
  ret <- sprintf(format_numeric, x)
  # Detect and beautify scientific notation
  if (grepl("e", ret)) {
    ret <- gsub("^([0-9]+\\.?[0-9]*)e\\+?(-?[0-9]+)$", "{\\1 \\\\times 10^{\\2}}", ret)
  }
  knit_print_helper_formula_replacements(ret, ...)
}

knit_print_helper_formula.logical <- function(x, format_numeric="%g", ...) {
  if (!is.logical(x)) {
    stop("x must be logical.")
  } else if (length(x) != 1) {
    stop("Cannot handle class logical in a formula unless it is a scalar.  Length is ", length(x))
  }
  ret <- paste0("\\textrm{", as.character(x), "}")
  knit_print_helper_formula_replacements(ret, ...)
}

knit_print_helper_formula.default <- function(x, ...) {
  stop("Cannot handle class(es): ", paste(class(x), collapse=", "))
}

knit_print_helper_formula.function_call <- function(x, ...) {
  ret <-
    if (length(x) > 1) {
      # determine the function call arguments
      funcargs <- sapply(x[seq_len(length(x)-1)+1],
                         knit_print_helper_formula,
                         ...)
      # determine the named arguments to the function call and apply them
      if (!is.null(names(x)) && !all(names(x) %in% "")) {
        printnames <- paste0(latex_clean_equation_string(names(x)[-1], ...), "=")
      } else {
        printnames <- rep("", length(x)-1)
      }
      funcargs <- paste0(printnames, funcargs)
      sprintf("%s\\left(%s\\right)",
              knit_print_helper_formula(x[[1]], ...),
              paste(funcargs, collapse=", "))
    } else {
      sprintf("%s\\left(\\right)",
              knit_print_helper_formula(x[[1]], ...))
    }
  knit_print_helper_formula_replacements(ret, ...)
}

knit_print_helper_formula_replacements <- function(x, replacements, ...) {
  if (missing(replacements)) {
    x
  } else if (x %in% names(replacements)) {
    replacements[[x]]
  } else {
    x
  }
}

latex_clean_equation_string <- function(x, ...) {
  ret <- gsub("%", "\\%", x, fixed=TRUE)
  ret <- gsub("_", "\\_", ret, fixed=TRUE)
  knit_print_helper_formula_replacements(ret, ...)
}
# 
# linebreak <- function(priority) {
#   ret <- list(priority=priority) 
#   class(ret) <- "linebreak_option"
#   ret
# }
# 
# as.character.linebreak_option <- function(x, ..., break_val="\n", nobreak_val="", priority=0) {
#   ifelse(x$priority > priority,
#          break_val,
#          nobreak_val)
# }
