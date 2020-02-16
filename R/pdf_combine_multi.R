#' Combine multiple PDF files including optional selection of pages.
#' @inheritParams qpdf::pdf_combine
#' @importFrom qpdf pdf_combine pdf_subset
pdf_combine_multi <- function(input, pages, output=NULL, password="") {
  stopifnot(is.character(input))
  if (!length(output)) {
    output <- sub("\\.pdf$", "_combined.pdf", input[1])
  }
  output <- normalizePath(output, mustWork = FALSE)
  if (missing(pages)) {
    # If pages is not supplied, combine all pages for all inputs
    combine_input <- input
  } else {
    stopifnot(length(input) == length(pages))
    stopifnot(is.list(pages))
    combine_inputs <- character(length=length(input))
    for (input_idx in seq_along(input)) {
      combine_inputs[[input_idx]] <-
        qpdf::pdf_subset(
          input=input[[input_idx]],
          pages=pages[[input_idx]],
          output=tempfile(fileext=".pdf")
        )
    }
  }
  ret <- qpdf:::cpp_pdf_combine(infiles=combine_inputs, outfile=output, password=password)
  if (!missing(pages)) {
    # clean up temporary files
    file.remove(combine_inputs)
  }
  ret
}
