#' Create a string of the filename with its hash as an attribute to allow change detection in drake
#' 
#' @param filename The filename or vector of filenames to generate a hash for
#' @return `filename` with an attribute of `hash` added.
#' @export
#' @examples
#' \dontrun{
#' my_plan <-
#'   drake_plan(
#'     big_filename=
#'       target(
#'         command=add_hash_to_file("a/file.xpt"),
#'         trigger=trigger(condition=TRUE)
#'       )
#'   )
#' }
#' @importFrom digest digest
add_hash_to_file <- function(filename) {
  structure(
    filename,
    hash=sapply(X=filename, FUN=digest::digest, file=TRUE)
  )
}
