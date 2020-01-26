#' Create a string of the filename with its hash as an attribute to allow change detection in drake
#' 
#' @param filename The file to generate a hash for
#' @return The filename with an attribute of `hash` added.
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
add_hash_to_file <- function(filename) {
  structure(
    filename,
    hash=digest::digest(filename, file=TRUE)
  )
}
