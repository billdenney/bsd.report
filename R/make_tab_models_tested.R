#' Create a tibble of tested models
#'
#' @param models A named list of fitted model objects (that can be run through
#'   the `AIC()` function)
#' @param caption The caption attribute to add to the tibble (enabling
#'   `pander::pander()` to automatically add the correct caption)
#' @return A tibble with a caption attribute.  The tibble will have columns of
#'   "Description", "AIC", and "dAIC".
#' @export
make_tab_models_tested <- function(models, caption) {
  ret <-
    models |>
    PKNCA:::AIC.list() |>
    dplyr::select(-df, -indentation, -isBest) |>
    tibble::rownames_to_column("Description") |>
    dplyr::mutate(dAIC = AIC - min(AIC, na.rm = TRUE))
  attr(ret, "caption") <- caption
  ret
}
