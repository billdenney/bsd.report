#' Expand II and ADDL with NONMEM-like methods for a single subject
#' 
#' @param time Time of assessment
#' @param evid NONMEM-style event identifier; 1 is a dose; 4 is a reset and dose; all other rows are ignored.
#' @param addl The number of additional doses (0 indicates one dose will be administered)
#' @param ii The interdose-interval (time between doses)
#' @return A data.frame with rows expanded based on addl and ii.  The final
#'   number of rows will be \code{sum(evid %in% c(1, 4)) + sum(addl[evid %in%
#'   c(1, 4)])}
#' @export
realize_addl_single <- function(time, evid, addl=0, ii=0) {
  stopifnot("evid must not be NA"=!any(is.na(evid)))
  stopifnot(is.numeric(time))
  stopifnot(is.numeric(evid))
  stopifnot(is.numeric(addl))
  stopifnot(is.numeric(ii))
  inputs <- data.frame(time, evid, addl, ii)
  # Only keep doses
  doses <- inputs[inputs$evid %in% c(1, 4), ]
  stopifnot("addl must not be non-negative"=all(doses$addl >= 0))
  stopifnot("ii must not be non-negative"=all(doses$ii >= 0))
  stopifnot("addl must be an integer"=all(as.integer(doses$addl) == doses$addl))
  stopifnot("time on dosing rows must not be NA"=!any(is.na(doses$time)))
  stopifnot("addl on dosing rows must not be NA"=!any(is.na(doses$addl)))
  stopifnot("ii on dosing rows must not be NA"=!any(is.na(doses$ii)))
  ret_list <- list()
  if (nrow(doses) == 0) {
    ret_list <- doses
  } else {
    for (idx in seq_len(nrow(doses))) {
      ret_tmp <-
        data.frame(
          time=doses$time[idx] + doses$ii[idx]*seq(from=0, to=doses$addl[idx]),
          # If the first evid is 4, keep that and then the subsequent evid are 1
          # to prevent resetting.
          evid=c(doses$evid[idx], rep(1, doses$addl[idx])),
          addl=0,
          ii=0
        )
      ret_list[[idx]] <- ret_tmp
    }
  }
  dplyr::bind_rows(ret_list)
}
