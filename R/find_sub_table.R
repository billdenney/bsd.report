#' When data are provided as blocks of tables within a larger data set, 
#' extract those sub-tables as a list.
#' 
#' @param data The data to extract from
#' @param value_search The data value to search for or a function to use
#'   for the search.  If a function, it should take in the data plus the
#'   \code{...} arguments and return a two-column data frame of
#'   \code{row} and \code{column} indices to each of the values.
#' @param edge_search A named list with either functions to search for 
#'   the edge of the table or an integer indicating how far to move in 
#'   that direction to find the edge.  If a function, the function 
#'   should take in the data, a row index (integer), a column index 
#'   (integer) to start the search, a direction (one of the values in 
#'   \code{search_order}), and a list indicating \code{found_edges}. The
#'   function should return a single integer of the number of cells in 
#'   the given direction.  The names for the list elements must be 
#'   "left", "right", "up", and "down", and the order of the list 
#'   elements defines the order that they will be searched.
#' @param ... Arguments passed to user-provided search functions.
#' @return A list of data frames (or similar) with the found sub-tables.
#' @family Sub-table finding
#' @export
find_sub_table <- function(data, value_search, edge_search, ...) {
  search_fun_distance <- function(distance) {
    distance_val <- distance
    function(data, row, column, direction, found_edges, ...) {
      if (direction %in% "up") {
        if ((row - distance_val) < 1) {
          distance_val <- row - 1
          warning("distance is above the first row of the data, setting it to row 1")
        }
      } else if (direction %in% "down") {
        if ((row + distance_val) > nrow(data)) {
          distance_val <- nrow(data) - row
          warning("distance is below the last row of the data, setting it to the last row")
        }
      } else if (direction %in% "left") {
        if ((column - distance_val) < 1) {
          distance_val <- column - 1
          warning("distance is to the left of the first column of the data, setting it to column 1")
        }
      } else if (direction %in% "right") {
        if ((column + distance_val) > ncol(data)) {
          distance_val <- ncol(data) - column
          warning("distance is to the right of the last column of the data, setting it to the last column")
        }
      }
      distance_val
    }
  }
  # Check the inputs
  if (!all(c("left", "right", "up", "down") %in% names(edge_search))) {
    stop("Each direction must be specified for the search.")
  }
  # Standardize the search_fun to always be a function
  for (direction in names(edge_search)) {
    search_fun <- edge_search[[direction]]
    if (is.numeric(search_fun) & !is.factor(search_fun)) {
      if (!(as.integer(search_fun) == search_fun)) {
        warning("Rounding search integer to the nearest integer.")
        search_fun <- as.integer(round(search_fun))
      }
      search_fun <- search_fun_distance(search_fun)
    } else if (!is.function(search_fun)) {
      stop("Elements in edge_search must be integers or functions")
    }
    edge_search[[direction]] <- search_fun
  }
  if (is.function(value_search)) {
    value_location <- value_search(data, ...)
  } else {
    value_location <- value_search_default(data, value_search, ...)
  }
  ret <- list()
  for (i in seq_len(nrow(value_location))) {
    # found_edges are relative to the value_location
    found_edges <- list()
    for (direction in names(edge_search)) {
      found_edges[[direction]] <-
        edge_search[[direction]](data,
                                 value_location[i,1],
                                 value_location[i,2],
                                 direction,
                                 found_edges, ...)
    }
    # final_edges are absolute rows and columns within the data
    final_edges <- list(up=value_location[i,1] - found_edges$up,
                        down=value_location[i,1] + found_edges$down,
                        left=value_location[i,2] - found_edges$left,
                        right=value_location[i,2] + found_edges$right)
    extracted <-
      data[final_edges$up:final_edges$down,
           final_edges$left:final_edges$right,
           drop=FALSE]
    attr(extracted, "source_location") <- final_edges
    ret <- append(ret, list(extracted))
  }
  ret
}

#' Go to the edge
#' 
#' @param data,row,column,direction,found_edges See \code{\link{find_sub_table}}
#' @return A number for the distance to the edge.
#' @family Sub-table finding
#' @export
search_fun_edge <- function(data, row, column, direction, found_edges, ...) {
  if (direction %in% "up") {
    row - 1
  } else if (direction %in% "down") {
    nrow(data) - row
  } else if (direction %in% "left") {
    column - 1
  } else if (direction %in% "right") {
    ncol(data) - column
  }
}

#' Go to a value or the edge
#' 
#' @param values The value to search for
#' @param from The location to search from within the block.  It must 
#'   already be found.  Options pairs of "up", "row", or "down" and
#'   "left", "column", or "right".  Defaults to the column and row of
#'   the found value.
#' @param skip The rows and columns to skip from the \code{from} 
#'   argument when starting the search.
#' @param exclude_value Exclude the found value from the edge?  (Useful
#'   if you are searching for an NA cell at the edge and do not want
#'   that NA value in the result.)
#' @return A number for the distance to the edge.
#' @family Sub-table finding
#' @export
search_fun_values_or_edge <- function(values, from=c("row", "column"), skip=c(0, 0), exclude_value=FALSE) {
  from_lr <- c(intersect(c("left", "column", "right"), from), "column")[1]
  from_ud <- c(intersect(c("up", "row", "down"), from), "row")[1]
  function(data, row, column, direction, found_edges, ...) {
    found_edge <- search_fun_edge(data, row, column, direction, found_edges, ...)
    if (from_ud %in% "up") {
      start_row <- row - found_edges$up
    } else if (from_ud %in% "down") {
      start_row <- row + found_edges$down
    } else {
      start_row <- row
    }
    if (from_lr %in% "left") {
      start_col <- column - found_edges$left
    } else if (from_lr %in% "right") {
      start_col <- column + found_edges$right
    } else {
      start_col <- column
    }
    start_row <- start_row + skip[1]
    start_col <- start_col + skip[2]
    if (direction %in% "up") {
      ret <- which(rev(data[1:start_row,start_col]) %in% values)
    } else if (direction %in% "down") {
      ret <- which(data[start_row:nrow(data),start_col] %in% values)
    } else if (direction %in% "left") {
      ret <- which(rev(data[start_row,1:start_col]) %in% values)
    } else if (direction %in% "right") {
      ret <- which(data[start_row,start_col:ncol(data)] %in% values)
    } else {
      stop("Unrecognized direction: ", direction)
    }
    ret <- ret - 1 - as.numeric(exclude_value)
    c(ret, found_edge)[1]
  }
}

value_search_general <- function(data, value, ..., match_fun) {
  ret <- data.frame()
  for (i in seq_along(data)) {
    rows <- which(match_fun(data[[i]], value))
    if (length(rows)) {
      ret <- dplyr::bind_rows(ret, data.frame(row=rows, col=i))
    }
  }
  ret
}

value_search_default <- function(data, value, ...) {
  value_search_general(data, value=NULL, ...,
                       match_fun=function(x, ...) x %in% value)
}

value_search_regex <- function(data, value_pattern, ...) {
  value_search_general(data, value=NULL, ...,
                       match_fun=function(x, ...) {
                         grepl(value_pattern, x, ...)
                       })
}
