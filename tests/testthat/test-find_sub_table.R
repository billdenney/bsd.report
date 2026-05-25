context("find_sub_table")

sub_table_example <-
  tibble::tribble(
    ~a, ~b, ~c,
    NA, NA, NA,
    "A123", "B", "C",
    "1", "2", "3",
    "4", "5", "6",
    NA, NA, NA,
    "A321", "B", "C",
    "4", "5", "6",
    "1", "2", "3",
    NA, NA, NA
  )

test_that("find_sub_table standard inputs", {
  expect_equivalent(
    find_sub_table(
      data=sub_table_example,
      value_search="B",
      edge_search=
        list(
          left=search_fun_edge,
          right=search_fun_edge,
          up=search_fun_values_or_edge(values=NA, exclude_value=TRUE),
          down=search_fun_values_or_edge(values=NA, exclude_value=TRUE)
        )
    ),
    list(
      tibble::tribble(
        ~a, ~b, ~c,
        "A123", "B", "C",
        "1", "2", "3",
        "4", "5", "6",
      ),
      tibble::tribble(
        ~a, ~b, ~c,
        "A321", "B", "C",
        "4", "5", "6",
        "1", "2", "3"
      )
    )
  )
})

test_that("value_search_general works", {
  expect_equal(
    value_search_general(
      data=sub_table_example,
      value="B",
      match_fun=function(x, value) x %in% value
    ),
    data.frame(row=c(2, 6), col=2)
  )
  expect_equal(
    value_search_general(
      data=sub_table_example,
      value="D",
      match_fun=function(x, value) x %in% value
    ),
    data.frame()
  )
})

test_that("value_search_default works", {
  expect_equal(
    value_search_default(data=sub_table_example, value="B"),
    data.frame(row=c(2, 6), col=2)
  )
  expect_equal(
    value_search_default(data=sub_table_example, value=c("A123", "A321")),
    data.frame(row=c(2, 6), col=1)
  )
})

test_that("value_search_regex works", {
  expect_equal(
    value_search_regex(data=sub_table_example, value_pattern="^A"),
    data.frame(row=c(2, 6), col=1)
  )
})

test_that("find_sub_table errors when edge_search is missing a direction", {
  expect_error(
    find_sub_table(sub_table_example, "B",
      edge_search=list(left=search_fun_edge, right=search_fun_edge)),
    regexp="Each direction must be specified",
    fixed=TRUE
  )
})

test_that("find_sub_table errors when edge_search element is not integer or function", {
  expect_error(
    find_sub_table(sub_table_example, "B",
      edge_search=list(left="bad", right=search_fun_edge, up=search_fun_edge, down=search_fun_edge)),
    regexp="Elements in edge_search must be integers or functions",
    fixed=TRUE
  )
})

test_that("find_sub_table warns and rounds non-integer numeric edge", {
  expect_warning(
    find_sub_table(sub_table_example, "B",
      edge_search=list(left=1.5, right=search_fun_edge, up=search_fun_edge, down=search_fun_edge)),
    regexp="Rounding search integer",
    fixed=TRUE
  )
})

test_that("find_sub_table warns when integer distances exceed data boundaries", {
  expect_warning(
    find_sub_table(sub_table_example, "B",
      edge_search=list(left=0L, right=0L, up=2L, down=0L)),
    regexp="distance is above the first row"
  )
  expect_warning(
    find_sub_table(sub_table_example, "B",
      edge_search=list(left=0L, right=0L, up=0L, down=4L)),
    regexp="distance is below the last row"
  )
  expect_warning(
    find_sub_table(sub_table_example, "B",
      edge_search=list(left=2L, right=0L, up=0L, down=0L)),
    regexp="distance is to the left of the first column"
  )
  expect_warning(
    find_sub_table(sub_table_example, "B",
      edge_search=list(left=0L, right=2L, up=0L, down=0L)),
    regexp="distance is to the right of the last column"
  )
})

test_that("find_sub_table accepts function value_search returning row/col data.frame", {
  result <- find_sub_table(
    sub_table_example,
    value_search=function(data, ...) data.frame(row=2L, col=2L),
    edge_search=list(left=0L, right=0L, up=0L, down=0L)
  )
  expect_length(result, 1)
  expect_equal(result[[1]][[1, 1]], "B")
})

test_that("find_sub_table errors when value_search function returns wrong column names", {
  expect_error(
    find_sub_table(
      sub_table_example,
      value_search=function(data, ...) data.frame(rows=2L, cols=2L),
      edge_search=list(left=0L, right=0L, up=0L, down=0L)
    ),
    regexp="value_search() must return a data.frame with column names 'row' and 'col'",
    fixed=TRUE
  )
})

test_that("search_fun_values_or_edge handles all from= variants", {
  data <- sub_table_example
  found_edges_stub <- list(up=1L, down=1L, left=1L, right=1L)
  row <- 5L; col <- 2L

  f_up <- search_fun_values_or_edge(values=NA, from="up")
  expect_true(is.numeric(f_up(data=data, row=row, column=col, direction="down", found_edges=found_edges_stub)))

  f_down <- search_fun_values_or_edge(values=NA, from="down")
  expect_true(is.numeric(f_down(data=data, row=row, column=col, direction="up", found_edges=found_edges_stub)))

  f_left <- search_fun_values_or_edge(values=NA, from="left")
  expect_true(is.numeric(f_left(data=data, row=row, column=col, direction="right", found_edges=found_edges_stub)))

  f_right <- search_fun_values_or_edge(values=NA, from="right")
  expect_true(is.numeric(f_right(data=data, row=row, column=col, direction="left", found_edges=found_edges_stub)))
})

test_that("value_search_regex passes ... to grepl", {
  expect_equal(
    value_search_regex(data=sub_table_example, value_pattern="^a", ignore.case=TRUE),
    data.frame(row=c(2, 6), col=1)
  )
})
