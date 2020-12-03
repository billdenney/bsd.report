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
