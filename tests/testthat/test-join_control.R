context("join_control")

test_that("join_control works as expected", {
  empty_df <- data.frame(A=1)[-1, , drop=FALSE]
  one_row_df <- data.frame(A=1, B=2)
  two_row_df <- data.frame(A=1:2, C=3)
  
  expect_equal(
    join_control(
      empty_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control="any", y_control="any"
    ),
    dplyr::left_join(empty_df, one_row_df)
  )
  expect_equal(
    join_control(
      one_row_df, two_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control="unique"
    ),
    dplyr::left_join(one_row_df, two_row_df)
  )
  expect_equal(
    join_control(
      two_row_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control="unique"
    ),
    dplyr::left_join(two_row_df, one_row_df)
  )
  expect_error(
    join_control(
      two_row_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control=c("unique", "nomissing")
    ),
    regexp="Rows are missing in the new dataset"
  )
  expect_equal(
    join_control(
      two_row_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control=c("all", "unique")
    ),
    dplyr::left_join(two_row_df, one_row_df)
  )
  expect_equal(
    join_control(
      two_row_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control=c("any", "nomissing")
    ),
    dplyr::left_join(two_row_df, one_row_df)
  )
  expect_equal(
    join_control(
      two_row_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control=c("unique", "missing")
    ),
    dplyr::left_join(two_row_df, one_row_df)
  )
})

test_that("join_control algorithm errors", {
  x <- data.frame()
  y <- data.frame()
  
  expect_error(
    join_control(x, y, join_fun=dplyr::left_join, x_control="foo", y_control="any"),
    regexp='should be one of .any., .all., .unique.'
  )
  expect_error(
    join_control(x, y, join_fun=dplyr::left_join, x_control="any", y_control="foo"),
    regexp='should be one of .any., .all., .unique.'
  )
})

test_that("join_many_to_one works", {
  x <- data.frame(A=rep(1:2, 2), B=1:4)
  y_nomissing <- data.frame(A=1:2, C=1:2)
  y_missing <- data.frame(A=1, C=1)
  expect_equal(
    join_many_to_one(x, y_nomissing),
    data.frame(A=rep(1:2, 2), B=1:4, C=rep(1:2, 2))
  )
  expect_error(
    join_many_to_one(x, y_missing),
    regexp="Rows are missing in the new dataset"
  )
})
