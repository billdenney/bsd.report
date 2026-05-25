test_that("filter_maybe normal operation", {
  expect_equal(
    nrow(filter_maybe(mtcars, cyl == 6)),
    7L
  )
})

test_that("filter_maybe returns NULL on error", {
  expect_null(filter_maybe(mtcars, nonexistent_col == 6))
})

test_that("filter_list applies filter_maybe to each element", {
  result <- filter_list(list(mtcars, iris), cyl == 6)
  expect_length(result, 2L)
  expect_equal(nrow(result[[1]]), 7L)
  # iris has no cyl column → error → NULL
  expect_null(result[[2]])
})

test_that("filter_list with single-element list", {
  result <- filter_list(list(mtcars), cyl == 6)
  expect_length(result, 1L)
  expect_equal(nrow(result[[1]]), 7L)
})
