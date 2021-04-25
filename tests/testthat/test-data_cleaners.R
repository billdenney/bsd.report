context("data_cleaners")

test_that("single_value", {
  expect_equal(single_value(1), 1)
  expect_equal(single_value(c(1, NA)), 1)
  expect_equal(single_value(c(1, NA, 1)), 1)
  expect_equal(single_value(NA_real_), NA)
  expect_equal(single_value(NA, missing=NA_real_), NA_real_)
  expect_error(
    single_value(1:2),
    regexp="More than one (2) value found (1, 2)",
    fixed=TRUE
  )
  expect_error(
    single_value(1:2, info="multiple"),
    regexp="More than one (2) value found (1, 2): multiple",
    fixed=TRUE
  )
})

test_that("setdiff_bidir", {
  expect_equal(
    setdiff_bidir(1:2, 2),
    c(x=1)
  )
  expect_equal(
    setdiff_bidir(1:3, 2),
    c(x1=1, x2=3)
  )
  expect_equal(
    setdiff_bidir(1:3, 2:5),
    c(x=1, y1=4, y2=5)
  )
})

test_that("duplicated_including_first", {
  expect_equal(
    duplicated_including_first(c(1, 1, 1)),
    rep(TRUE, 3)
  )
})
