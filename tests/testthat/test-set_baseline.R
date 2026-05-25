test_that("set_baseline returns single value within window", {
  expect_equal(
    set_baseline(x = c(5, 10, 15), time = c(-1, 0, 1)),
    10
  )
})

test_that("set_baseline uses value at maximum time within window", {
  expect_equal(
    set_baseline(x = c(5, 10, 15), time = c(-2, -1, 0)),
    15,
    info = "latest time wins"
  )
})

test_that("set_baseline applies summaryfun to ties at max time", {
  expect_equal(
    set_baseline(x = c(4, 6), time = c(0, 0)),
    5,
    info = "two values at time 0, mean = 5"
  )
})

test_that("set_baseline returns NA when no value falls in window", {
  result <- set_baseline(x = c(5, 10), time = c(1, 2))
  expect_true(is.na(result))
})

test_that("set_baseline masks NA in x", {
  result <- set_baseline(x = c(NA, 10), time = c(0, -1))
  expect_equal(result, 10)
})

test_that("set_baseline masks NA in time", {
  result <- set_baseline(x = c(5, 10), time = c(NA, 0))
  expect_equal(result, 10)
})

test_that("set_baseline returns NA of same class as x", {
  result <- set_baseline(x = c("A", "B"), time = c(1, 2))
  expect_equal(result, NA_character_)
})

test_that("set_baseline stopifnot: length mismatch", {
  expect_error(set_baseline(x = 1:2, time = 1:3))
})

test_that("set_baseline stopifnot: non-numeric time", {
  expect_error(set_baseline(x = 1:2, time = c("a", "b")))
})

test_that("set_baseline stopifnot: min_bl_time not scalar", {
  expect_error(set_baseline(x = 1:2, time = 1:2, min_bl_time = c(-1, 0)))
})

test_that("set_baseline stopifnot: max_bl_time not scalar", {
  expect_error(set_baseline(x = 1:2, time = 1:2, max_bl_time = c(0, 1)))
})

test_that("set_baseline stopifnot: NA min_bl_time", {
  expect_error(set_baseline(x = 1:2, time = 1:2, min_bl_time = NA_real_))
})

test_that("set_baseline stopifnot: NA max_bl_time", {
  expect_error(set_baseline(x = 1:2, time = 1:2, max_bl_time = NA_real_))
})
