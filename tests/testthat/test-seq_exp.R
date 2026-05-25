test_that("seq_exp returns exponentially-spaced sorted vector", {
  result <- seq_exp(1, 100, length.out = 5)
  expect_length(result, 5L)
  expect_equal(result[1], 1)
  expect_equal(result[5], 100)
  # Values should increase by roughly constant ratio on log scale
  log_diffs <- diff(log(result))
  expect_equal(log_diffs, rep(log_diffs[1], 4), tolerance = 1e-10)
})

test_that("seq_exp from=0 adds zero to sequence", {
  result <- seq_exp(0, 100, length.out = 5)
  expect_true(0 %in% result)
  expect_equal(min(result), 0)
})

test_that("seq_exp add_values are included in result", {
  result <- seq_exp(1, 10, length.out = 3, add_values = c(5))
  expect_true(5 %in% result)
})

test_that("seq_exp errors when from < 0", {
  expect_error(seq_exp(-1, 10, length.out = 5))
})
