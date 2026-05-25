test_that("emax_fun returns correct values", {
  expect_equal(emax_fun(0, e0 = 0, emax = 1, ex50 = 1), 0)
  expect_equal(emax_fun(1, e0 = 0, emax = 1, ex50 = 1), 0.5)
  # emax_fun(Inf) = Inf/Inf = NaN in R; use a large but finite x to test limit
  expect_equal(emax_fun(1e10, e0 = 0, emax = 1, ex50 = 1), 1, tolerance = 1e-8)
  # e0 offset
  expect_equal(emax_fun(0, e0 = 2, emax = 1, ex50 = 1), 2)
  # hill != 1
  expect_equal(emax_fun(1, e0 = 0, emax = 1, ex50 = 1, hill = 2), 0.5)
})

test_that("inverse_emax is the inverse of emax_fun", {
  for (x in c(0.1, 0.5, 1, 2, 10)) {
    effect <- emax_fun(x, e0 = 0, emax = 2, ex50 = 1, hill = 1)
    expect_equal(
      inverse_emax(effect, e0 = 0, emax = 2, ex50 = 1, hill = 1),
      x,
      tolerance = 1e-10
    )
  }
})

test_that("cumsum_reset resets at specified value", {
  expect_equal(
    cumsum_reset(c(1, 2, NA, 3), reset = NA),
    c(1, 3, 0, 3)
  )
  expect_equal(
    cumsum_reset(c(1, 2, 0, 3), reset = 0),
    c(1, 3, 0, 3)
  )
})

test_that("cumsum_reset with no reset value just cumulates", {
  # reset=NA and none of the values are NA → no reset occurs
  expect_equal(
    cumsum_reset(c(1, 2, 3)),
    c(1, 3, 6)
  )
})
