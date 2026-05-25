context("logLik")

test_that("NA_logLik_", {
  expect_true(is.na(NA_logLik_))
  expect_equal(AIC(NA_logLik_), NA_real_)
})

test_that("logLik for NA and NA-like objects", {
  # logical
  expect_equal(logLik(NA), NA_logLik_)
  expect_error(
    logLik(c(NA, NA)),
    regexp="length must be 1"
  )
  expect_error(
    logLik(TRUE),
    regexp="logLik on a logical value must be NA"
  )
  
  # NULL
  expect_equal(logLik(NULL), NA_logLik_)
  
  # try-error
  test_try_error <- try({stop("foo")}, silent=TRUE)
  expect_equal(logLik(test_try_error), NA_logLik_)
})
