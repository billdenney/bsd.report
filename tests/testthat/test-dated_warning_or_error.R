test_that("dated_warning_or_error gives warning for future date", {
  expect_warning(
    dated_warning_or_error("3000-01-01", "too early"),
    regexp = "too early",
    fixed = TRUE
  )
})

test_that("dated_warning_or_error gives error for past date", {
  expect_error(
    dated_warning_or_error("1900-01-01", "too late"),
    regexp = "too late",
    fixed = TRUE
  )
})
