test_that("realize_addl_single() errors with removal message", {
  expect_error(realize_addl_single(), regexp = "removed", fixed = TRUE)
  expect_error(realize_addl_single(), regexp = "mrgsolve::realize_addl", fixed = TRUE)
})
