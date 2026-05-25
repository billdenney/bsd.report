test_that("patch_data() errors with removal message", {
  expect_error(patch_data(), regexp = "removed", fixed = TRUE)
  expect_error(patch_data(), regexp = "dplyr::rows_patch", fixed = TRUE)
})
