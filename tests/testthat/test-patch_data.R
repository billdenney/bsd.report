test_that("patch_data replaces NA values", {
  base <- data.frame(id = 1:2, val = c(NA_real_, 2), stringsAsFactors = FALSE)
  patch <- data.frame(id = 1L, val = 10, stringsAsFactors = FALSE)
  expect_message(
    result <- patch_data(base, patch, by = "id"),
    regexp = "Replaced 1 values in column val",
    fixed = TRUE
  )
  expect_equal(result$val, c(10, 2))
})

test_that("patch_data verbose=FALSE suppresses messages", {
  base <- data.frame(id = 1:2, val = c(NA_real_, 2), stringsAsFactors = FALSE)
  patch <- data.frame(id = 1L, val = 10, stringsAsFactors = FALSE)
  expect_no_message(patch_data(base, patch, by = "id", verbose = FALSE))
})

test_that("patch_data replace=NULL replaces all values", {
  base <- data.frame(id = 1:2, val = c(5, 2), stringsAsFactors = FALSE)
  patch <- data.frame(id = 1L, val = 10, stringsAsFactors = FALSE)
  result <- patch_data(base, patch, by = "id", replace = NULL, verbose = FALSE)
  expect_equal(result$val, c(10, 2))
})

test_that("patch_data do_not_replace=NULL uses all patch values", {
  base <- data.frame(id = 1:2, val = c(NA_real_, NA_real_), stringsAsFactors = FALSE)
  patch <- data.frame(id = 1:2, val = c(10, 20), stringsAsFactors = FALSE)
  result <- patch_data(base, patch, by = "id", do_not_replace = NULL, verbose = FALSE)
  expect_equal(result$val, c(10, 20))
})

test_that("patch_data errors when by is empty", {
  base <- data.frame(id = 1, val = NA_real_, stringsAsFactors = FALSE)
  patch <- data.frame(id = 1, val = 10, stringsAsFactors = FALSE)
  expect_error(
    patch_data(base, patch, by = character(0)),
    regexp = "`by` must be provided with at least one column.",
    fixed = TRUE
  )
})

test_that("patch_data errors when by column missing from patchdata", {
  base <- data.frame(id = 1, val = NA_real_, stringsAsFactors = FALSE)
  patch <- data.frame(other = 1, val = 10, stringsAsFactors = FALSE)
  expect_error(
    patch_data(base, patch, by = "id"),
    regexp = "All names in `by` must be present as columns of `patchdata`.",
    fixed = TRUE
  )
})

test_that("patch_data errors when patchdata has duplicate keys", {
  base <- data.frame(id = 1:3, val = NA_real_, stringsAsFactors = FALSE)
  patch <- data.frame(id = c(1, 1), val = c(10, 20), stringsAsFactors = FALSE)
  expect_error(
    patch_data(base, patch, by = "id"),
    regexp = "`patchdata` must have 0 or 1 row for each group in basedata",
    fixed = TRUE
  )
})

test_that("patch_data warns about new columns in patchdata", {
  base <- data.frame(id = 1, val = NA_real_, stringsAsFactors = FALSE)
  patch <- data.frame(id = 1, val = 10, extra = "x", stringsAsFactors = FALSE)
  expect_warning(
    patch_data(base, patch, by = "id", verbose = FALSE),
    regexp = "new columns will be added",
    fixed = FALSE
  )
})
