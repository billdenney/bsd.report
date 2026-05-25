test_that("translate_value.character replaces old with new", {
  expect_equal(translate_value("old", "old", "new"), "new")
  expect_equal(
    translate_value(c("old", "keep"), "old", "new"),
    c("new", "keep")
  )
})

test_that("translate_value.factor renames the level", {
  result <- translate_value(factor("old"), "old", "new")
  expect_equal(levels(result), "new")
  expect_equal(as.character(result), "new")
})

test_that("translate_value.data.frame updates all matching columns", {
  d <- data.frame(A = "old", B = "old", stringsAsFactors = FALSE)
  result <- translate_value(d, "old", "new")
  expect_equal(result$A, "new")
  expect_equal(result$B, "new")
})

test_that("translate_value.data.frame respects exclude_col", {
  d <- data.frame(A = "old", B = "old", stringsAsFactors = FALSE)
  result <- translate_value(d, "old", "new", exclude_col = "B")
  expect_equal(result$A, "new")
  expect_equal(result$B, "old")
})

test_that("translate_value.default returns x unchanged", {
  expect_equal(translate_value(42L, "old", "new"), 42L)
  expect_equal(translate_value(TRUE, "old", "new"), TRUE)
})
