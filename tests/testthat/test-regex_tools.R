test_that("grepl_multi_pattern matches any of the patterns", {
  expect_equal(
    grepl_multi_pattern(c("A", "B"), c("A", "B", "C", "D")),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("grepl_multi_pattern with ignore.case", {
  expect_equal(
    grepl_multi_pattern(c("a", "b"), c("A", "B", "C"), ignore.case = TRUE),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("gsub_multi_pattern replaces first matching pattern only", {
  # "hello" matches "hello" first; "world" is not re-processed
  expect_equal(
    gsub_multi_pattern(c("hello", "world"), c("hello", "world"), "X"),
    c("X", "X")
  )
  # Already-matched elements are not re-processed by later patterns
  expect_equal(
    gsub_multi_pattern("hello", c("hello", "hell"), "X"),
    "X"
  )
})

test_that("gsub_multi_pattern returns NA for unmatched values", {
  expect_equal(
    gsub_multi_pattern(c("hello", "other"), "hello", "X"),
    c("X", NA_character_)
  )
})

test_that("gsub_multi_pattern verbose mode emits messages", {
  expect_message(
    gsub_multi_pattern(c("hello", "world", "other"), c("hello", "world"), "X", verbose = TRUE),
    regexp = "1 values matched the following pattern: hello",
    fixed = TRUE
  )
  expect_message(
    gsub_multi_pattern(c("hello", "other"), "hello", "X", verbose = TRUE),
    regexp = "1 values matched no pattern.",
    fixed = TRUE
  )
})

test_that("number_patterns correctly match expected strings", {
  expect_true(grepl(paste0("^", number_patterns$natural, "$"), "123"))
  expect_false(grepl(paste0("^", number_patterns$natural, "$"), "0"))
  expect_true(grepl(paste0("^", number_patterns$integer, "$"), "-5"))
  expect_true(grepl(paste0("^", number_patterns$integer, "$"), "+5"))
  expect_true(grepl(paste0("^", number_patterns$scientific_notation, "$"), "1.5e+10"))
  expect_false(grepl(paste0("^", number_patterns$scientific_notation, "$"), "1.5e.10"))
  expect_true(grepl(paste0("^", number_patterns$number_relaxed, "$"), "1.5"))
  expect_true(grepl(paste0("^", number_patterns$number_relaxed, "$"), "1.5e10"))
  expect_true(grepl(paste0("^", number_patterns$scientific_notation_relaxed, "$"), "1e5"))
  expect_true(grepl(paste0("^", number_patterns$scientific_notation_relaxed, "$"), "1e-5"))
  expect_false(grepl(paste0("^", number_patterns$scientific_notation_relaxed, "$"), "1e.5"))
})
