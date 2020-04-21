context("replace_synonym")

test_that("replace_synonym works", {
  expect_equal(
    replace_synonym(
      c("A", "B", "C", "a"),
      c(A="apple", B="bear", C="cabbage")
    ),
    c("apple", "bear", "cabbage", "apple")
  )
  expect_equal(
    replace_synonym(
      c("A", "B", "C", "a"),
      c(A="apple", B="bear", C="cabbage"),
      ignore_case=FALSE
    ),
    c("apple", "bear", "cabbage", "a")
  )
  expect_equal(
    replace_synonym(
      c("A", "B", "C", "D"),
      c(A="apple", B="bear", C="cabbage"),
      ignore_case=FALSE
    ),
    c("apple", "bear", "cabbage", "D"),
    info="All returned values are not accidentally lower case."
  )
})

test_that("replace_synonym errors appropriately", {
  expect_error(
    replace_synonym(1, 1),
    regexp="`x` must be a character vector",
    fixed=TRUE
  )
  expect_error(
    replace_synonym("A", 1),
    regexp="`synonyms` must be a character vector.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym("A", "A"),
    regexp="`synonyms` must be named.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym("A", c(a="A", "B")),
    regexp="All `synonyms` must be named, and no names may be blank.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym("A", c(a="A", a="B")),
    regexp="All names of `synonyms` (verbatim values) must be unique.",
    fixed=TRUE
  )
})
