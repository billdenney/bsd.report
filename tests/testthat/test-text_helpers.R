context("text helpers")

test_that("comma_and", {
  expect_equal(comma_and("A"), "A")
  expect_equal(comma_and("A", conjunction="foo"), "A")
  expect_equal(comma_and(c("A", "B")), "A and B")
  expect_equal(comma_and(c("A", "B"), conjunction="to"), "A to B")
  expect_equal(comma_and(c("A", "B", "C")), "A, B, and C")
  expect_equal(comma_and(c("A", "B", "C"), oxford_comma=FALSE), "A, B and C")
  expect_equal(comma_and(c("A", "B", "C"), oxford_comma=FALSE, conjunction="foo"), "A, B foo C")
  expect_equal(expect_warning(comma_and(c())), "")
})

test_that("make_ci", {
  expect_equal(make_ci(NA, NA), NA_character_, check.attributes=FALSE)
  expect_equal(make_ci(1, NA), "1 [NA]", check.attributes=FALSE)
  expect_equal(make_ci(1, 1), "1 [-0.96, 2.96]", check.attributes=FALSE)
  expect_equal(
    make_ci(1, 1, level=0.9),
    "1 [-0.645, 2.64]",
    check.attributes=FALSE
  )
  expect_equal(
    make_ci(c(1, NA), c(1, 1), level=0.9),
    c("1 [-0.645, 2.64]", NA_character_),
    check.attributes=FALSE
  )
  expect_equal(
    make_ci(c(1, NA), c(1, 1), level=0.9, transform=exp),
    c("2.72 [0.525, 14.1]", NA_character_),
    check.attributes=FALSE
  )
  expect_equal(
    make_ci(c(1, 1), c(1, NA), level=0.9),
    c("1 [-0.645, 2.64]", "1 [NA]"),
    check.attributes=FALSE
  )
  expect_equal(
    make_ci(c(1, 1), c(1, NA), level=0.9, format_numeric="%0.4g"),
    c("1 [-0.6449, 2.645]", "1 [NA]"),
    check.attributes=FALSE
  )
})
