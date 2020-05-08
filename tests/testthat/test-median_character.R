context("median_character")

test_that("median_character", {
  expect_equal(
    median_character(character()),
    NA_character_
  )
  expect_equal(
    median_character("A"),
    "A"
  )
  expect_equal(
    median_character(LETTERS[1:2]),
    "A"
  )
  expect_equal(
    median_character(LETTERS[1:2], tie="upper"),
    "B"
  )
  expect_equal(
    median_character(LETTERS[1:3]),
    "B"
  )
  expect_equal(
    median_character(LETTERS[1:3], tie="upper"),
    "B"
  )
  expect_equal(
    median_character(c(NA, LETTERS[1:3]), na.rm=FALSE),
    NA_character_
  )
})
