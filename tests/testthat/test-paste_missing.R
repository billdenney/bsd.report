context("paste_missing")

test_that("standard missing paste", {
  expect_equal(
    paste_missing(), character(0),
    info="zero arguments matches paste()"
  )
  expect_equal(
    paste_missing(c("A", "B", NA)),
    c("A", "B", NA),
    info="one argument is returned as-is"
  )
  expect_equal(
    paste_missing(c("A", "B", NA), collapse=";"),
    "A;B;NA",
    info="one argument is returned as-is, collapsing"
  )
  expect_equal(
    paste_missing(c("A", "B", NA), c(NA, "C", "D")),
    c("A", "B C", "D")
  )
  expect_equal(
    paste_missing(c("A", "B", NA), c(NA, "C", "D"), sep="E"),
    c("A", "BEC", "D")
  )
  expect_equal(
    paste_missing(c("A", "B", NA), c(NA, "C", "D"), sep="E", collapse=";"),
    c("A;BEC;D")
  )
  expect_equal(
    paste_missing(
      c("A", "B", NA, NA),
      c(NA, "C", "D", NA)
    ),
    c("A", "B C", "D", NA)
  )
  expect_equal(
    paste_missing(
      c("A", "B", NA, NA, NA),
      c(NA, "C", "D", NA, NA),
      c(NA, NA, "E", "F", NA)
    ),
    c("A", "B C", "D E", "F", NA)
  )
  expect_equal(
    paste_missing(
      c("A", "B", NA, NA, NA),
      c(NA, "C", "D", NA, NA),
      c(NA, NA, "E", "F", NA),
      missing_values=c(NA, "F")
    ),
    c("A", "B C", "D E", NA, NA)
  )
  expect_equal(
    paste_missing(c("A", NA), "B"),
    c("A B", "B")
  )
  expect_equal(
    paste_missing("B", c("A", NA)),
    c("B A", "B")
  )
})
