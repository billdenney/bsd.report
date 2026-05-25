test_that("expect_n", {
  expect_equal(expect_n(TRUE), TRUE)
  expect_error(
    expect_n("A"),
    regexp = "`x` must be a logical vector: \"A\""
  )
  expect_error(
    expect_n("A", msg = "foo"),
    regexp = "`x` must be a logical vector: foo; \"A\""
  )
  expect_error(
    expect_n(NA, msg = "foo"),
    regexp = "`x` may not be NA: foo; NA"
  )
  foo_bar <- TRUE
  expect_error(
    expect_n(foo_bar, n = 2, msg = "foo"),
    regexp = "Expected 2 TRUE values but got 1 TRUE values: foo; foo_bar"
  )
})
