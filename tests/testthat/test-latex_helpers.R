context("latex helpers")

test_that("latex_label_clean", {
  expect_equal(latex_label_clean("A"), "A")
  expect_equal(latex_label_clean("A B"), "A-B")
  expect_equal(latex_label_clean("A.B"), "A.B")
  expect_equal(latex_label_clean("A_B"), "A-B")
})
