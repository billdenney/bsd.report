context("latex helpers")

test_that("latex_label_clean", {
  expect_equal(latex_label_clean("A"), "A")
  expect_equal(latex_label_clean("A B"), "A-B")
  expect_equal(latex_label_clean("A.B"), "A-B")
  expect_equal(latex_label_clean("A_B"), "A-B")
  expect_equal(latex_label_clean("A(B)"), "A-B-")
  # Ensure that normal 'fig:' and 'tab:' are not changed
  expect_equal(latex_label_clean("fig:A(B)"), "fig:A-B-")
  expect_equal(latex_label_clean("tab:A(B)"), "tab:A-B-")
})
