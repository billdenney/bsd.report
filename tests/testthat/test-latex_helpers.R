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

test_that("latex_label_first_last", {
  expect_equal(
    latex_label_first_last(1:3, text="fig:test"),
    c("\\label{fig:test-first}", "", "\\label{fig:test-last}")
  )
  expect_equal(
    latex_label_first_last(1:2, text="fig:test"),
    c("\\label{fig:test-first}", "\\label{fig:test-last}")
  )
})

test_that("latex_label_first_last errors", {
  expect_error(
    latex_label_first_last(1, text="fig:test"),
    regexp="object must be >1 long",
    fixed=TRUE
  )
  expect_error(
    latex_label_first_last(1:2, text=c("A", "fig:test")),
    regexp="text must be a scalar",
    fixed=TRUE
  )
  expect_error(
    latex_label_first_last(1:2, text="fig:test", sep=c("A", "B")),
    regexp="sep must be a scalar",
    fixed=TRUE
  )
  expect_error(
    latex_label_first_last(1:2, text="fig:test", suffix_text="foo"),
    regexp="suffix_text must be 2 long",
    fixed=TRUE
  )
})
