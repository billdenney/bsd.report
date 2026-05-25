test_that("as.data.frame.power.htest converts two-sample test", {
  pt <- power.t.test(n = 10, delta = 1, sd = 1)
  result <- as.data.frame(pt)
  expect_true(is.data.frame(result))
  expect_equal(names(result), c("Parameter", "Value"))
  # alternative recoding
  expect_true("Two-sided" %in% result$Value)
  # numeric formatting: n=10 should appear as "10"
  expect_equal(result$Value[result$Parameter == "N"], "10")
})

test_that("as.data.frame.power.htest NULL note becomes (none)", {
  # one-sample test has a NULL note
  pt <- power.t.test(n = 10, delta = 1, sd = 1, type = "one.sample")
  result <- as.data.frame(pt)
  expect_true("(none)" %in% result$Value)
})

test_that("as.data.frame.power.htest respects digits argument", {
  pt <- power.t.test(n = 10, delta = 1, sd = 1)
  result <- as.data.frame(pt, digits = 2)
  # n should still be exact ("10"), power should have 2 sig figs
  power_val <- as.numeric(result$Value[result$Parameter == "Power"])
  expect_equal(signif(power_val, 2), power_val)
})
