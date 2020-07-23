context("fun_no_na")

test_that("fun_no_na", {
  expect_equal(max_no_na(c(1, 2)), 2)
  expect_equal(max_no_na(c(1, 2, NA)), 2)
  expect_equal(max_no_na(NA_real_), NA_real_)
  expect_equal(
    max_no_na(rep(NA_real_, 3)),
    NA_real_,
    info="Scalar is returned, not full vector"
  )
  expect_equal(max_no_na(NA_character_), NA_character_)
  expect_equal(max_no_na(double()), NA_real_)
  expect_equal(max_no_na(double(), zero_len=NA), NA_real_)
  expect_equal(max_no_na(double(), zero_len=NULL), double())
  expect_equal(max_no_na(character(), zero_len=NA), NA_character_)
  expect_error(
    max_no_na(character(), zero_len="A"),
    regexp="`zero_len` must be `NULL` or `NA`.",
    fixed=TRUE
  )

  expect_equal(min_no_na(c(1, 2, NA)), 1)
  expect_equal(min_no_na(NA), NA)
  
  expect_equal(median_no_na(c(1, 2, 4, NA)), 2)
  expect_equal(mean_no_na(c(1, 2, 4, NA)), 7/3)
  expect_equal(sd_no_na(c(1, 2, 4, NA)), sd(c(1, 2, 4)))
})
