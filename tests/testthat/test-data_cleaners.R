context("data_cleaners")

test_that("single_value", {
  expect_equal(single_value(1), 1)
  expect_equal(single_value(c(1, NA)), 1)
  expect_equal(single_value(c(1, NA, 1)), 1)
  expect_equal(single_value(NA_real_), NA)
  expect_equal(single_value(NA, missing=NA_real_), NA_real_)
  expect_error(
    single_value(1:2),
    regexp="More than one (2) value found (1, 2)",
    fixed=TRUE
  )
  expect_error(
    single_value(1:2, info="multiple"),
    regexp="More than one (2) value found (1, 2): multiple",
    fixed=TRUE
  )
})

test_that("setdiff_bidir", {
  expect_equal(
    setdiff_bidir(1:2, 2),
    c(x=1)
  )
  expect_equal(
    setdiff_bidir(1:3, 2),
    c(x1=1, x2=3)
  )
  expect_equal(
    setdiff_bidir(1:3, 2:5),
    c(x=1, y1=4, y2=5)
  )
})

test_that("interesting_cols drops boring columns and tracks repeats", {
  df <- data.frame(A=1:2, B=2, stringsAsFactors=FALSE)
  result <- interesting_cols(df)
  expect_equal(names(result), "A")
  expect_equal(attr(result, "boring")[["B"]], 2)

  result_keep <- interesting_cols(df, keep_anyway="B")
  expect_true("B" %in% names(result_keep))

  df2 <- data.frame(A=1:2, B=1:2, stringsAsFactors=FALSE)
  result2 <- interesting_cols(df2)
  expect_equal(names(result2), "A")
  expect_true("A" %in% names(attr(result2, "repeats")))

  df0 <- data.frame(A=integer(), B=integer())
  result0 <- interesting_cols(df0)
  expect_equal(nrow(attr(result0, "boring")), 0)
})

test_that("make_loq extracts numeric, llq, ulq, and text columns", {
  result <- make_loq(c("1", "A", "<1", ">60"))
  expect_equal(nrow(result), 4)
  expect_equal(names(result), c("text", "number", "llq", "ulq"))

  expect_error(make_loq(1), regexp="x must be a character vector", fixed=TRUE)

  result_na_llq <- make_loq(c("<1"), replace_llq=NA_real_)
  expect_true(is.na(result_na_llq$number[1]))

  result_na_ulq <- make_loq(c(">60"), replace_ulq=NA_real_)
  expect_true(is.na(result_na_ulq$number[1]))

  expect_warning(
    make_loq(c(">ULQ"), ulq_pattern=">ULQ"),
    regexp="not converting"
  )

  result_text <- make_loq(c("A", "B"))
  expect_true(all(is.na(result_text$number)))
  expect_true(all(is.na(result_text$llq)))
  expect_true(all(is.na(result_text$ulq)))
})

test_that("duplicated_including_first", {
  expect_equal(
    duplicated_including_first(c(1, 1, 1)),
    rep(TRUE, 3)
  )
})
