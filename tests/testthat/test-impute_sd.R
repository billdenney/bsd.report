context("impute_sd")

test_that("input checks work", {
  for (current_fun in c(impute_sd, impute_sd_ci, impute_sd_cv, impute_sd_iqr, impute_sd_range, impute_sd_sd, impute_sd_se)) {
    # Mismatch in argument lengths
    expect_error(current_fun(point=1:2, var1=1, var2=1, n=1, vartype="SD"))
    expect_error(current_fun(point=1, var1=1:2, var2=1, n=1, vartype="SD"))
    expect_error(current_fun(point=1, var1=1, var2=1:2, n=1, vartype="SD"))
    expect_error(current_fun(point=1, var1=1, var2=1, n=1:2, vartype="SD"))
    expect_error(current_fun(point=1, var1=1, var2=1, n=1, vartype=rep("SD", 2)))
    expect_error(current_fun(point=1:3, var1=1:3, var2=1:3, n=1:3, vartype=rep("SD", 2)))
    expect_error(current_fun(point=1, var1=1, var2=1, n=1, vartype=NA_character_))
  }
})

test_that("imputation selects the correct method", {
  expect_equal(
    impute_sd(point=1, var1=1, var2=NA_real_, n=5, vartype="SD"),
    impute_sd_sd(point=1, var1=1, var2=NA_real_, n=5, vartype="SD")
  )
  expect_equal(
    impute_sd(point=1, var1=0.5, var2=NA_real_, n=5, vartype="90% CI"),
    impute_sd_ci(point=1, var1=0.5, var2=NA_real_, n=5, vartype="90% CI")
  )
  expect_equal(
    impute_sd(point=1, var1=5, var2=NA_real_, n=5, vartype="CV"),
    impute_sd_cv(point=1, var1=5, var2=NA_real_, n=5, vartype="CV")
  )
  expect_equal(
    impute_sd(point=1, var1=5, var2=NA_real_, n=5, vartype="%CV"),
    impute_sd_cv(point=1, var1=5, var2=NA_real_, n=5, vartype="%CV")
  )
  expect_equal(
    impute_sd(point=1, var1=5, var2=NA_real_, n=5, vartype="CV%"),
    impute_sd_cv(point=1, var1=5, var2=NA_real_, n=5, vartype="CV%")
  )
  expect_equal(
    impute_sd(point=1, var1=0.5, var2=1.5, n=5, vartype="IQR"),
    impute_sd_iqr(point=1, var1=0.5, var2=1.5, n=5, vartype="IQR")
  )
  expect_equal(
    expect_warning(impute_sd(point=1, var1=0, var2=2, n=5, vartype="RANGE")),
    expect_warning(impute_sd_range(point=1, var1=0, var2=2, n=5, vartype="RANGE"))
  )
})

test_that("sd imputation works", {
  expect_error(
    impute_sd_sd(point=1, var1=1, var2=NA_real_, n=5, vartype="foo"),
    info="correct vartype"
  )
  expect_error(
    impute_sd_sd(point=1, var1=1, var2=1, n=5, vartype="SD"),
    info="no var2"
  )
  expect_equal(
    impute_sd_sd(point=1, var1=1, var2=NA_real_, n=5, vartype="SD"),
    1
  )
})

test_that("cv imputation works", {
  expect_error(
    impute_sd_cv(point=1, var1=5, var2=NA_real_, n=5, vartype="foo"),
    info="correct vartype"
  )
  expect_error(
    impute_sd_cv(point=1, var1=5, var2=1, n=5, vartype="CV"),
    info="no var2"
  )
  expect_equal(
    impute_sd_cv(point=1, var1=5, var2=NA_real_, n=5, vartype="CV"),
    0.05
  )
  expect_equal(
    expect_warning(
      impute_sd_cv(point=1, var1=0.05, var2=NA_real_, n=5, vartype="CV"),
      info="fractional input"
    ),
    0.0005
  )
})

test_that("se imputation works", {
  expect_error(
    impute_sd_se(point=1, var1=1, var2=NA_real_, n=5, vartype="foo"),
    info="correct vartype"
  )
  expect_error(
    impute_sd_se(point=1, var1=1, var2=1, n=5, vartype="SE"),
    info="no var2"
  )
  expect_equal(
    impute_sd_se(point=1, var1=1, var2=NA_real_, n=5, vartype="SE"),
    1*sqrt(4)
  )
})

test_that("ci imputation works", {
  expect_error(
    impute_sd_ci(point=1, var1=1, var2=1, n=5, vartype="foo"),
    regexp="vartype must match the regular expression",
    info="correct vartype"
  )
  expect_error(
    impute_sd_ci(point=1, var1=1, var2=2, n=5, vartype="90% CI"),
    regexp="var1 must be <= point", fixed=TRUE,
    info="var1 == point"
  )
  expect_error(
    impute_sd_ci(point=1, var1=2, var2=2, n=5, vartype="90% CI"),
    regexp="var1 must be <= point", fixed=TRUE,
    info="var1 > point"
  )
  expect_error(
    impute_sd_ci(point=1, var1=0, var2=1, n=5, vartype="90% CI"),
    regexp="var2 must be >= point", fixed=TRUE,
    info="var2 == point"
  )
  expect_error(
    impute_sd_ci(point=1, var1=0, var2=0, n=5, vartype="90% CI"),
    regexp="var2 must be >= point", fixed=TRUE,
    info="var2 < point"
  )
  expect_equal(
    impute_sd_ci(point=1, var1=0, var2=2, n=5, vartype="90% CI"),
    1/qt(0.95, 5)
  )
  expect_equal(
    impute_sd_ci(point=1, var1=NA_real_, var2=2, n=5, vartype="90% CI"),
    1/qt(0.95, 5),
    info="NA works in var1"
  )
  expect_equal(
    impute_sd_ci(point=1, var1=0, var2=NA_real_, n=5, vartype="90% CI"),
    1/qt(0.95, 5),
    info="NA works in var2"
  )
  expect_equal(
    impute_sd_ci(point=1, var1=0, var2=3, n=5, vartype="90% CI"),
    1.5/qt(0.95, 5),
    info="average of lower and upper is returned"
  )
})

test_that("iqr imputation works", {
  expect_error(
    impute_sd_iqr(point=1, var1=1, var2=NA_real_, n=5, vartype="foo"),
    info="correct vartype"
  )
  expect_error(
    impute_sd_iqr(point=1, var1=1, var2=1, n=5, vartype="IQR"),
    regexp="`var1` must be < `point`.", fixed=TRUE
  )
  expect_error(
    impute_sd_iqr(point=1, var1=0, var2=1, n=5, vartype="IQR"),
    regexp="`var2` must be > `point`.", fixed=TRUE
  )
  expect_equal(
    impute_sd_iqr(point=1, var1=0, var2=2, n=5, vartype="IQR"),
    2/(2*qt(p=0.75, df=5))
  )
})

test_that("range imputation works", {
  expect_error(
    expect_warning(impute_sd_range(point=1, var1=1, var2=NA_real_, n=5, vartype="foo")),
    info="correct vartype"
  )
  expect_error(
    expect_warning(impute_sd_range(point=1, var1=1, var2=1, n=5, vartype="RANGE")),
    regexp="`var1` must be < `point`.", fixed=TRUE
  )
  expect_error(
    expect_warning(impute_sd_range(point=1, var1=0, var2=1, n=5, vartype="RANGE")),
    regexp="`var2` must be > `point`.", fixed=TRUE
  )
  expect_equal(
    expect_warning(impute_sd_range(point=1, var1=0, var2=2, n=5, vartype="RANGE")),
    2/4
  )
})
