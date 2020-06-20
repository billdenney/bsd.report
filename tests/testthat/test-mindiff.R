context("mindiff")

test_that("mindiff", {
  expect_equal(
    mindiff(1:3, choices=c(1.5, 2.5)),
    c(1.5, 1.5, 2.5)
  )
  expect_equal(
    mindiff(1:3, choices=c(1.5, 2.5), tie="first"),
    mindiff(1:3, choices=c(1.5, 2.5)),
    info="Confirm default argument"
  )
  expect_equal(
    mindiff(1:3, choices=c(1.5, 2.5), tie="last"),
    c(1.5, 2.5, 2.5)
  )
  expect_equal(
    mindiff(1:3, choices=c(1.5, 2.5), tie="median-first"),
    c(1.5, 1.5, 2.5)
  )
  expect_equal(
    mindiff(1:3, choices=c(1.5, 2.5, 1.5), tie="median-first"),
    c(1.5, 1.5, 2.5)
  )
  expect_equal(
    mindiff(1:3, choices=c(1.5, 2.5, 1.5), tie="median-last"),
    c(1.5, 1.5, 2.5)
  )
  expect_error(
    mindiff(1:3, choices=c()),
    regexp="No `choices` given.",
    fixed=TRUE
  )
  expect_error(
    mindiff(1:3, choices=1, tie="foo"),
  )
})

test_that("mindiff_after", {
  expect_equal(
    mindiff_after(1:3, choices=c(1.5, 2.5)),
    c(-0.5, 0.5, 0.5)
  )
  expect_equal(
    mindiff_after(1:3, choices=c(1.5, 2, 2.5)),
    c(-0.5, 0, 0.5)
  )
  expect_equal(
    mindiff_after(1:3, choices=c(1.5, 2, 2.5), include_zero=FALSE),
    c(-0.5, 0.5, 0.5)
  )
  expect_equal(
    mindiff_after(1:3, choices=c(1.5, 2, 2.5), none="negative"),
    mindiff_after(1:3, choices=c(1.5, 2, 2.5)),
    info="Confirm default value for `none`"
  )
  expect_equal(
    mindiff_after(1:3, choices=c(1.5, 2, 2.5), none="negative"),
    c(-0.5, 0, 0.5)
  )
  expect_equal(
    mindiff_after(1:3, choices=c(1.5, 2, 2.5), none="na"),
    c(NA, 0, 0.5)
  )
  expect_equal(
    mindiff_after(1:3, choices=c(1.5, 2, 2.5), none="na"),
    c(NA, 0, 0.5)
  )
  expect_equal(
    expect_warning(
      mindiff_after(1:3, choices=c()),
      regexp="No `choices` given, returning NA",
      fixed=TRUE
    ),
    rep(NA_real_, 3)
  )
  expect_null(
    expect_warning(
      mindiff_after(c(), choices=1),
      regexp="`x` is zero-length, cannot match to any `choices`",
      fixed=TRUE
    )
  )
  expect_null(
    expect_warning(
      mindiff_after(c(), choices=c()),
      regexp="`x` is zero-length, cannot match to any `choices`",
      fixed=TRUE
    )
  )
})
