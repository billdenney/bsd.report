context("unicode_to_ascii")

test_that("unicode_to_ascii", {
  expect_equal(
    expect_message(
      unicode_to_ascii("\u03bc", verbose=TRUE),
      regexp="1 change for Unicode to ascii conversion"
    ),
    "u"
  )
  expect_equal(
    expect_silent(unicode_to_ascii("\u03bc", verbose=FALSE)),
    "u"
  )
  expect_equal(
    expect_message(
      unicode_to_ascii(c("\u03bc", NA), verbose=TRUE)
    ),
    "u"
  )
  expect_equal(
    expect_silent(unicode_to_ascii("\u03bc", verbose=FALSE)),
    "u"
  )
  expect_equal(
    unicode_to_ascii("\u4E01"),
    "ding"
  )
  expect_equal(
    unicode_to_ascii("\u4E01", general_conversion=FALSE),
    "\u4E01"
  )
  expect_equal(
    unicode_to_ascii(
      factor(c("\u4E01", "a"), levels=c("\u4E01", "a")),
      general_conversion=FALSE
    ),
    factor(c("\u4E01", "a"), levels=c("\u4E01", "a"))
  )
  expect_equal(
    unicode_to_ascii(
      factor(c("\u4E01", "a"), levels=c("\u4E01", "a"))
    ),
    factor(c("ding", "a"), levels=c("ding", "a"))
  )
  expect_equal(unicode_to_ascii(NA), NA)
  expect_equal(unicode_to_ascii(TRUE), TRUE)
  expect_equal(
    expect_message(
      unicode_to_ascii(5, verbose=TRUE),
      regexp="No Unicode to ascii changes for class: numeric",
      fixed=TRUE
    ),
    5
  )
  expect_null(
    expect_message(
      unicode_to_ascii(NULL, verbose=TRUE),
      regexp="No Unicode to ascii changes for class: NULL",
      fixed=TRUE
    )
  )
  expect_equal(
    expect_message(
      unicode_to_ascii(as.POSIXct("2020-06-20"), verbose=TRUE),
      regexp="Returning unchanged.*POSIXct"
    ),
    as.POSIXct("2020-06-20")
  )
  expect_equal(
    expect_message(
      unicode_to_ascii(
        data.frame(
          Date=as.POSIXct("2020-06-20"),
          Character=c("abc \u4E01 def", "\u4E01", "A", "\u03bc"),
          Number=1:4,
          stringsAsFactors=FALSE
        ),
        verbose=TRUE
      ),
      regexp="Unicode to ascii current column number and name: 2, Character",
      fixed=TRUE
    ),
    data.frame(
      Date=as.POSIXct("2020-06-20"),
      Character=c("abc ding def", "ding", "A", "u"),
      Number=1:4,
      stringsAsFactors=FALSE
    )
  )
})
