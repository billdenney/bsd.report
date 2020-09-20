context("data management")

test_that("get_data_manage_standard_cols", {
  d1 <-
    setNames(
      data.frame(
        ct=c("A", "B", "A"),
        cn=c("C", "D", "E"),
        stringsAsFactors=FALSE
      ),
      c("Column Type", "Column Name")
    )
  expect_equal(
    get_data_manage_standard_cols(d1, coltype=NULL),
    c("C", "E", "D"),
    info="Order is by first-found column type order"
  )
  expect_equal(
    get_data_manage_standard_cols(d1, coltype="A"),
    c("C", "E"),
    info="subsetting occurs"
  )
  expect_error(
    get_data_manage_standard_cols(d1, coltype=c("A", "C")),
    regexp="The following `Column Type` values were not found: `C`",
    fixed=TRUE,
    info="Missing `Column Types` yield an error"
  )
  expect_error(
    get_data_manage_standard_cols(d1),
    regexp='argument "coltype" is missing, with no default',
    fixed=TRUE,
    info="`coltype` is a required argument"
  )
})

test_that("check_expected_cols", {
  d1 <-
    data.frame(
      date=as.POSIXct("2020-05-31 12:02"),
      fA=factor("A"),
      A="A",
      one=1,
      stringsAsFactors=FALSE
    )
  colorder <- c("date", "one", "A", "fA")
  expect_equal(
    check_expected_cols(d1, colorder),
    d1[, colorder]
  )
  expect_error(
    check_expected_cols(d1, c("foo", colorder)),
    regexp="The following columns are missing: foo",
    fixed=TRUE,
    info="One missing column"
  )
  expect_error(
    check_expected_cols(d1, colorder[-1]),
    regexp="The following extra columns are present: date",
    fixed=TRUE,
    info="One extra column"
  )
  expect_error(
    check_expected_cols(d1, colorder[-(1:2)]),
    regexp="The following extra columns are present: date, one",
    fixed=TRUE,
    info="Multple extra columns"
  )
  expect_error(
    check_expected_cols(d1, c("foo", colorder[-1])),
    regexp="The following extra columns are present: date.*The following columns are missing: foo",
    info="Missing and extra columns"
  )
})

test_that("nonmem_column_order", {
  d1 <-
    data.frame(
      A="A",
      one=1,
      stringsAsFactors=FALSE
    )
  expect_equal(
    nonmem_column_order(d1),
    d1[, c("one", "A")]
  )
  d2 <-
    data.frame(
      date=as.POSIXct("2020-05-31 12:02"),
      fA=factor("A"),
      A="A",
      one=1,
      stringsAsFactors=FALSE
    )
  expect_equal(
    nonmem_column_order(d2),
    d2[, c("one", "date", "fA", "A")]
  )
  d3 <- data.frame(one=1)
  d4 <- data.frame(A="A", stringsAsFactors=FALSE)
  expect_equal(
    nonmem_column_order(d3),
    d3,
    info="One column maintains being a data.frame (numeric)"
  )
  expect_equal(
    nonmem_column_order(d4),
    d4,
    info="One column maintains being a data.frame (non-numeric)"
  )
})

test_that("round_to_precision", {
  expect_equal(
    round_to_precision(1111.111111111),
    round_to_precision(1111.111111111, digits=6)
  )
  expect_equal(round_to_precision(1), "1")
  expect_equal(round_to_precision(1.1), "1.1")
  expect_equal(round_to_precision(1.111111111), "1.111111")
  expect_equal(round_to_precision(1.111111111, digits=4), "1.1111")
  expect_equal(round_to_precision(-1), "-1")
  expect_equal(round_to_precision(-1.1), "-1.1")
  expect_equal(round_to_precision(-1.111111111), "-1.111111")
  expect_equal(round_to_precision(-1.111111111, digits=4), "-1.1111")
  expect_equal(round_to_precision(NA, digits=4), NA_character_)
  expect_equal(round_to_precision(NaN, digits=4), NA_character_)
  expect_equal(round_to_precision(Inf, digits=4), "Inf")
  expect_equal(round_to_precision(-Inf, digits=4), "-Inf")
})
