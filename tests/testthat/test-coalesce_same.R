context("coalesce_same")

test_that("coalesce_same vector success", {
  expect_equal(
    coalesce_same(c("A", NA), c(NA, "B")),
    c("A", "B")
  )
  expect_equal(
    coalesce_same(c(1, NA), c(NA, 2L)),
    c(1, 2),
    info="numeric and integer combine"
  )
  expect_equal(
    coalesce_same(
      factor(c("A", NA), levels=c("A", "B")),
      factor(c(NA, "B"), levels=c("A", "B"))
    ),
    factor(c("A", "B"), levels=c("A", "B"))
  )
})

test_that("coalesce_same vector warnings and errors", {
  expect_equal(
    expect_warning(
      coalesce_same(1:3),
      regexp="`coalesce_same()` is typically called with >1 argument.",
      fixed=TRUE
    ),
    1:3
  )
  expect_error(
    coalesce_same(1:2, 1:3),
    regexp="Argument 2 must have length 1 or the same length as the first argument.",
    fixed=TRUE
  )
  expect_error(
    coalesce_same("A", 1),
    regexp="Argument 2 must be a character, not a numeric.",
    fixed=TRUE
  )
  expect_error(
    coalesce_same(
      factor(c("A", NA), levels=c("A", "B", "C")),
      factor(c(NA, "B"), levels=c("A", "B"))
    ),
    regexp="level sets of factors are different",
    fixed=TRUE,
    info="factors may move into factors only with the same levels"
  )
  expect_error(
    coalesce_same(1:3, 3:1),
    regexp="Some items in argument 2 overlap with prior values, but the value is not the same.",
    fixed=TRUE
  )
})

test_that("coalesce_same data.frame success", {
  expect_equal(
    coalesce_same(
      data.frame(
        a=1:3,
        b=NA_character_,
        stringsAsFactors=FALSE
      ),
      data.frame(
        a=NA_real_,
        b=c("A", "B", NA_character_),
        stringsAsFactors=FALSE
      )
    ),
    data.frame(
      a=1:3,
      b=c("A", "B", NA_character_),
      stringsAsFactors=FALSE
    )
  )
  expect_equal(
    coalesce_same(
      data.frame(
        b=rep(NA_character_, 3),
        stringsAsFactors=FALSE
      ),
      data.frame(
        a=NA_real_,
        b=c("A", "B", NA_character_),
        stringsAsFactors=FALSE
      )
    ),
    data.frame(
      b=c("A", "B", NA_character_),
      stringsAsFactors=FALSE
    ),
    info="Extra columns are ignored"
  )
})

test_that("coalesce_same data.frame warnings and errors", {
  expect_equal(
    expect_warning(
      coalesce_same(
        data.frame(
          a=1:3,
          b=NA_character_,
          stringsAsFactors=FALSE
        )
      ),
      regexp="`coalesce_same()` is typically called with >1 argument.",
      fixed=TRUE
    ),
    data.frame(
      a=1:3,
      b=NA_character_,
      stringsAsFactors=FALSE
    )
  )
  expect_error(
    coalesce_same(data.frame(A=1), NULL),
    regexp="Argument 2 must be a data.frame since the first argument is a data.frame",
    fixed=TRUE
  )
})
