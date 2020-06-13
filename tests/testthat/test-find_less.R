context("find_less")

test_that("find_less normal operation", {
  expect_equal(
    find_less(c("A", "B", "C"), "B", include_same=TRUE, none="first"),
    rep("B", 3)
  )
  expect_equal(
    find_less(c("A", "B", "C"), "B", include_same=TRUE, none="na"),
    c(NA, "B", "B")
  )
  expect_equal(
    find_less(c("A", "B", "C"), "B", include_same=FALSE, none="na"),
    c(NA, NA, "B")
  )
  expect_equal(
    find_less(c("A", "C", "D"), "B", include_same=FALSE, none="na"),
    c(NA, "B", "B")
  )
  expect_equal(
    find_less(c("A", "B", "C"), "Z", include_same=FALSE, none="na"),
    rep(NA_character_, 3)
  )
  expect_equal(
    find_less(c("A", "B", "C"), 1, include_same=FALSE, none="na"),
    rep(1, 3),
    info="Comparison only has to be able to occur between the types."
  )
  expect_equal(
    find_less(c("A", "C", "D", NA), "B", include_same=FALSE, none="na"),
    c(NA, "B", "B", NA)
  )
})

test_that("find_less corner cases", {
  expect_null(
    expect_warning(
      find_less(c(), c()),
      regexp="An empty vector was given for `x`, returning empty",
      fixed=TRUE
    )
  )
  expect_equal(
    expect_warning(
      find_less(c(), character()),
      regexp="An empty vector was given for `x`, returning empty",
      fixed=TRUE
    ),
    character(),
    info="Output type matches input type"
  )
  expect_equal(
    expect_warning(
      find_less("A", character()),
      regexp="`choices` only contains `NA` values or is empty; returning NA.",
      fixed=TRUE
    ),
    NA_character_,
    info="Output type matches input type"
  )
  expect_equal(
    expect_warning(
      find_less(1, character()),
      regexp="`choices` only contains `NA` values or is empty; returning NA.",
      fixed=TRUE
    ),
    NA_character_,
    info="Output type matches input type (even when input types don't match)"
  )
  expect_equal(
    expect_warning(
      find_less(character(), 1),
      regexp="An empty vector was given for `x`, returning empty.",
      fixed=TRUE
    ),
    numeric(),
    info="Output type matches input type (even when input types don't match)"
  )
})
