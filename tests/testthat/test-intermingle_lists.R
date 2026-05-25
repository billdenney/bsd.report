test_that("intermingle_list interleaves two lists", {
  expect_equal(
    intermingle_list(as.list(1:3), as.list(4:6)),
    list(1, 4, 2, 5, 3, 6)
  )
})

test_that("intermingle_list interleaves three lists", {
  expect_equal(
    intermingle_list(as.list(1:2), as.list(3:4), as.list(5:6)),
    list(1, 3, 5, 2, 4, 6)
  )
})

test_that("intermingle_list errors on non-list argument", {
  expect_error(
    intermingle_list(1:3, as.list(1:3)),
    regexp = "All arguments must be lists",
    fixed = TRUE
  )
})

test_that("intermingle_list errors on unequal-length lists", {
  expect_error(
    intermingle_list(as.list(1:2), as.list(1:3)),
    regexp = "All lists must be the same length",
    fixed = TRUE
  )
})
