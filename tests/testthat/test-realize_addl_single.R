test_that("realize_addl_single handles evid=4 (reset+dose) correctly", {
  result <- realize_addl_single(time=0, evid=4, addl=2, ii=0.5)
  expect_equal(result$evid, c(4, 1, 1))
  expect_equal(result$time, c(0, 0.5, 1.0))
})

test_that("realize_addl_single drops observation rows (evid=0)", {
  result <- realize_addl_single(time=c(0, 1), evid=c(0, 1), addl=c(0, 2), ii=c(0, 0.5))
  expect_equal(nrow(result), 3)
  expect_true(all(result$evid == 1))
})

test_that("realize_addl_single stopifnot: NA in time on dosing rows", {
  expect_error(
    realize_addl_single(time=NA_real_, evid=1, addl=0, ii=0),
    regexp="time on dosing rows must not be NA"
  )
  # NA in addl/ii triggers the >= 0 check first (all(NA >= 0) = NA = falsy)
  expect_error(realize_addl_single(time=0, evid=1, addl=NA_real_, ii=0))
  expect_error(realize_addl_single(time=0, evid=1, addl=0, ii=NA_real_))
})

test_that("realize_addl_single", {
  expect_equal(
    realize_addl_single(time=numeric(), evid=numeric(), addl=numeric(), ii=numeric()),
    data.frame(time=numeric(), evid=numeric(), addl=numeric(), ii=numeric())
  )
  expect_error(
    realize_addl_single(time=NA, evid=1, addl=2, ii=2),
    regexp="is.numeric(time) is not TRUE",
    fixed=TRUE
  )
  expect_error(
    realize_addl_single(time=0, evid=1, addl=-2, ii=2),
    regexp="addl must not be non-negative"
  )
  expect_error(
    realize_addl_single(time=0, evid=1, addl=2, ii=-2),
    regexp="ii must not be non-negative"
  )
  expect_equal(
    realize_addl_single(time=0, evid=1, addl=2, ii=0.5),
    data.frame(time=0.5*(0:2), evid=1, addl=0, ii=0)
  )
  expect_equal(
    realize_addl_single(time=2, evid=1, addl=2, ii=0.5),
    data.frame(time=2 + 0.5*(0:2), evid=1, addl=0, ii=0)
  )
  expect_error(
    realize_addl_single(time=0, evid=1, addl=2.1, ii=0.5),
    regexp="addl must be an integer"
  )
})
