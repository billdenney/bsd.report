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
