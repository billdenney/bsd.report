context("sprintf_data_frame")

test_that("sprintf_data_frame_single", {
  expect_equal(
    sprintf_data_frame_single(
      data=mtcars,
      format=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    )[1:4],
    c(
      "21 miles/gallon, 6 cylinders", "21 miles/gallon, 6 cylinders", 
      "22.8 miles/gallon, 4 cylinders", "21.4 miles/gallon, 6 cylinders"
    )
  )
})

test_that("sprintf_data_frame_single errors", {
  expect_error(
    sprintf_data_frame_single(
      data=mtcars,
      format=c("%g miles/gallon, ", "%g cylinders")
    ),
    regexp="'format' must be named",
    fixed=TRUE
  )
  expect_error(
    sprintf_data_frame_single(
      data=mtcars,
      format=c(mpg="%g miles/gallon, ", "%g cylinders")
    ),
    regexp="All elements of 'format' must be named",
    fixed=TRUE
  )
  expect_error(
    sprintf_data_frame_single(
      data=mtcars,
      format=c(mpg="%g miles/gallon, ", foo="%g cylinders")
    ),
    regexp="All names of 'format' must be column headers in 'data'",
    fixed=TRUE
  )
})

test_that("sprintf_data_frame errors", {
  expect_error(
    sprintf_data_frame(
      data=mtcars,
      c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    ),
    regexp="... arguments must be named",
    fixed=TRUE
  )
  expect_error(
    sprintf_data_frame(
      data=mtcars,
      foo=c(mpg="%g miles/gallon, ", cyl="%g cylinders"),
      c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    ),
    regexp="All ... arguments must be named",
    fixed=TRUE
  )
  expect_error(
    sprintf_data_frame(
      data=mtcars,
      cyl=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    ),
    regexp="No names of ... may match names of data",
    fixed=TRUE
  )
})

test_that("sprintf_data_frame results", {
  expect_equal(
    sprintf_data_frame(
      data=mtcars,
      foo=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    )$foo,
    sprintf_data_frame_single(
      data=mtcars,
      format=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    )
  )
})
