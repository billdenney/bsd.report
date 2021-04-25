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

test_that("sprintf_data_frame_single factoring", {
  mtcars_f <- mtcars
  mtcars_f$cyl <- factor(mtcars_f$cyl)
  mtcars_o <- mtcars
  mtcars_o$cyl <- ordered(mtcars_f$cyl)
  expect_false(
    is.factor(sprintf_data_frame_single(
      data=mtcars,
      format=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    ))
  )
  expect_true(
    is.factor(sprintf_data_frame_single(
      data=mtcars_f,
      format=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    ))
  )
  expect_true(
    is.ordered(sprintf_data_frame_single(
      data=mtcars_o,
      format=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    ))
  )
  expect_true(
    is.ordered(sprintf_data_frame_single(
      data=mtcars_f,
      format=c(mpg="%g miles/gallon, ", cyl="%g cylinders"),
      ordered=TRUE
    ))
  )
  expect_false(
    is.ordered(sprintf_data_frame_single(
      data=mtcars_o,
      format=c(mpg="%g miles/gallon, ", cyl="%g cylinders"),
      ordered=FALSE
    ))
  )
  expect_equal(
    levels(sprintf_data_frame_single(
      data=mtcars_f,
      format=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    )),
    levels(sprintf_data_frame_single(
      data=mtcars_o,
      format=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    ))
  )
  expect_equal(
    sprintf_data_frame_single(
      data=mtcars_f,
      format=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
    ),
    structure(
      c(17L, 17L, 21L, 19L, 13L, 12L, 3L, 22L, 21L, 14L, 
                11L, 9L, 10L, 6L, 1L, 1L, 4L, 26L, 25L, 27L, 20L, 7L, 6L, 2L, 
                15L, 24L, 23L, 25L, 8L, 16L, 5L, 18L),
      .Label = c("10.4 miles/gallon, 3 cylinders", 
                 "13.3 miles/gallon, 3 cylinders", "14.3 miles/gallon, 3 cylinders", 
                 "14.7 miles/gallon, 3 cylinders", "15 miles/gallon, 3 cylinders", 
                 "15.2 miles/gallon, 3 cylinders", "15.5 miles/gallon, 3 cylinders", 
                 "15.8 miles/gallon, 3 cylinders", "16.4 miles/gallon, 3 cylinders", 
                 "17.3 miles/gallon, 3 cylinders", "17.8 miles/gallon, 2 cylinders", 
                 "18.1 miles/gallon, 2 cylinders", "18.7 miles/gallon, 3 cylinders", 
                 "19.2 miles/gallon, 2 cylinders", "19.2 miles/gallon, 3 cylinders", 
                 "19.7 miles/gallon, 2 cylinders", "21 miles/gallon, 2 cylinders", 
                 "21.4 miles/gallon, 1 cylinders", "21.4 miles/gallon, 2 cylinders", 
                 "21.5 miles/gallon, 1 cylinders", "22.8 miles/gallon, 1 cylinders", 
                 "24.4 miles/gallon, 1 cylinders", "26 miles/gallon, 1 cylinders", 
                 "27.3 miles/gallon, 1 cylinders", "30.4 miles/gallon, 1 cylinders", 
                 "32.4 miles/gallon, 1 cylinders", "33.9 miles/gallon, 1 cylinders"
      ), class = "factor"
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
