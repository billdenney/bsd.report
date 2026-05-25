test_that("as_gg_list adds gg_list class to a list", {
  result <- as_gg_list(list(1, 2))
  expect_equal(class(result), "gg_list")
})

test_that("as_gg_list errors for non-list", {
  expect_error(as_gg_list("not a list"), regexp = "Not a list", fixed = TRUE)
})

test_that("knit_print.gg returns x invisibly and cat newlines", {
  p <- ggplot2::ggplot() + ggplot2::geom_blank()
  result <- NULL
  expect_output(
    result <- withVisible(knit_print.gg(p)),
    regexp = "\n\n",
    fixed = TRUE
  )
  expect_false(result$visible)
  expect_identical(result$value, p)
})

test_that("knit_print.gg outputs fig_prefix when provided", {
  p <- ggplot2::ggplot() + ggplot2::geom_blank()
  expect_output(
    knit_print.gg(p, fig_prefix = "MYPREFIX"),
    regexp = "MYPREFIX",
    fixed = TRUE
  )
})

test_that("knit_print.gg outputs fig_suffix when provided", {
  p <- ggplot2::ggplot() + ggplot2::geom_blank()
  expect_output(
    knit_print.gg(p, fig_suffix = "MYSUFFIX"),
    regexp = "MYSUFFIX",
    fixed = TRUE
  )
})

test_that("knit_print.gg saves to file when filename provided", {
  p <- ggplot2::ggplot() + ggplot2::geom_blank()
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  knit_print.gg(p, filename = tmp)
  expect_true(file.exists(tmp))
})

test_that("knit_print.gg_list prints each plot", {
  p1 <- ggplot2::ggplot() + ggplot2::geom_blank()
  p2 <- ggplot2::ggplot() + ggplot2::geom_blank()
  gl <- as_gg_list(list(p1, p2))
  capture.output(result <- withVisible(knit_print.gg_list(gl)))
  expect_false(result$visible)
})

test_that("knit_print.gg_list with %d filename generates per-plot files", {
  p1 <- ggplot2::ggplot() + ggplot2::geom_blank()
  p2 <- ggplot2::ggplot() + ggplot2::geom_blank()
  gl <- as_gg_list(list(p1, p2))
  tmp_pattern <- file.path(tempdir(), "plot%d.png")
  files <- sprintf(tmp_pattern, 1:2)
  on.exit(unlink(files))
  capture.output(knit_print.gg_list(gl, filename = tmp_pattern))
  expect_true(all(file.exists(files)))
})

test_that("knit_print.gg_list errors with wrong-length filename", {
  p1 <- ggplot2::ggplot() + ggplot2::geom_blank()
  p2 <- ggplot2::ggplot() + ggplot2::geom_blank()
  gl <- as_gg_list(list(p1, p2))
  expect_error(
    knit_print.gg_list(gl, filename = "only_one.png")
  )
})
