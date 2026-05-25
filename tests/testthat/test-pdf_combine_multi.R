create_test_pdf <- function(n_pages = 1, path = tempfile(fileext = ".pdf")) {
  grDevices::pdf(path, width = 8, height = 11)
  for (i in seq_len(n_pages)) {
    graphics::plot(i, main = paste("Page", i))
  }
  grDevices::dev.off()
  path
}

test_that("pdf_combine_multi combines PDFs without pages argument", {
  p1 <- create_test_pdf()
  p2 <- create_test_pdf()
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(p1, p2, out)))
  result <- bsd.report:::pdf_combine_multi(input = c(p1, p2), output = out)
  expect_true(file.exists(out))
  expect_equal(qpdf::pdf_length(out), 2L)
})

test_that("pdf_combine_multi selects pages subset", {
  p1 <- create_test_pdf(n_pages = 3)
  p2 <- create_test_pdf(n_pages = 2)
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(p1, p2, out)))
  bsd.report:::pdf_combine_multi(input = c(p1, p2), pages = list(1:2, 1L), output = out)
  expect_equal(qpdf::pdf_length(out), 3L) # 2 pages from p1 + 1 page from p2
})

test_that("pdf_combine_multi auto-generates output filename", {
  p1 <- create_test_pdf()
  p2 <- create_test_pdf()
  auto_out <- sub("\\.pdf$", "_combined.pdf", p1)
  on.exit(unlink(c(p1, p2, auto_out)))
  bsd.report:::pdf_combine_multi(input = c(p1, p2))
  expect_true(file.exists(auto_out))
})

test_that("pdf_combine_multi errors for non-character input", {
  expect_error(bsd.report:::pdf_combine_multi(input = 1:2))
})

test_that("pdf_combine_multi errors when input and pages lengths differ", {
  p1 <- create_test_pdf()
  on.exit(unlink(p1))
  expect_error(bsd.report:::pdf_combine_multi(input = p1, pages = list(1L, 2L)))
})

test_that("pdf_combine_multi errors when pages is not a list", {
  p1 <- create_test_pdf()
  on.exit(unlink(p1))
  expect_error(bsd.report:::pdf_combine_multi(input = p1, pages = 1L))
})
