test_that("soft_hyphenate inserts soft hyphens", {
  # 8-char string: no trailing hyphen
  expect_equal(soft_hyphenate("12345678"), "12345678")
  # 9-char string: hyphen after position 8
  expect_equal(soft_hyphenate("123456789"), paste0("12345678", "­", "9"))
  # 16-char string with width=8: hyphen at 8
  result <- soft_hyphenate("1234567890123456", character_width = 8)
  expect_equal(result, paste0("12345678", "­", "90123456"))
})

test_that("knit_print.sessionInfo generates session info output", {
  result <- knit_print.sessionInfo()
  expect_true(inherits(result, "knit_asis"))
  expect_true(grepl(R.version$version.string, as.character(result), fixed = TRUE))
})

test_that("knit_print.sessionInfo accepts explicit sessionInfo argument", {
  si <- utils::sessionInfo()
  result <- knit_print.sessionInfo(si)
  expect_true(inherits(result, "knit_asis"))
})

test_that("knit_print.sessionInfo handles GithubSHA1 in package entry", {
  # Construct a minimal mock sessionInfo-like object that has a GithubSHA1 entry
  mock_pkg <- list(Package = "mypkg", Version = "1.0", GithubSHA1 = "abc123def456")
  si <- utils::sessionInfo()
  si$otherPkgs <- list(mypkg = mock_pkg)
  result <- knit_print.sessionInfo(si)
  expect_true(grepl("GithubSHA1", as.character(result), fixed = TRUE))
  # soft_hyphenate inserts a soft hyphen after 8 chars, so search for the first 8
  expect_true(grepl("abc123de", as.character(result), fixed = TRUE))
})
