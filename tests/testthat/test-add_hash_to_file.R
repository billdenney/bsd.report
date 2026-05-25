test_that("add_hash_to_file attaches hash attribute", {
  tmp <- tempfile()
  writeLines("hello", tmp)
  on.exit(unlink(tmp))

  result <- add_hash_to_file(tmp)
  expect_equal(result, tmp, check.attributes=FALSE)
  expect_true(!is.null(attr(result, "hash")))
  expect_true(is.character(attr(result, "hash")))
})

test_that("add_hash_to_file handles vector of filenames", {
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  writeLines("a", tmp1)
  writeLines("b", tmp2)
  on.exit({ unlink(tmp1); unlink(tmp2) })

  result <- add_hash_to_file(c(tmp1, tmp2))
  expect_equal(result, c(tmp1, tmp2), check.attributes=FALSE)
  expect_length(attr(result, "hash"), 2L)
})

test_that("add_hash_to_file hash changes when file content changes", {
  tmp <- tempfile()
  writeLines("first", tmp)
  on.exit(unlink(tmp))

  hash1 <- attr(add_hash_to_file(tmp), "hash")
  writeLines("second", tmp)
  hash2 <- attr(add_hash_to_file(tmp), "hash")
  expect_false(identical(hash1, hash2))
})
