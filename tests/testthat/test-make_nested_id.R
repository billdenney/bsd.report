test_that("make_nested_id returns IDs for single column", {
  result <- make_nested_id(c("B", "A", "B"))
  expect_equal(result[1], result[3])   # same input → same ID
  expect_false(result[1] == result[2]) # different inputs → different IDs
})

test_that("make_nested_id produces unique IDs for unique two-column combinations", {
  result <- make_nested_id(c("A", "A", "B"), c("X", "Y", "X"))
  expect_length(result, 3)
  expect_equal(result[1], 11)  # A,X: outer=1, inner=1, multiplier=10 → 11
  expect_equal(result[2], 12)  # A,Y: outer=1, inner=2 → 12
  expect_equal(result[3], 21)  # B,X: outer=2, inner=1 → 21
})

test_that("make_nested_id is stable: adding inner value in another group does not change IDs", {
  r_before <- make_nested_id(c("A", "A", "B"), c("X", "Y", "X"))
  r_after  <- make_nested_id(c("A", "A", "B", "B"), c("X", "Y", "X", "Z"))
  expect_equal(r_before[1], r_after[1]) # A,X unchanged
  expect_equal(r_before[2], r_after[2]) # A,Y unchanged
  expect_equal(r_before[3], r_after[3]) # B,X unchanged
})

test_that("make_nested_id boundary: exactly 10 unique inner values triggers 3-digit IDs", {
  # 9 unique inner values → max 2-digit result
  r9  <- make_nested_id(c(rep("G1", 9),  "G2"), c(paste0("V", 1:9),  "V1"))
  # 10 unique inner values → any(1:10 >= 10^1) is TRUE → extra digit → 3-digit result
  r10 <- make_nested_id(c(rep("G1", 10), "G2"), c(paste0("V", 1:10), "V1"))
  expect_true(max(r9)  < 100)
  expect_true(max(r10) >= 100)
})
