test_that("make_tab_models_tested returns tibble with correct structure", {
  m1 <- lm(mpg ~ cyl, mtcars)
  m2 <- lm(mpg ~ 1, mtcars)
  result <- make_tab_models_tested(
    models = list(Full = m1, Null = m2),
    caption = "Model comparison"
  )
  expect_true(is.data.frame(result))
  expect_equal(names(result), c("Description", "AIC", "dAIC"))
  expect_equal(nrow(result), 2L)
})

test_that("make_tab_models_tested attaches caption attribute", {
  m1 <- lm(mpg ~ cyl, mtcars)
  m2 <- lm(mpg ~ 1, mtcars)
  result <- make_tab_models_tested(list(Full = m1, Null = m2), caption = "My caption")
  expect_equal(attr(result, "caption"), "My caption")
})

test_that("make_tab_models_tested dAIC is 0 for the best model", {
  m1 <- lm(mpg ~ cyl, mtcars)
  m2 <- lm(mpg ~ 1, mtcars)
  result <- make_tab_models_tested(list(Full = m1, Null = m2), caption = "test")
  expect_equal(min(result$dAIC), 0)
  expect_true(all(result$dAIC >= 0))
})

test_that("make_tab_models_tested Description matches model names", {
  m1 <- lm(mpg ~ cyl, mtcars)
  m2 <- lm(mpg ~ 1, mtcars)
  result <- make_tab_models_tested(list(Full = m1, Null = m2), caption = "test")
  expect_equal(result$Description, c("Full", "Null"))
})
