test_that("bayesian_decision_figure returns expected list structure (log scale)", {
  result <- bayesian_decision_figure(
    n = 10, lrv = 0.8, urv = 1.25,
    one_sided_prob = 0.1, sd_single_measure = 0.3,
    sd_scale = "log"
  )
  expect_named(result, c("data", "plot_prob", "plot_lines"))
  expect_true(inherits(result$plot_prob, "gg"))
  expect_true(inherits(result$plot_lines, "gg"))
  expect_true(is.data.frame(result$data))
  expect_true(all(c("x", "p_go", "p_stop", "p_between", "p_pause") %in% names(result$data)))
  # Probabilities should be non-negative and finite
  expect_true(all(result$data$p_go >= 0, na.rm = TRUE))
  expect_true(all(result$data$p_stop >= 0, na.rm = TRUE))
})

test_that("bayesian_decision_figure works on linear scale", {
  result <- bayesian_decision_figure(
    n = 10, lrv = -0.2, urv = 0.2,
    one_sided_prob = 0.1, sd_single_measure = 0.3,
    sd_scale = "linear"
  )
  expect_named(result, c("data", "plot_prob", "plot_lines"))
  expect_true(is.data.frame(result$data))
})

test_that("bayesian_decision_figure accepts add_points", {
  result <- bayesian_decision_figure(
    n = 10, lrv = 0.8, urv = 1.25,
    one_sided_prob = 0.1, sd_single_measure = 0.3,
    sd_scale = "log",
    add_points = c(0.9, 1.1)
  )
  # The add_points (back-transformed) should appear in $data$x
  expect_true(any(abs(result$data$x - 0.9) < 1e-10))
  expect_true(any(abs(result$data$x - 1.1) < 1e-10))
})
