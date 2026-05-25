test_that("remove_ggplot_legend hides the legend", {
  p <- ggplot2::ggplot(
    data.frame(x = 1:3, grp = c("a", "b", "a")),
    ggplot2::aes(x = x, y = x, color = grp)
  ) + ggplot2::geom_point()
  result <- remove_ggplot_legend(p)
  expect_true(inherits(result, "gg"))
  expect_equal(result$theme$legend.position, "none")
})

test_that("extract_ggplot_legend returns a grob", {
  p <- ggplot2::ggplot(
    data.frame(x = 1:3, grp = c("a", "b", "a")),
    ggplot2::aes(x = x, y = x, color = grp)
  ) + ggplot2::geom_point()
  legend <- extract_ggplot_legend(p)
  expect_true(inherits(legend, "grob") || inherits(legend, "gtable"))
})

test_that("plot_grid_one_legend returns plots plus legend", {
  p <- ggplot2::ggplot(
    data.frame(x = 1:3, grp = c("a", "b", "a")),
    ggplot2::aes(x = x, y = x, color = grp)
  ) + ggplot2::geom_point()
  result <- plot_grid_one_legend(p, p)
  # Two legend-free plots plus one legend = 3 elements
  expect_length(result, 3L)
  # Each of the first two should have legend hidden
  expect_equal(result[[1]]$theme$legend.position, "none")
  expect_equal(result[[2]]$theme$legend.position, "none")
})
