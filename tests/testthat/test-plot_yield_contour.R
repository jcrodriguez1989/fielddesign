test_that("plot_yield_contour handles invalid input", {
  expect_error(plot_yield_contour("not a matrix"), "x must be a numeric matrix")
  expect_error(plot_yield_contour(data.frame(a = 1, b = 2)), "x must be a numeric matrix")
  expect_error(plot_yield_contour(list(1, 2, 3)), "x must be a numeric matrix")
})

test_that("plot_yield_contour returns a ggplot object for valid input", {
  valid_matrix <- matrix(rnorm(100), nrow = 10)
  p <- plot_yield_contour(valid_matrix)
  expect_s3_class(p, "ggplot")
})
