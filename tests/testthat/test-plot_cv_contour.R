test_that("plot_cv_contour returns a ggplot object", {
  # Mock fit with a simple lm model
  df <- expand.grid(Length = 1:5, Width = 1:5)
  df$y <- with(df, Length + Width)
  fit <- lm(y ~ Length + Width, data = df)
  p <- plot_cv_contour(fit, nr = 5, nc = 5)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cv_contour applies custom labels", {
  df <- expand.grid(Length = 1:3, Width = 1:3)
  df$y <- with(df, Length + Width)
  fit <- lm(y ~ Length + Width, data = df)
  title <- "My Custom Title"
  p <- plot_cv_contour(fit, nr = 3, nc = 3, title = title, xlab = "Custom X", ylab = "Custom Y")
  labels <- p$labels
  expect_equal(labels$title, title)
  expect_equal(labels$x, "Custom X")
  expect_equal(labels$y, "Custom Y")
})

test_that("plot_cv_contour marks points correctly", {
  df <- expand.grid(Length = 1:3, Width = 1:3)
  df$y <- with(df, Length + Width)
  fit <- lm(y ~ Length + Width, data = df)
  mark <- data.frame(Width = c(1.1, 2), Length = c(1.5, 2.5))
  mark_lab <- "Test point"
  p <- plot_cv_contour(fit, nr = 3, nc = 3, mark = mark, mark_lab = mark_lab)
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$data[[2]]$x, mark$Width)
  expect_equal(built$data[[2]]$y, mark$Length)
  expect_equal(built$data[[3]]$label, mark_lab)
})

test_that("plot_cv_contour works without mark", {
  df <- expand.grid(Length = 1:4, Width = 1:4)
  df$y <- with(df, Length + Width)
  fit <- lm(y ~ Length + Width, data = df)
  p <- plot_cv_contour(fit, nr = 4, nc = 4)
  built <- ggplot2::ggplot_build(p)
  layers <- vapply(built$plot$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomPoint" %in% layers)
  expect_false("GeomText" %in% layers)
})
