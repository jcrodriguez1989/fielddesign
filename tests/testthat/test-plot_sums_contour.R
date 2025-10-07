# Mock spatial_variation_exhaustive object for testing.
mock_sv_exhaustive <- function() {
  set.seed(420)
  sums_data <- data.frame(
    w = c(1, 1, 2, 2, 1, 1),
    h = c(1, 1, 1, 1, 2, 2),
    c = c(1, 2, 1, 2, 1, 2),
    r = c(1, 1, 1, 1, 1, 1),
    sum = c(10, 12, 15, 18, 11, 13)
  )
  res_data <- data.frame(
    Width = c(1, 2, 1),
    Length = c(1, 1, 2),
    CV = c(0.1, 0.15, 0.12)
  )
  sv <- list(res = res_data, sums = sums_data)
  class(sv) <- c("sv_exhaustive", "sv")
  return(sv)
}

# Mock spatial_variation_tiling object for testing.
mock_sv_tiling <- function() {
  set.seed(420)
  sums_data <- data.frame(
    w = c(1, 1, 2, 2),
    h = c(1, 1, 1, 1),
    c = c(1, 2, 1, 2),
    r = c(1, 1, 1, 1),
    sum = c(20, 22, 25, 28)
  )
  res_data <- data.frame(
    Width = c(1, 2),
    Length = c(1, 1),
    CV = c(0.2, 0.25)
  )
  sv <- list(res = res_data, sums = sums_data)
  class(sv) <- c("sv_tiling", "sv")
  return(sv)
}

test_that("plot_sums_contour stops for invalid sv object type", {
  # Test with a generic list, not of class sv_tiling or sv_exhaustive
  invalid_sv <- list(a = 1, b = 2)
  expect_error(
    plot_sums_contour(invalid_sv, 1, 1),
    "sv must be created by `spatial_variation_exhaustive` or `spatial_variation_tiling`"
  )

  # Test with a class 'sv' but not specific 'sv_tiling' or 'sv_exhaustive'
  sv_generic <- list(res = data.frame(), sums = data.frame())
  class(sv_generic) <- "sv"
  expect_error(
    plot_sums_contour(sv_generic, 1, 1),
    "sv must be created by `spatial_variation_exhaustive` or `spatial_variation_tiling`"
  )
})

test_that("plot_sums_contour stops if sv object lacks 'sums' component", {
  # Create an sv_exhaustive object without the 'sums' component
  sv_no_sums <- list(res = data.frame(Width = 1, Length = 1, CV = 0.1))
  class(sv_no_sums) <- c("sv_exhaustive", "sv")
  expect_error(
    plot_sums_contour(sv_no_sums, 1, 1),
    "`spatial_variation_` function must be executed with parameter `return_sums = TRUE`"
  )
})

test_that("plot_sums_contour returns ggplot object for valid sv_exhaustive input", {
  sv_exh <- mock_sv_exhaustive()
  p <- plot_sums_contour(sv_exh, 1, 1)
  expect_s3_class(p, "ggplot")
  # Test with W=2, L=1
  p2 <- plot_sums_contour(sv_exh, 2, 1)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_sums_contour returns ggplot object for valid sv_tiling input", {
  sv_til <- mock_sv_tiling()
  p <- plot_sums_contour(sv_til, 1, 1)
  expect_s3_class(p, "ggplot")
})

test_that("plot_sums_contour returns NULL invisibly and messages for unmatched width/length", {
  sv_exh <- mock_sv_exhaustive()
  # Expected message based on unique(sv_exh$sums[, c("w", "h")]) which are (1,1), (1,2), (2,1)
  expect_message(
    expect_null(plot_sums_contour(sv_exh, 99, 99)),
    "Please set `\\(width, length)` parameters as one of: \\(1, 1) - \\(1, 2) - \\(2, 1)"
  )
})

test_that("plot_sums_contour messages for exhaustive with swapped dimensions if CV not found directly", {
  sv_exh <- mock_sv_exhaustive()
  # Trigger the message when width != length and CV for (width, length) is not directly found
  # but CV for (length, width) is found and sums_t is found.
  # For mock_sv_exhaustive, calling with (1,2) will directly find CV (1,2).
  # It will then check for sums_t (2,1) and generate the message.
  expect_message(
    plot_sums_contour(sv_exh, 1, 2),
    "The CV calculation also took into account W=2, L=1"
  )
})
