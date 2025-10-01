# Helper function to create non-collinear test data.
create_test_data <- function() {
  data.frame(
    Length = c(2, 3, 5, 7, 10),
    Width = c(3, 5, 4, 8, 6),
    CV = c(10, 8, 6, 4, 2)
  )
}

test_that("solve_constrained returns correct structure", {
  test_data <- create_test_data()
  fit <- lm(CV ~ Length + Width, data = test_data)
  result <- solve_constrained(fit, tau = c(0.1, 0.1), nr = 10, nc = 10)
  expect_type(result, "list")
  expect_named(result, c("h_star", "w_star"))
  expect_type(result$h_star, "double")
  expect_type(result$w_star, "double")
})

test_that("solve_constrained respects bounds", {
  test_data <- create_test_data()
  fit <- lm(CV ~ Length + Width, data = test_data)
  result <- solve_constrained(fit, tau = c(0.1, 0.1), nr = 15, nc = 20)
  expect_gte(result$h_star, 1)
  expect_lte(result$h_star, 15)
  expect_gte(result$w_star, 1)
  expect_lte(result$w_star, 20)
})

test_that("solve_constrained handles different tau values", {
  test_data <- create_test_data()
  fit <- lm(CV ~ Length + Width, data = test_data)
  # Zero penalties.
  result_zero <- solve_constrained(fit, tau = c(0, 0), nr = 10, nc = 10)
  expect_false(is.na(result_zero$h_star))
  expect_false(is.na(result_zero$w_star))
  # High penalties should push toward lower values.
  result_high <- solve_constrained(fit, tau = c(10, 10), nr = 10, nc = 10)
  expect_false(is.na(result_high$h_star))
  expect_false(is.na(result_high$w_star))
  # Asymmetric penalties.
  result_asym <- solve_constrained(fit, tau = c(1, 0.1), nr = 10, nc = 10)
  expect_false(is.na(result_asym$h_star))
  expect_false(is.na(result_asym$w_star))
})

test_that("solve_constrained handles very small bounds", {
  test_data <- create_test_data()
  fit <- lm(CV ~ Length + Width, data = test_data)
  result <- solve_constrained(fit, tau = c(0.1, 0.1), nr = 2, nc = 2)
  expect_gte(result$h_star, 1)
  expect_lte(result$h_star, 2)
  expect_gte(result$w_star, 1)
  expect_lte(result$w_star, 2)
})

test_that("solve_constrained works with interaction terms", {
  test_data <- create_test_data()
  fit <- lm(CV ~ Length * Width, data = test_data)
  result <- solve_constrained(fit, tau = c(0.1, 0.1), nr = 10, nc = 10)
  expect_type(result, "list")
  expect_false(is.na(result$h_star))
  expect_false(is.na(result$w_star))
})

test_that("solve_constrained works with polynomial terms", {
  test_data <- create_test_data()
  fit <- lm(CV ~ poly(Length, 2) + poly(Width, 2), data = test_data)
  result <- solve_constrained(fit, tau = c(0.1, 0.1), nr = 10, nc = 10)
  expect_type(result, "list")
  expect_false(is.na(result$h_star))
  expect_false(is.na(result$w_star))
})

test_that("solve_constrained returns NA when optimization fails", {
  test_data <- create_test_data()
  fit <- lm(CV ~ Length + Width, data = test_data)
  # Test with extreme values that might cause numerical issues.
  result <- solve_constrained(fit, tau = c(1e10, 1e10), nr = 1e-10, nc = 1e-10)
  # Should handle gracefully.
  expect_type(result, "list")
})

test_that("solve_constrained finds minimum with decreasing CV model", {
  # Model where CV decreases with size (optimal should be at upper bounds).
  test_data <- data.frame(
    Length = c(1, 3, 5, 7, 9),
    Width = c(2, 4, 6, 8, 10),
    CV = c(10, 8, 6, 4, 2)
  )
  fit <- lm(CV ~ Length + Width, data = test_data)
  # With zero penalty, should prefer larger sizes.
  suppressWarnings(result <- solve_constrained(fit, tau = c(0, 0), nr = 10, nc = 10))
  # Should push toward upper bounds.
  expect_gte(result$h_star, 5)
  expect_gte(result$w_star, 5)
})

test_that("solve_constrained finds minimum with increasing CV model", {
  # Model where CV increases with size (optimal should be at lower bounds).
  test_data <- data.frame(
    Length = c(1, 3, 5, 7, 9),
    Width = c(2, 4, 6, 8, 10),
    CV = c(2, 4, 6, 8, 10)
  )
  fit <- lm(CV ~ Length + Width, data = test_data)
  # With zero penalty, should prefer smaller sizes.
  suppressWarnings(result <- solve_constrained(fit, tau = c(0, 0), nr = 10, nc = 10))
  # Should push toward lower bounds.
  expect_lte(result$h_star, 5)
  expect_lte(result$w_star, 5)
})

test_that("solve_constrained handles rectangular bounds correctly", {
  test_data <- create_test_data()
  fit <- lm(CV ~ Length + Width, data = test_data)
  # Very different nr and nc.
  result <- solve_constrained(fit, tau = c(0.1, 0.1), nr = 5, nc = 20)
  expect_gte(result$h_star, 1)
  expect_lte(result$h_star, 5)
  expect_gte(result$w_star, 1)
  expect_lte(result$w_star, 20)
})

test_that("solve_constrained produces consistent results", {
  test_data <- create_test_data()
  fit <- lm(CV ~ Length + Width, data = test_data)
  # Run multiple times with same inputs.
  result1 <- solve_constrained(fit, tau = c(0.5, 0.5), nr = 10, nc = 10)
  result2 <- solve_constrained(fit, tau = c(0.5, 0.5), nr = 10, nc = 10)
  expect_equal(result1$h_star, result2$h_star, tolerance = 1e-6)
  expect_equal(result1$w_star, result2$w_star, tolerance = 1e-6)
})

test_that("solve_constrained handles negative coefficients", {
  # Model with negative coefficients.
  test_data <- data.frame(
    Length = c(1, 3, 5, 7, 9),
    Width = c(2, 4, 6, 8, 10),
    CV = c(20, 18, 16, 14, 12)
  )
  fit <- lm(CV ~ Length + Width, data = test_data)
  suppressWarnings(result <- solve_constrained(fit, tau = c(0.1, 0.1), nr = 10, nc = 10))
  expect_false(is.na(result$h_star))
  expect_false(is.na(result$w_star))
  expect_gte(result$h_star, 1)
  expect_gte(result$w_star, 1)
})

test_that("penalty terms affect optimization direction", {
  test_data <- create_test_data()
  fit <- lm(CV ~ Length + Width, data = test_data)
  # High penalty on Length should favor smaller h_star.
  result_high_tau1 <- solve_constrained(fit, tau = c(5, 0.1), nr = 10, nc = 10)
  # High penalty on Width should favor smaller w_star.
  result_high_tau2 <- solve_constrained(fit, tau = c(0.1, 5), nr = 10, nc = 10)
  # Compare with balanced penalties.
  result_balanced <- solve_constrained(fit, tau = c(0.1, 0.1), nr = 10, nc = 10)
  # High Length penalty should push h_star lower.
  expect_lte(result_high_tau1$h_star, result_balanced$h_star + 1)
  # High Width penalty should push w_star lower.
  expect_lte(result_high_tau2$w_star, result_balanced$w_star + 1)
})

test_that("solve_constrained works with quadratic surface", {
  # Create data with a clear minimum in the interior.
  test_data <- data.frame(
    Length = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    Width = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    CV = c(10, 7, 5, 4, 3.5, 3.5, 4, 5, 7, 10)
  )
  fit <- lm(CV ~ poly(Length, 2) + poly(Width, 2), data = test_data)
  suppressWarnings(result <- solve_constrained(fit, tau = c(0, 0), nr = 10, nc = 10))
  expect_false(is.na(result$h_star))
  expect_false(is.na(result$w_star))
  # Should find interior minimum around 5-6.
  expect_gte(result$h_star, 3)
  expect_lte(result$h_star, 8)
})

test_that("solve_constrained handles models with intercept only effect", {
  # Model where Length and Width have minimal effect.
  test_data <- data.frame(
    Length = c(1, 3, 5, 7, 9),
    Width = c(2, 4, 6, 8, 10),
    CV = c(5.1, 5.0, 4.9, 5.0, 5.1) # Nearly constant.
  )
  fit <- lm(CV ~ Length + Width, data = test_data)
  suppressWarnings(result <- solve_constrained(fit, tau = c(0.5, 0.5), nr = 10, nc = 10))
  expect_false(is.na(result$h_star))
  expect_false(is.na(result$w_star))
  # With penalty, should prefer smaller values.
  expect_lte(result$h_star, 5)
  expect_lte(result$w_star, 5)
})

test_that("solve_constrained returns NA when all optimization attempts fail", {
  # Wrong names for length and width.
  test_data <- data.frame(
    length = 1:10,
    width = 11:20,
    CV = 1:10
  )
  fit <- lm(CV ~ length + width, data = test_data)
  max_rows <- 25
  max_cols <- 25
  result <- solve_constrained(fit, tau = c(0.01, 0.01), nr = 25, nc = 25)
  expected_result <- list(h_star = NA_real_, w_star = NA_real_)
  expect_equal(result, expected_result)
})
