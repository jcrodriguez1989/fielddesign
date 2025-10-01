# Helper function to create a dummy lm object for testing.
create_dummy_lm <- function(coef_list) {
  b <- unlist(coef_list)
  names(b) <- names(coef_list)
  fit <- list(coefficients = b)
  class(fit) <- "lm"
  fit
}

test_that("solve_analytical calculates optimal plot size with interaction", {
  # Coefficients corresponding to CV = 0.01*L^2 + 0.015*W^2 - 0.5*L - 0.6*W + 0.005*L*W + 10.
  coefs <- list(
    "(Intercept)" = 10,
    "Length" = -0.5,
    "Width" = -0.6,
    "I(Length^2)" = 0.01,
    "I(Width^2)" = 0.015,
    "Length:Width" = 0.005
  )
  fit_obj <- create_dummy_lm(coefs)
  tau_val <- c(0.1, 0.2)
  result <- solve_analytical(fit_obj, tau_val)
  expect_equal(result$h_star, 17.3913, tolerance = 1e-6)
  expect_equal(result$w_star, 10.43478, tolerance = 1e-6)
})

test_that("solve_analytical handles no interaction term", {
  # Coefficients corresponding to CV = 0.01*L^2 + 0.015*W^2 - 0.5*L - 0.6*W + 10 (no interaction).
  coefs <- list(
    "(Intercept)" = 10,
    "Length" = -0.5,
    "Width" = -0.6,
    "I(Length^2)" = 0.01,
    "I(Width^2)" = 0.015
    # No Length:Width term
  )
  fit_obj <- create_dummy_lm(coefs)
  tau_val <- c(0.1, 0.2)
  result <- solve_analytical(fit_obj, tau_val)
  expect_equal(result$h_star, 20, tolerance = 1e-6)
  expect_equal(result$w_star, 13.33333, tolerance = 1e-6)
})

test_that("solve_analytical returns NA for singular Hessian matrix", {
  # Create coefficients that lead to a singular Hessian matrix (e.g., if bh2 and bw2 are 0).
  # If all quadratic coefficients are zero, H = [[0, 0], [0, 0]], det(H) = 0.
  # This should cause the condition in solve_analytical to fail, resulting in h_star and w_star
  # remaining NA_real_.
  coefs <- list(
    "(Intercept)" = 10,
    "Length" = -0.5,
    "Width" = -0.6,
    "I(Length^2)" = 0, # Makes 2*bh2 = 0
    "I(Width^2)" = 0, # Makes 2*bw2 = 0
    "Length:Width" = 0 # Makes bhw = 0.
  )
  fit_obj <- create_dummy_lm(coefs)
  tau_val <- c(0.1, 0.2)
  result <- solve_analytical(fit_obj, tau_val)
  expect_true(is.na(result$h_star))
  expect_true(is.na(result$w_star))
})

test_that("solve_analytical calculates both h_star and w_star negative", {
  # Coefficients that lead to negative optimal h_star and w_star.
  # Assume no interaction for simplicity first: bhw = 0 CV = 0.01*L^2 + 0.01*W^2 - 0.1*L - 0.1*W.
  coefs <- list(
    "(Intercept)" = 0, # irrelevant for derivatives
    "Length" = -0.1,
    "Width" = -0.1,
    "I(Length^2)" = 0.01,
    "I(Width^2)" = 0.01,
    "Length:Width" = 0 # No interaction
  )
  fit_obj <- create_dummy_lm(coefs)
  tau_val <- c(0.5, 0.5)
  result <- solve_analytical(fit_obj, tau_val)
  expect_equal(result$h_star, -20, tolerance = 1e-6)
  expect_equal(result$w_star, -20, tolerance = 1e-6)
})

test_that("solve_analytical calculates h_star negative, w_star positive", {
  coefs <- list(
    "(Intercept)" = 0,
    "Length" = 0.2,
    "Width" = -0.2,
    "I(Length^2)" = 0.01,
    "I(Width^2)" = 0.01,
    "Length:Width" = 0
  )
  fit_obj <- create_dummy_lm(coefs)
  tau_val <- c(0.1, 0.1)
  result <- solve_analytical(fit_obj, tau_val)
  expect_equal(result$h_star, -15, tolerance = 1e-6)
  expect_equal(result$w_star, 5, tolerance = 1e-6)
})

test_that("solve_analytical calculates h_star positive, w_star negative", {
  coefs <- list(
    "(Intercept)" = 0,
    "Length" = -0.2,
    "Width" = 0.2,
    "I(Length^2)" = 0.01,
    "I(Width^2)" = 0.01,
    "Length:Width" = 0
  )
  fit_obj <- create_dummy_lm(coefs)
  tau_val <- c(0.1, 0.1)
  result <- solve_analytical(fit_obj, tau_val)
  expect_equal(result$h_star, 5, tolerance = 1e-6)
  expect_equal(result$w_star, -15, tolerance = 1e-6)
})
