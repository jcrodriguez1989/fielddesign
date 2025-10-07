# Helper function to create spatial variation (sv) data for testing.
# This creates a convex quadratic surface for CV, allowing for predictable optima.
# The analytical minimum for CV (without penalties) is at (center_l, center_w).
# For a penalty `tau_h * h + tau_w * w`, the minimum of (CV + penalty) occurs at
# h_star ~> center_l - tau_h / (2 * scale_factor)
# w_star ~> center_w - tau_w / (2 * scale_factor)
create_test_sv <- function(nr, nc, center_l, center_w, scale_factor = 0.1, base_cv = 10) {
  sv_data <- expand.grid(Length = 1:nr, Width = 1:nc)
  sv_data$CV <- scale_factor * (sv_data$Length - center_l)^2 +
    scale_factor * (sv_data$Width - center_w)^2 + base_cv
  return(sv_data)
}

test_that("fit_exhaustive_optimal_plot_size - analytical solution is valid and within bounds", {
  nr <- 10
  nc <- 10
  tau <- c(1, 1)
  scale_factor <- 0.1
  # Choose center_l=8, center_w=8 so that h_star = 3, w_star = 3 (within [1,10]).
  sv <- create_test_sv(nr, nc, center_l = 8, center_w = 8, scale_factor = scale_factor)
  result <- fit_exhaustive_optimal_plot_size(sv, nr, nc, include_interaction = TRUE, tau = tau)
  expect_s3_class(result$fit, "lm")
  expect_equal(result$method_used, "analytical")
  # h_star and w_star should be close to 3.
  expect_equal(result$h_star, 3)
  expect_equal(result$w_star, 3)
  # For a simple quadratic, integer optimum should be the closest integer.
  expect_equal(result$h_opt, 3)
  expect_equal(result$w_opt, 3)
})

test_that("fit_exhaustive_optimal_plot_size - falls back to constrained optimization", {
  nr <- 10
  nc <- 10
  tau <- c(1, 1)
  scale_factor <- 0.1
  sv <- create_test_sv(nr, nc, center_l = 4, center_w = 4, scale_factor = scale_factor)
  expect_message(
    {
      result <- fit_exhaustive_optimal_plot_size(sv, nr, nc, include_interaction = TRUE, tau = tau)
    },
    "Analytical solution invalid or out of bounds. Using constrained optimization..."
  )
  expect_s3_class(result$fit, "lm")
  expect_equal(result$method_used, "constrained")
  # Constrained optimal solution should be at the boundary (1,1) for this scenario.
  expect_equal(result$h_star, 1)
  expect_equal(result$w_star, 1)
  expect_equal(result$h_opt, 1)
  expect_equal(result$w_opt, 1)
})

test_that("fit_exhaustive_optimal_plot_size - falls back to constrained optimization", {
  nr <- 10
  nc <- 10
  tau <- c(1, 1)
  scale_factor <- 0.1
  sv <- create_test_sv(nr, nc, center_l = 18, center_w = 18, scale_factor = scale_factor)
  expect_message(
    {
      result <- fit_exhaustive_optimal_plot_size(sv, nr, nc, include_interaction = TRUE, tau = tau)
    },
    "Analytical solution invalid or out of bounds. Using constrained optimization..."
  )
  expect_s3_class(result$fit, "lm")
  expect_equal(result$method_used, "constrained")
  # Constrained optimal solution should be at the boundary (10,10) for this scenario.
  expect_equal(result$h_star, 10)
  expect_equal(result$w_star, 10)
  expect_equal(result$h_opt, 10)
  expect_equal(result$w_opt, 10)
})


test_that("fit_exhaustive_optimal_plot_size - works with no interaction term", {
  nr <- 10
  nc <- 10
  tau <- c(1, 1)
  scale_factor <- 0.1
  # Choose center_l=8, center_w=8 so that h_star = 3, w_star = 3 (within [1,10])
  sv <- create_test_sv(nr, nc, center_l = 8, center_w = 8, scale_factor = scale_factor)
  result <- fit_exhaustive_optimal_plot_size(sv, nr, nc, include_interaction = FALSE, tau = tau)
  expect_s3_class(result$fit, "lm")
  expect_equal(result$method_used, "analytical")
  expect_equal(result$h_star, 3)
  expect_equal(result$w_star, 3)
  expect_equal(result$h_opt, 3)
  expect_equal(result$w_opt, 3)
  # Verify that the model formula used does not contain an interaction term
  expect_false(any(grepl("Length:Width", as.character(formula(result$fit)))))
})

test_that("fit_exhaustive_optimal_plot_size - handles unresolvable cases by returning NA", {
  nr <- 10
  nc <- 10
  tau <- c(1, 1)
  # Create `sv` data with NA CVs, which should lead to `lm` failing and then NA results.
  sv_na_cv <- expand.grid(Length = 1:nr, Width = 1:nc)
  sv_na_cv$CV <- NA_real_
  expect_error(fit_exhaustive_optimal_plot_size(
    sv_na_cv, nr, nc,
    include_interaction = TRUE, tau = tau
  ))
  expect_error(fit_exhaustive_optimal_plot_size(
    sv_na_cv, nr, nc,
    include_interaction = FALSE, tau = tau
  ))
})

# Add test for when nr or nc are very small, leading to edge cases.
test_that("fit_exhaustive_optimal_plot_size - handles small nr/nc dimensions", {
  nr <- 2
  nc <- 2
  tau <- c(1, 1)
  scale_factor <- 0.1
  # Analytical h_star = center_l - 5. For center_l=8, h_star=3 (out of bounds high for nr=2).
  sv <- create_test_sv(nr, nc, center_l = 8, center_w = 8, scale_factor = scale_factor)
  suppressWarnings(expect_message(
    {
      result <- fit_exhaustive_optimal_plot_size(sv, nr, nc, include_interaction = TRUE, tau = tau)
    },
    "Analytical solution invalid or out of bounds. Using constrained optimization..."
  ))
  expect_s3_class(result$fit, "lm")
  expect_equal(result$method_used, "constrained")
  # Should be constrained to the maximum possible values (nr, nc).
  expect_equal(result$h_opt, nr)
  expect_equal(result$w_opt, nc)
})

test_that("fit_exhaustive_optimal_plot_size - optimal plot size cannot be calculated", {
  nr <- 1 # Small nr, making Length effectively constant
  nc <- 5 # Some variation in Width
  tau <- c(1, 1)
  # Create `sv` data with constant CV. With Length being constant, and `include_interaction = TRUE`,
  # the `lm` model will produce NA coefficients for `Length` and `I(Length^2)` due to collinearity
  # with the intercept and `Length:Width` (which becomes `Width`). This should lead to problems
  # with subsequent calculations in analytical and constrained solvers, yielding NAs
  # for h_star/w_star and triggering the warning.
  sv <- expand.grid(Length = 1:nr, Width = 1:nc)
  sv$CV <- 10
  suppressWarnings(expect_warning(
    result <- fit_exhaustive_optimal_plot_size(sv, nr, nc, include_interaction = TRUE, tau = tau),
    "Couldn't calculate optimal plot size"
  ))
  expect_true(is.na(result$h_star))
  expect_true(is.na(result$w_star))
  expect_true(is.na(result$h_opt))
  expect_true(is.na(result$w_opt))
  expect_true(is.na(result$method_used))
})
