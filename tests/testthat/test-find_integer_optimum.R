test_that("find_integer_optimum handles non-finite h_star or w_star", {
  x <- data.frame(Length = 1:2, Width = 1:2, CV = c(0.1, 0.2))
  fit_model <- lm(CV ~ Length + Width, data = x)
  tau_vec <- c(0.1, 0.1)
  nr_max <- 10
  nc_max <- 10

  result_h_na <- find_integer_optimum(NA_real_, 5.8, fit_model, tau_vec, nr_max, nc_max)
  expect_equal(result_h_na$h_opt, NA_integer_)
  expect_equal(result_h_na$w_opt, NA_integer_)
  expect_equal(result_h_na$pred_at_opt, NA_real_)

  result_w_na <- find_integer_optimum(10.3, NA_real_, fit_model, tau_vec, nr_max, nc_max)
  expect_equal(result_w_na$h_opt, NA_integer_)
  expect_equal(result_w_na$w_opt, NA_integer_)
  expect_equal(result_w_na$pred_at_opt, NA_real_)

  result_h_inf <- find_integer_optimum(Inf, 5.8, fit_model, tau_vec, nr_max, nc_max)
  expect_equal(result_h_inf$h_opt, NA_integer_)
  expect_equal(result_h_inf$w_opt, NA_integer_)
  expect_equal(result_h_inf$pred_at_opt, NA_real_)
})

test_that("find_integer_optimum finds optimal integers within bounds", {
  # Create a dummy model where CV decreases with Length and Width
  x <- data.frame(
    Length = c(1, 5, 10, 20, 1, 5, 10, 20),
    Width = c(1, 1, 1, 1, 5, 5, 5, 5),
    CV = c(0.5, 0.3, 0.2, 0.1, 0.4, 0.2, 0.1, 0.05)
  )
  fit_model <- lm(CV ~ Length + Width, data = x)

  tau_vec <- c(0, 0) # No penalties for simplicity in this test
  nr_max <- 20
  nc_max <- 10

  # Test case 1: h_star and w_star are far from boundaries
  h_star_1 <- 10.3
  w_star_1 <- 5.8
  result_1 <- find_integer_optimum(h_star_1, w_star_1, fit_model, tau_vec, nr_max, nc_max)

  expect_equal(result_1$h_opt, 11)
  expect_equal(result_1$w_opt, 6)
  expect_equal(round(result_1$pred_at_opt, 3), 0.130, ignore_attr = TRUE)

  # Test case 2: h_star and w_star are near upper bounds
  h_star_2 <- 19.8
  w_star_2 <- 9.7
  result_2 <- find_integer_optimum(h_star_2, w_star_2, fit_model, tau_vec, nr_max, nc_max)

  expect_equal(result_2$h_opt, 20)
  expect_equal(result_2$w_opt, 10)
  expect_equal(round(result_2$pred_at_opt, 3), -0.119, ignore_attr = TRUE)

  # Test case 3: h_star and w_star are near lower bounds
  h_star_3 <- 1.2
  w_star_3 <- 1.4
  result_3 <- find_integer_optimum(h_star_3, w_star_3, fit_model, tau_vec, nr_max, nc_max)

  expect_equal(result_3$h_opt, 2)
  expect_equal(result_3$w_opt, 2)
  expect_equal(round(result_3$pred_at_opt, 3), 0.379, ignore_attr = TRUE)
})

test_that("find_integer_optimum incorporates tau_vec penalties correctly", {
  # Create a dummy model where CV decreases with Length and Width
  x <- data.frame(
    Length = c(1, 5, 10, 20, 1, 5, 10, 20),
    Width = c(1, 1, 1, 1, 5, 5, 5, 5),
    CV = c(0.5, 0.3, 0.2, 0.1, 0.4, 0.2, 0.1, 0.05)
  )
  fit_model <- lm(CV ~ Length + Width, data = x)

  nr_max <- 20
  nc_max <- 10

  h_star <- 10.5
  w_star <- 5.5

  # No penalty
  result_no_penalty <- find_integer_optimum(h_star, w_star, fit_model, c(0, 0), nr_max, nc_max)

  expect_equal(result_no_penalty$h_opt, 11)
  expect_equal(result_no_penalty$w_opt, 6)

  # With penalties favoring smaller h, w
  tau_vec_penalty <- c(0.01, 0.02) # Penalties that might shift the optimum
  result_with_penalty <- find_integer_optimum(
    h_star, w_star, fit_model, tau_vec_penalty, nr_max, nc_max
  )


  # Let's adjust tau to strongly favor smaller values and test
  tau_vec_strong_penalty <- c(0.05, 0.1)
  result_strong_penalty <- find_integer_optimum(
    h_star, w_star, fit_model, tau_vec_strong_penalty, nr_max, nc_max
  )

  expect_equal(result_strong_penalty$h_opt, 10)
  expect_equal(result_strong_penalty$w_opt, 5)
})

test_that("find_integer_optimum handles all candidates out of bounds", {
  x <- data.frame(Length = 1:2, Width = 1:2, CV = c(0.1, 0.2))
  fit_model <- lm(CV ~ Length + Width, data = x)
  tau_vec <- c(0.1, 0.1)

  # All candidates too high
  nr_max_s1 <- 5
  nc_max_s1 <- 5
  h_star_s1 <- 6.1
  w_star_s1 <- 6.1
  result_s1 <- find_integer_optimum(h_star_s1, w_star_s1, fit_model, tau_vec, nr_max_s1, nc_max_s1)
  expect_equal(result_s1$h_opt, NA_integer_)
  expect_equal(result_s1$w_opt, NA_integer_)
  expect_equal(result_s1$pred_at_opt, NA_real_)
})

test_that("find_integer_optimum handles non-finite values in tau_vec", {
  x <- data.frame(Length = 1:10, Width = 1:10, CV = runif(10))
  fit_model <- lm(CV ~ Length + Width, data = x)
  h_star <- 5.5
  w_star <- 5.5
  nr_max <- 10
  nc_max <- 10

  # Test with NA in tau_vec
  suppressWarnings(
    result_tau_na_1 <- find_integer_optimum(h_star, w_star, fit_model, c(NA, 0.1), nr_max, nc_max)
  )
  expect_equal(result_tau_na_1$h_opt, NA_integer_)
  expect_equal(result_tau_na_1$w_opt, NA_integer_)
  expect_equal(result_tau_na_1$pred_at_opt, NA_real_)

  suppressWarnings(
    result_tau_na_2 <- find_integer_optimum(h_star, w_star, fit_model, c(0.1, NA), nr_max, nc_max)
  )
  expect_equal(result_tau_na_2$h_opt, NA_integer_)
  expect_equal(result_tau_na_2$w_opt, NA_integer_)
  expect_equal(result_tau_na_2$pred_at_opt, NA_real_)

  # Test with Inf in tau_vec
  suppressWarnings(
    result_tau_inf_1 <- find_integer_optimum(h_star, w_star, fit_model, c(Inf, 0.1), nr_max, nc_max)
  )
  expect_equal(result_tau_inf_1$h_opt, NA_integer_)
  expect_equal(result_tau_inf_1$w_opt, NA_integer_)
  expect_equal(result_tau_inf_1$pred_at_opt, NA_real_)

  suppressWarnings(
    result_tau_inf_2 <- find_integer_optimum(h_star, w_star, fit_model, c(0.1, Inf), nr_max, nc_max)
  )
  expect_equal(result_tau_inf_2$h_opt, NA_integer_)
  expect_equal(result_tau_inf_2$w_opt, NA_integer_)
  expect_equal(result_tau_inf_2$pred_at_opt, NA_real_)

  # Test with NaN in tau_vec (though usually NA propagates to NaN)
  suppressWarnings(
    result_tau_nan_1 <- find_integer_optimum(h_star, w_star, fit_model, c(NaN, 0.1), nr_max, nc_max)
  )
  expect_equal(result_tau_nan_1$h_opt, NA_integer_)
  expect_equal(result_tau_nan_1$w_opt, NA_integer_)
  expect_equal(result_tau_nan_1$pred_at_opt, NA_real_)
})

test_that("find_integer_optimum works with a complex quadratic formula", {
  set.seed(420)
  x <- data.frame(
    Length = c(2, 5, 8, 2, 5, 8, 2, 5, 8),
    Width = c(2, 2, 2, 5, 5, 5, 8, 8, 8),
    CV = c(0.3, 0.2, 0.3, 0.2, 0.1, 0.2, 0.3, 0.2, 0.3) + rnorm(9, 0, 0.01) # Add some noise
  )
  # Fit the specified quadratic model
  fit_model_quadratic <- lm(CV ~ Length + Width + I(Length^2) + I(Width^2) + Length:Width, data = x)

  tau_vec <- c(0, 0)
  nr_max <- 10
  nc_max <- 10
  h_star <- 4.8
  w_star <- 5.2

  result_quadratic <- find_integer_optimum(
    h_star, w_star, fit_model_quadratic, tau_vec, nr_max, nc_max
  )
  expect_equal(result_quadratic$h_opt, 5)
  expect_equal(result_quadratic$w_opt, 5)
  expect_equal(round(result_quadratic$pred_at_opt, 3), 0.1, ignore_attr = TRUE)

  # Add another case with penalties
  tau_vec_quad_penalty <- c(0.01, 0.01)
  # Small penalties
  h_star_p <- 4.8
  w_star_p <- 5.2
  result_quadratic_penalty <- find_integer_optimum(
    h_star_p, w_star_p, fit_model_quadratic, tau_vec_quad_penalty, nr_max, nc_max
  )
  expect_equal(result_quadratic_penalty$h_opt, 5)
  expect_equal(result_quadratic_penalty$w_opt, 5)
  expect_equal(round(result_quadratic_penalty$pred_at_opt, 3), 0.1, ignore_attr = TRUE)
})

test_that("find_integer_optimum works with tau_vec = c(1, 1)", {
  x <- data.frame(
    Length = c(1, 5, 10, 20, 1, 5, 10, 20),
    Width = c(1, 1, 1, 1, 5, 5, 5, 5),
    CV = c(0.5, 0.3, 0.2, 0.1, 0.4, 0.2, 0.1, 0.05)
  )
  fit_model <- lm(CV ~ Length + Width, data = x)

  nr_max <- 20
  nc_max <- 10

  h_star <- 10.5
  w_star <- 5.5
  tau_vec_default <- c(1, 1)

  result_default_tau <- find_integer_optimum(
    h_star, w_star, fit_model, tau_vec_default, nr_max, nc_max
  )
  expect_equal(result_default_tau$h_opt, 10)
  expect_equal(result_default_tau$w_opt, 5)
  expect_equal(round(result_default_tau$pred_at_opt, 3), 0.170, ignore_attr = TRUE)
})

test_that("find_integer_optimum handles predict returning non-finite values for all candidates", {
  # Create a dummy model that, for all valid candidates, will lead to non-finite predictions.
  # A simple way is to fit a model to data where all response values are NA.
  # In such a case, lm might return coefficients as NA or predict will return NA.
  x_data_finite <- data.frame(Length = 1, Width = 1, CV = 0.1)
  fit_model_na_predict <- lm(CV ~ Length + Width, data = x_data_finite)
  fit_model_na_predict$coefficients[] <- NA_real_ # Force all coefficients to NA

  tau_vec <- c(0.1, 0.1)
  nr_max <- 10
  nc_max <- 10
  h_star <- 5.5
  w_star <- 5.5 # Ensure candidates are within bounds [1,10]

  suppressWarnings(
    result <- find_integer_optimum(h_star, w_star, fit_model_na_predict, tau_vec, nr_max, nc_max)
  )

  # Expect no optimum found if all predictions are non-finite
  expect_equal(result$h_opt, NA_integer_)
  expect_equal(result$w_opt, NA_integer_)
  expect_equal(result$pred_at_opt, NA_real_)
})
