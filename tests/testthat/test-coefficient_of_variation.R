test_that("coefficient_of_variation calculates correctly for positive values", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(coefficient_of_variation(x), 52.70463, tolerance = 1e-6)
})

test_that("coefficient_of_variation handles vectors with mean close to zero", {
  x_zero_mean <- c(-2, -1, 0, 1, 2)
  expect_equal(coefficient_of_variation(x_zero_mean), Inf)
})

test_that("coefficient_of_variation handles vectors with sd close to zero", {
  x_zero_sd <- rep(4, 10)
  expect_equal(coefficient_of_variation(x_zero_sd), 0)
})

test_that("coefficient_of_variation handles vectors with both mean and sd close to zero", {
  x_zero_stats <- rep(0, 10)
  expect_equal(coefficient_of_variation(x_zero_stats), NaN)
})

test_that("coefficient_of_variation handles negative and mixed values correctly", {
  x_mixed <- c(-5, 0, 5, 10, 15)
  expect_equal(coefficient_of_variation(x_mixed), 158.1139, tolerance = 1e-6)
})

test_that("coefficient_of_variation propagates NA values", {
  x_na <- c(1, 2, NA, 4, 5)
  expect_true(is.na(coefficient_of_variation(x_na)))
})
