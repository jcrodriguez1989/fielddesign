test_that("sliding_window_sum handles basic 2x2 window on a 3x3 matrix", {
  x <- matrix(1:9, nrow = 3, byrow = TRUE)
  # 1 2 3
  # 4 5 6
  # 7 8 9
  # Expected sums for h=2, w=2:
  # Window 1 (top-left): 1+2+4+5 = 12
  # Window 2 (top-right): 2+3+5+6 = 16
  # Window 3 (bottom-left): 4+5+7+8 = 24
  # Window 4 (bottom-right): 5+6+8+9 = 28
  expected_sums <- c(12, 16, 24, 28)
  expect_equal(sliding_window_sum(x, h = 2, w = 2, combine_orientations = FALSE)$res, expected_sums)
})

test_that("sliding_window_sum handles 1x1 window", {
  x <- matrix(1:4, nrow = 2)
  expect_equal(sliding_window_sum(x, h = 1, w = 1, combine_orientations = FALSE)$res, c(1, 3, 2, 4))
})

test_that("sliding_window_sum handles window dimensions larger than matrix", {
  x <- matrix(1:4, nrow = 2)
  expect_null(sliding_window_sum(x, h = 3, w = 3, combine_orientations = FALSE)$res)
  expect_null(sliding_window_sum(x, h = 1, w = 3, combine_orientations = FALSE)$res)
  expect_null(sliding_window_sum(x, h = 3, w = 1, combine_orientations = FALSE)$res)
})

test_that("sliding_window_sum handles combine_orientations = TRUE when h != w", {
  x <- matrix(1:6, nrow = 2, byrow = TRUE) # 2x3 matrix
  # 1 2 3
  # 4 5 6

  # h=1, w=2 windows:
  # (1,1)-(1,2) = 1+2 = 3
  # (1,2)-(1,3) = 2+3 = 5
  # (2,1)-(2,2) = 4+5 = 9
  # (2,2)-(2,3) = 5+6 = 11
  expected_h1w2 <- c(3, 5, 9, 11)

  # For combine_orientations=TRUE, also consider w=1, h=2 (i.e., h=2, w=1 for the function call)
  # h=2, w=1 windows:
  # (1,1)-(2,1) = 1+4 = 5
  # (1,2)-(2,2) = 2+5 = 7
  # (1,3)-(2,3) = 3+6 = 9
  expected_h2w1 <- c(5, 7, 9)

  result_only_h1w2 <- sliding_window_sum(x, h = 1, w = 2, combine_orientations = FALSE)$res
  # Explicitly get w x h sums
  result_only_h2w1 <- sliding_window_sum(x, h = 2, w = 1, combine_orientations = FALSE)$res
  result_combined <- sliding_window_sum(x, h = 1, w = 2, combine_orientations = TRUE)$res

  expect_equal(result_only_h1w2, expected_h1w2)
  expect_equal(result_only_h2w1, expected_h2w1)
  expect_equal(result_combined, c(expected_h1w2, expected_h2w1))
})

test_that("sliding_window_sum handles combine_orientations = TRUE when h == w", {
  x <- matrix(1:9, nrow = 3, byrow = TRUE)
  # With h=2, w=2, combine_orientations should not add duplicate results.
  expected_sums_h2w2 <- c(12, 16, 24, 28) # From first test case
  expect_equal(
    sliding_window_sum(x, h = 2, w = 2, combine_orientations = TRUE)$res, expected_sums_h2w2
  )
})

test_that("sliding_window_sum handles data frame input (coercible to matrix)", {
  df <- data.frame(a = 1:3, b = 4:6)
  # h=2, w=2 on a 3x2 df. Only one window possible.
  # 1 4
  # 2 5
  # 3 6
  # Window: (1,1)-(2,2) = 1+4+2+5 = 12
  # For combine_orientations=TRUE, also h=2, w=2 which is the same, so no change.
  expect_equal(sliding_window_sum(df, h = 2, w = 2, combine_orientations = FALSE)$res, c(12, 16))
  expect_equal(sliding_window_sum(df, h = 2, w = 2, combine_orientations = TRUE)$res, c(12, 16))
})

test_that("sliding_window_sum check sums 3x3", {
  x <- matrix(1:9, nrow = 3)
  # H = 1, W = 1
  result <- sliding_window_sum(x, h = 1, w = 1, combine_orientations = FALSE, return_sums = TRUE)
  expected <- data.frame(
    r = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    c = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    h = 1,
    w = 1,
    sum = c(1, 4, 7, 2, 5, 8, 3, 6, 9)
  )
  expect_equal(result$sums, expected)
  result <- sliding_window_sum(x, h = 1, w = 1, combine_orientations = TRUE, return_sums = TRUE)
  expect_equal(result$sums, expected)
  # H = 2, W = 1
  result <- sliding_window_sum(x, h = 2, w = 1, combine_orientations = FALSE, return_sums = TRUE)
  expected <- data.frame(
    r = c(1, 1, 1, 2, 2, 2),
    c = c(1, 2, 3, 1, 2, 3),
    h = 2,
    w = 1,
    sum = c(3, 9, 15, 5, 11, 17)
  )
  expect_equal(result$sums, expected)
  result <- sliding_window_sum(x, h = 2, w = 1, combine_orientations = TRUE, return_sums = TRUE)
  expected <- data.frame(
    r = c(1, 1, 1, 2, 2, 2, 1, 1, 2, 2, 3, 3),
    c = c(1, 2, 3, 1, 2, 3, 1, 2, 1, 2, 1, 2),
    h = c(2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1),
    w = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    sum = c(3, 9, 15, 5, 11, 17, 5, 11, 7, 13, 9, 15)
  )
  expect_equal(result$sums, expected)
  # H = 1, W = 2
  result <- sliding_window_sum(x, h = 1, w = 2, combine_orientations = FALSE, return_sums = TRUE)
  expected <- data.frame(
    r = c(1, 1, 2, 2, 3, 3),
    c = c(1, 2, 1, 2, 1, 2),
    h = 1,
    w = 2,
    sum = c(5, 11, 7, 13, 9, 15)
  )
  expect_equal(result$sums, expected)
  result <- sliding_window_sum(x, h = 1, w = 2, combine_orientations = TRUE, return_sums = TRUE)
  expected <- data.frame(
    r = c(1, 1, 2, 2, 3, 3, 1, 1, 1, 2, 2, 2),
    c = c(1, 2, 1, 2, 1, 2, 1, 2, 3, 1, 2, 3),
    h = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    w = c(2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1),
    sum = c(5, 11, 7, 13, 9, 15, 3, 9, 15, 5, 11, 17)
  )
  expect_equal(result$sums, expected)
  # H = 2, W = 2
  result <- sliding_window_sum(x, h = 2, w = 2, combine_orientations = FALSE, return_sums = TRUE)
  expected <- data.frame(
    r = c(1, 1, 2, 2),
    c = c(1, 2, 1, 2),
    h = 2,
    w = 2,
    sum = c(12, 24, 16, 28)
  )
  expect_equal(result$sums, expected)
  result <- sliding_window_sum(x, h = 2, w = 2, combine_orientations = TRUE, return_sums = TRUE)
  expect_equal(result$sums, expected)
})
