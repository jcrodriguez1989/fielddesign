test_that("cumulative_sum_matrix works with basic cases", {
  # Test case 1: simple 2x2 matrix
  mat1 <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
  expected1 <- matrix(c(1, 3, 4, 10), nrow = 2, byrow = TRUE)
  expect_equal(cumulative_sum_matrix(mat1), expected1)

  # Test case 2: 3x3 matrix
  mat2 <- matrix(1:9, nrow = 3, byrow = TRUE)
  expected2 <- matrix(c(
    1,   3,  6,
    5,  12, 21,
    12, 27, 45
  ), nrow = 3, byrow = TRUE)
  expect_equal(cumulative_sum_matrix(mat2), expected2)

  # Test case 3: matrix with negative numbers
  mat3 <- matrix(c(-1, -2, -3, -4), nrow = 2, byrow = TRUE)
  expected3 <- matrix(c(-1, -3, -4, -10), nrow = 2, byrow = TRUE)
  expect_equal(cumulative_sum_matrix(mat3), expected3)
})

test_that("cumulative_sum_matrix handles edge cases", {
  # Test case 5: row vector matrix
  mat5 <- matrix(c(1, 2, 3), nrow = 1, byrow = TRUE)
  expected5 <- matrix(c(1, 3, 6), nrow = 1, byrow = TRUE)
  expect_error(cumulative_sum_matrix(mat5), "must have a positive length")
  expect_equal(t(cumulative_sum_matrix(t(mat5))), expected5)

  # Test case 6: column vector matrix
  mat6 <- matrix(c(1, 2, 3), ncol = 1)
  expected6 <- matrix(c(1, 3, 6), ncol = 1)
  expect_equal(cumulative_sum_matrix(mat6), expected6)
})
