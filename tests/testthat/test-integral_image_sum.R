test_that("integral_image_sum calculates sum for a central region correctly", {
  m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
  ii <- cumulative_sum_matrix(m)
  # Sum of region M[2,2]
  expect_equal(integral_image_sum(ii, r = 2, c = 2, h = 1, w = 1), 5)
  # Sum of region M[2:3, 2:3]
  expect_equal(integral_image_sum(ii, r = 2, c = 2, h = 2, w = 2), sum(m[2:3, 2:3]))
})

test_that("integral_image_sum calculates sum for top-left region correctly (r=1, c=1)", {
  m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
  ii <- cumulative_sum_matrix(m)
  # Sum of region M[1,1]
  expect_equal(integral_image_sum(ii, r = 1, c = 1, h = 1, w = 1), 1)
  # Sum of region M[1:2, 1:2]
  expect_equal(integral_image_sum(ii, r = 1, c = 1, h = 2, w = 2), sum(m[1:2, 1:2]))
})

test_that("integral_image_sum calculates sum for top row region correctly (r=1, c>1)", {
  m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
  ii <- cumulative_sum_matrix(m)
  # Sum of region M[1,2]
  expect_equal(integral_image_sum(ii, r = 1, c = 2, h = 1, w = 1), 2)
  # Sum of region M[1:2, 2:3]
  expect_equal(integral_image_sum(ii, r = 1, c = 2, h = 2, w = 2), sum(m[1:2, 2:3]))
})

test_that("integral_image_sum calculates sum for left column region correctly (r>1, c=1)", {
  m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
  ii <- cumulative_sum_matrix(m)
  # Sum of region M[2,1]
  expect_equal(integral_image_sum(ii, r = 2, c = 1, h = 1, w = 1), 4)
  # Sum of region M[2:3, 1:2]
  expect_equal(integral_image_sum(ii, r = 2, c = 1, h = 2, w = 2), sum(m[2:3, 1:2]))
})

test_that("integral_image_sum calculates sum for entire matrix correctly", {
  m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
  ii <- cumulative_sum_matrix(m)
  expect_equal(integral_image_sum(ii, r = 1, c = 1, h = 3, w = 3), sum(m))
})

test_that("integral_image_sum handles single-row matrix correctly", {
  m <- matrix(c(10, 20, 30), nrow = 1, byrow = TRUE)
  ii <- t(cumulative_sum_matrix(t(m)))
  expect_equal(integral_image_sum(ii, r = 1, c = 1, h = 1, w = 1), 10)
  expect_equal(integral_image_sum(ii, r = 1, c = 2, h = 1, w = 1), 20)
  expect_equal(integral_image_sum(ii, r = 1, c = 1, h = 1, w = 3), sum(m))
})

test_that("integral_image_sum handles single-column matrix correctly", {
  m <- matrix(c(10, 20, 30), ncol = 1, byrow = FALSE)
  ii <- cumulative_sum_matrix(m)
  expect_equal(integral_image_sum(ii, r = 1, c = 1, h = 1, w = 1), 10)
  expect_equal(integral_image_sum(ii, r = 2, c = 1, h = 1, w = 1), 20)
  expect_equal(integral_image_sum(ii, r = 1, c = 1, h = 3, w = 1), sum(m))
})

test_that("integral_image_sum handles integral image of zeros", {
  m <- matrix(0, nrow = 3, ncol = 3)
  ii <- cumulative_sum_matrix(m)
  expect_equal(integral_image_sum(ii, r = 1, c = 1, h = 3, w = 3), 0)
  expect_equal(integral_image_sum(ii, r = 2, c = 2, h = 1, w = 1), 0)
})

test_that("integral_image_sum handles big matrix", {
  m <- matrix(rnorm(1200, 500, 60), nrow = 40, ncol = 30)
  ii <- cumulative_sum_matrix(m)
  expect_equal(integral_image_sum(ii, r = 20, c = 25, h = 1, w = 1), m[20, 25])
  expect_equal(
    integral_image_sum(ii, r = 20, c = 25, h = 2, w = 2),
    m[20, 25] + m[21, 25] + m[20, 26] + m[21, 26]
  )
  expect_equal(integral_image_sum(ii, r = 1, c = 1, h = 40, w = 30), sum(m))
})
