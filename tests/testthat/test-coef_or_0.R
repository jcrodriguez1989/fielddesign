test_that("coef_or_0 extracts existing finite coefficient", {
  b <- c(a = 1, b = 2, c = 3)
  expect_equal(coef_or_0(b, "b"), 2)
  expect_equal(coef_or_0(b, "a"), 1)
})

test_that("coef_or_0 returns 0 for non-existing coefficient", {
  b <- c(a = 1, b = 2, c = 3)
  expect_equal(coef_or_0(b, "d"), 0)
})

test_that("coef_or_0 returns 0 for NA coefficient", {
  b <- c(a = 1, b = NA_real_, c = 3)
  expect_equal(coef_or_0(b, "b"), 0)
})

test_that("coef_or_0 returns 0 for Inf coefficient", {
  b <- c(a = 1, b = Inf, c = 3)
  expect_equal(coef_or_0(b, "b"), 0)
})

test_that("coef_or_0 returns 0 for -Inf coefficient", {
  b <- c(a = 1, b = -Inf, c = 3)
  expect_equal(coef_or_0(b, "b"), 0)
})

test_that("coef_or_0 handles empty vector b", {
  b <- numeric(0)
  expect_equal(coef_or_0(b, "anything"), 0)
})

test_that("coef_or_0 handles vector with only one element", {
  b <- c(alpha = 10)
  expect_equal(coef_or_0(b, "alpha"), 10)
  expect_equal(coef_or_0(b, "beta"), 0)
})
