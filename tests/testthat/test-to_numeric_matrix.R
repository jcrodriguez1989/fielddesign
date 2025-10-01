test_that("to_numeric_matrix converts data.frame to numeric matrix correctly", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c("4", "5", "6"),
    c = c(7.1, 8.2, 9.3)
  )
  expected_matrix <- matrix(c(1, 2, 3, 4, 5, 6, 7.1, 8.2, 9.3), ncol = 3, byrow = FALSE)
  colnames(expected_matrix) <- c("a", "b", "c")
  result <- to_numeric_matrix(df)
  expect_true(is.matrix(result))
  expect_true(is.numeric(result))
  expect_equal(result, expected_matrix)
})

test_that("to_numeric_matrix handles existing numeric matrix correctly", {
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
  expected_mat <- mat
  result <- to_numeric_matrix(mat)
  expect_true(is.matrix(result))
  expect_true(is.numeric(result))
  expect_equal(result, expected_mat)
})

test_that("to_numeric_matrix handles character matrix correctly", {
  char_mat <- matrix(c("1", "2", "3", "4"), nrow = 2, byrow = TRUE)
  expected_mat <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
  result <- to_numeric_matrix(char_mat)
  expect_true(is.matrix(result))
  expect_true(is.numeric(result))
  expect_equal(result, expected_mat)
})

test_that("to_numeric_matrix handles mixed types and NA", {
  df_mixed <- data.frame(
    a = c(1, NA, 3),
    b = c("4", "five", "6")
  )
  # When converting "five" to numeric, it becomes NA.
  expected_matrix_mixed <- matrix(c(1, NA, 3, 4, NA, 6), ncol = 2, byrow = FALSE)
  colnames(expected_matrix_mixed) <- c("a", "b")
  result <- suppressWarnings(to_numeric_matrix(df_mixed))
  expect_true(is.matrix(result))
  expect_true(is.numeric(result))
  expect_equal(result, expected_matrix_mixed)
})

test_that("to_numeric_matrix handles empty data.frame", {
  df_empty <- data.frame()
  result <- to_numeric_matrix(df_empty)
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(0, 0))
})

test_that("to_numeric_matrix handles empty matrix", {
  mat_empty <- matrix(numeric(0), nrow = 0, ncol = 0)
  result <- to_numeric_matrix(mat_empty)
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(0, 0))
})

test_that("to_numeric_matrix preserves column names for data.frame input", {
  df <- data.frame(colA = c(1, 2), colB = c(3, 4))
  result <- to_numeric_matrix(df)
  expect_equal(colnames(result), c("colA", "colB"))
})
