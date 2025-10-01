### test structure

test_that("function handles basic matrix input and returns expected structure", {
  x <- matrix(rnorm(4, 500, 60), 2, 2)
  result <- spatial_variation_exhaustive(x)$res
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("Length", "Width", "Size", "plots", "CV") %in% names(result)))
  expect_true(all(result$Size <= nrow(x) * ncol(x)))
  expect_equal(result, result[order(result$Size, result$Length, result$Width), ])
})


### combine_orientations FALSE

test_that("2x2 matrix combine_orientations=FALSE", {
  x <- matrix(rnorm(4, 500, 60), 2, 2)
  result <- spatial_variation_exhaustive(x, combine_orientations = FALSE)$res
  expected <- data.frame(
    Size = c(1, 2, 2),
    Width = c(1, 2, 1),
    Length = c(1, 1, 2),
    plots = c(4, 2, 2)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(rowSums(x)),
    coefficient_of_variation(colSums(x))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("3x2 matrix combine_orientations=FALSE", {
  x <- matrix(rnorm(6, 500, 60), 3, 2)
  result <- spatial_variation_exhaustive(x, combine_orientations = FALSE)$res
  expected <- data.frame(
    Size = c(1, 2, 2, 3, 4),
    Width = c(1, 2, 1, 1, 2),
    Length = c(1, 1, 2, 3, 2),
    plots = c(6, 3, 4, 2, 2)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(rowSums(x)),
    coefficient_of_variation(c(sum(x[1:2, 1]), sum(x[1:2, 2]), sum(x[2:3, 1]), sum(x[2:3, 2]))),
    coefficient_of_variation(colSums(x)),
    coefficient_of_variation(c(sum(x[1:2, 1:2]), sum(x[2:3, 1:2])))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("2x3 matrix combine_orientations=FALSE", {
  x <- matrix(rnorm(6, 500, 60), 2, 3)
  result <- spatial_variation_exhaustive(x, combine_orientations = FALSE)$res
  expected <- data.frame(
    Size = c(1, 2, 2, 3, 4),
    Width = c(1, 2, 1, 3, 2),
    Length = c(1, 1, 2, 1, 2),
    plots = c(6, 4, 3, 2, 2)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(c(sum(x[1, 1:2]), sum(x[2, 1:2]), sum(x[1, 2:3]), sum(x[2, 2:3]))),
    coefficient_of_variation(c(sum(x[1:2, 1]), sum(x[1:2, 2]), sum(x[1:2, 3]))),
    coefficient_of_variation(rowSums(x)),
    coefficient_of_variation(c(sum(x[1:2, 1:2]), sum(x[1:2, 2:3])))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("1x3 matrix combine_orientations=FALSE", {
  x <- matrix(rnorm(3, 500, 60), 1, 3)
  expect_warning(
    result <- spatial_variation_exhaustive(x, combine_orientations = FALSE)$res,
    "Transposing the input matrix to improve calculations"
  )
  expected <- data.frame(
    Size = c(1, 2),
    Width = c(1, 1),
    Length = c(1, 2),
    plots = c(3, 2)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(c(sum(x[1, 1:2]), sum(x[1, 2:3])))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("3x1 matrix combine_orientations=FALSE", {
  x <- matrix(rnorm(3, 500, 60), 3, 1)
  result <- spatial_variation_exhaustive(x, combine_orientations = FALSE)$res
  expected <- data.frame(
    Size = c(1, 2),
    Width = c(1, 1),
    Length = c(1, 2),
    plots = c(3, 2)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(c(sum(x[1:2, 1]), sum(x[2:3, 1])))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("3x3 matrix combine_orientations=FALSE", {
  x <- matrix(rnorm(9, 500, 60), 3, 3)
  result <- spatial_variation_exhaustive(x, combine_orientations = FALSE)$res
  expected <- data.frame(
    Size = c(1, 2, 2, 3, 3, 4, 6, 6),
    Width = c(1, 2, 1, 3, 1, 2, 3, 2),
    Length = c(1, 1, 2, 1, 3, 2, 2, 3),
    plots = c(9, 6, 6, 3, 3, 4, 2, 2)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(c(
      sum(x[1, 1:2]), sum(x[1, 2:3]),
      sum(x[2, 1:2]), sum(x[2, 2:3]),
      sum(x[3, 1:2]), sum(x[3, 2:3])
    )),
    coefficient_of_variation(c(
      sum(x[1:2, 1]), sum(x[2:3, 1]),
      sum(x[1:2, 2]), sum(x[2:3, 2]),
      sum(x[1:2, 3]), sum(x[2:3, 3])
    )),
    coefficient_of_variation(rowSums(x)),
    coefficient_of_variation(colSums(x)),
    coefficient_of_variation(c(
      sum(x[1:2, 1:2]), sum(x[1:2, 2:3]), sum(x[2:3, 1:2]), sum(x[2:3, 2:3])
    )),
    coefficient_of_variation(c(sum(x[1:2, 1:3]), sum(x[2:3, 1:3]))),
    coefficient_of_variation(c(sum(x[1:3, 1:2]), sum(x[1:3, 2:3])))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("1x3 == 3x1 matrix combine_orientations=FALSE", {
  values <- rnorm(30, 500, 60)
  x1 <- matrix(values, 1, 30)
  x2 <- matrix(values, 30, 1)
  expect_warning(
    result1 <- spatial_variation_exhaustive(x1, combine_orientations = FALSE)$res,
    "Transposing the input matrix to improve calculations"
  )
  result2 <- spatial_variation_exhaustive(x2, combine_orientations = FALSE)$res
  expect_equal(result1, result2)
})


### combine_orientations TRUE

test_that("2x2 matrix combine_orientations=TRUE", {
  x <- matrix(rnorm(4, 500, 60), 2, 2)
  result <- spatial_variation_exhaustive(x, combine_orientations = TRUE)$res
  expected <- data.frame(
    Size = c(1, 2),
    Width = c(1, 2),
    Length = c(1, 1),
    plots = c(4, 4)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(c(rowSums(x), colSums(x)))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("3x2 matrix combine_orientations=TRUE", {
  x <- matrix(rnorm(6, 500, 60), 3, 2)
  result <- spatial_variation_exhaustive(x, combine_orientations = TRUE)$res
  expected <- data.frame(
    Size = c(1, 2, 3, 4),
    Width = c(1, 2, 3, 2),
    Length = c(1, 1, 1, 2),
    plots = c(6, 7, 2, 2)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(c(
      rowSums(x), sum(x[1:2, 1]), sum(x[2:3, 1]), sum(x[1:2, 2]), sum(x[2:3, 2])
    )),
    coefficient_of_variation(colSums(x)),
    coefficient_of_variation(c(sum(x[1:2, 1:2]), sum(x[2:3, 1:2])))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("2x3 matrix combine_orientations=TRUE", {
  x <- matrix(rnorm(6, 500, 60), 2, 3)
  result <- spatial_variation_exhaustive(x, combine_orientations = TRUE)$res
  expected <- data.frame(
    Size = c(1, 2, 3, 4),
    Width = c(1, 2, 3, 2),
    Length = c(1, 1, 1, 2),
    plots = c(6, 7, 2, 2)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(c(
      colSums(x), sum(x[1, 1:2]), sum(x[1, 2:3]), sum(x[2, 1:2]), sum(x[2, 2:3])
    )),
    coefficient_of_variation(rowSums(x)),
    coefficient_of_variation(c(sum(x[1:2, 1:2]), sum(x[1:2, 2:3])))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("1x3 matrix combine_orientations=TRUE", {
  x <- matrix(rnorm(3, 500, 60), 1, 3)
  expect_warning(
    result <- spatial_variation_exhaustive(x, combine_orientations = TRUE)$res,
    "Transposing the input matrix to improve calculations"
  )
  expected <- data.frame(
    Size = c(1, 2),
    Width = c(1, 2),
    Length = c(1, 1),
    plots = c(3, 2)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(c(sum(x[1, 1:2]), sum(x[1, 2:3])))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("3x1 matrix combine_orientations=TRUE", {
  x <- matrix(rnorm(3, 500, 60), 3, 1)
  result <- spatial_variation_exhaustive(x, combine_orientations = TRUE)$res
  expected <- data.frame(
    Size = c(1, 2),
    Width = c(1, 2),
    Length = c(1, 1),
    plots = c(3, 2)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(c(sum(x[1:2, 1]), sum(x[2:3, 1])))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("3x3 matrix combine_orientations=TRUE", {
  x <- matrix(rnorm(9, 500, 60), 3, 3)
  result <- spatial_variation_exhaustive(x, combine_orientations = TRUE)$res
  expected <- data.frame(
    Size = c(1, 2, 3, 4, 6),
    Width = c(1, 2, 3, 2, 3),
    Length = c(1, 1, 1, 2, 2),
    plots = c(9, 12, 6, 4, 4)
  )
  expected$CV <- c(
    coefficient_of_variation(x),
    coefficient_of_variation(c(
      sum(x[1, 1:2]), sum(x[1, 2:3]),
      sum(x[2, 1:2]), sum(x[2, 2:3]),
      sum(x[3, 1:2]), sum(x[3, 2:3]),
      sum(x[1:2, 1]), sum(x[2:3, 1]),
      sum(x[1:2, 2]), sum(x[2:3, 2]),
      sum(x[1:2, 3]), sum(x[2:3, 3])
    )),
    coefficient_of_variation(c(rowSums(x), colSums(x))),
    coefficient_of_variation(c(
      sum(x[1:2, 1:2]), sum(x[1:2, 2:3]), sum(x[2:3, 1:2]), sum(x[2:3, 2:3])
    )),
    coefficient_of_variation(c(
      sum(x[1:2, 1:3]), sum(x[2:3, 1:3]), sum(x[1:3, 1:2]), sum(x[1:3, 2:3])
    ))
  )
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("1x3 == 3x1 matrix combine_orientations=TRUE", {
  values <- rnorm(30, 500, 60)
  x1 <- matrix(values, 1, 30)
  x2 <- matrix(values, 30, 1)
  expect_warning(
    result1 <- spatial_variation_exhaustive(x1, combine_orientations = TRUE)$res,
    "Transposing the input matrix to improve calculations"
  )
  result2 <- spatial_variation_exhaustive(x2, combine_orientations = TRUE)$res
  expect_equal(result1, result2)
})


### extras

test_that("max_area parameter limits window sizes", {
  x <- matrix(rnorm(25, 500, 60), 5, 5)
  max_area_limit <- 10
  result <- spatial_variation_exhaustive(x, max_area = max_area_limit)$res
  expect_true(all(result$Size <= max_area_limit))
  expect_true(all(result$Length * result$Width <= max_area_limit))
})

test_that("function stops with non-finite values in matrix", {
  expect_error(
    spatial_variation_exhaustive(matrix(c(1, 2, NA, 4), 2, 2))$res,
    "There are non-finite values in your input matrix"
  )
  expect_error(
    spatial_variation_exhaustive(matrix(c(1, 2, Inf, 4), 2, 2))$res,
    "There are non-finite values in your input matrix"
  )
  expect_error(
    expect_warning(spatial_variation_exhaustive(matrix(c(1, 2, "a", 4), 2, 2)))$res,
    "There are non-finite values in your input matrix"
  )
})

test_that("function warns and transposes for single row matrix, and correctly processes windows", {
  single_row_matrix_1x5 <- matrix(1:5, nrow = 1)
  # Test combine_orientations = FALSE
  result_1x5_false <- suppressWarnings(spatial_variation_exhaustive(
    single_row_matrix_1x5,
    combine_orientations = FALSE
  )$res)
  expect_warning(
    spatial_variation_exhaustive(single_row_matrix_1x5, combine_orientations = FALSE)$res,
    "Transposing the input matrix to improve calculations"
  )
  # After transposition (5x1 matrix), and combine_orientations=FALSE, only 1-column widths are
  # considered.
  expect_true(all(result_1x5_false$Width == 1))
  expect_equal(sort(unique(result_1x5_false$Length)), 1:4)
  # Test combine_orientations = TRUE
  result_1x5_true <- suppressWarnings(spatial_variation_exhaustive(
    single_row_matrix_1x5,
    combine_orientations = TRUE
  )$res)
  expect_warning(
    spatial_variation_exhaustive(single_row_matrix_1x5, combine_orientations = TRUE)$res,
    "Transposing the input matrix to improve calculations"
  )
  # For a 1x5 original (becomes 5x1 internal), combine_orientations=TRUE should yield H=1, W=1..5
  expect_true(all(result_1x5_true$Length == 1))
  expect_equal(sort(unique(result_1x5_true$Width)), 1:4)
  # For (Length=1, Width=2) on a 5x1 matrix with combine_orientations=TRUE, it should effectively
  # calculate 2x1 windows.
  # Number of 2x1 windows on a 5x1 matrix is (5-2+1)*(1-1+1) = 4*1 = 4.
  expect_equal(
    result_1x5_true[result_1x5_true$Length == 1 & result_1x5_true$Width == 2, "plots"], 4
  )
})

test_that("empty matrix or single element matrix returns empty data frame", {
  empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 0)
  result_empty <- spatial_variation_exhaustive(empty_matrix)$res
  expect_s3_class(result_empty, "data.frame")
  expect_equal(nrow(result_empty), 0)
})

test_that("matrix with all same values has CV of 0", {
  matrix_same_values <- matrix(500, nrow = 3, ncol = 3)
  result <- spatial_variation_exhaustive(matrix_same_values)$res
  # Expect CV to be 0 or very close to 0 due to floating point arithmetic
  expect_true(all(result$CV < .Machine$double.eps^0.5))
})

test_that("2x2 return sums", {
  x <- matrix(rnorm(4, 500, 60), 2, 2)
  result <- spatial_variation_exhaustive(x, return_sums = TRUE)$sums
  expected <- data.frame(
    r = c(1, 1, 2, 2, 1, 2, 1, 1),
    c = c(1, 2, 1, 2, 1, 1, 1, 2),
    h = c(1, 1, 1, 1, 1, 1, 2, 2),
    w = c(1, 1, 1, 1, 2, 2, 1, 1),
    sum = c(as.vector(t(x)), rowSums(x), colSums(x))
  )
  expect_equal(result, expected)
})
