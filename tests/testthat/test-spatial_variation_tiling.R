### test structure

test_that("spatial_variation_tiling calculates variation correctly with default exclusion", {
  x <- matrix(rnorm(4, 500, 60), 2, 2)
  result <- spatial_variation_tiling(x)
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(colnames(result), c("Size", "Width", "Length", "plots", "Vx", "CV"))
  expect_true(all(result$Size <= nrow(x) * ncol(x)))
  # Check if result are ordered correctly
  expect_equal(result, result[order(result$Size, result$Length, result$Width), ])
})


### exclusion "smith"

test_that("2x2 matrix smith", {
  x <- matrix(rnorm(4, 500, 60), 2, 2)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "smith")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1),
    Width = c(1),
    Length = c(1),
    plots = c(x_dim)
  )
  expected$Vx <- c((sum(x^2) - (sum(x)^2 / x_dim)) / (x_dim - 1))
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("3x2 matrix exclusion='smith'", {
  x <- matrix(rnorm(6, 500, 60), 3, 2)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "smith")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1),
    Width = c(1),
    Length = c(1),
    plots = c(x_dim)
  )
  expected$Vx <- c((sum(x^2) - (sum(x)^2 / x_dim)) / (x_dim - 1))
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("2x3 matrix exclusion='smith'", {
  x <- matrix(rnorm(6, 500, 60), 2, 3)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "smith")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1),
    Width = c(1),
    Length = c(1),
    plots = c(x_dim)
  )
  expected$Vx <- c((sum(x^2) - (sum(x)^2 / x_dim)) / (x_dim - 1))
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("1x3 matrix exclusion='smith'", {
  x <- matrix(rnorm(3, 500, 60), 1, 3)
  result <- spatial_variation_tiling(x, exclusion = "smith")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expect_equal(result, data.frame())
})

test_that("3x1 matrix exclusion='smith'", {
  x <- matrix(rnorm(3, 500, 60), 3, 1)
  result <- spatial_variation_tiling(x, exclusion = "smith")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expect_equal(result, data.frame())
})

test_that("3x3 matrix exclusion='smith'", {
  x <- matrix(rnorm(9, 500, 60), 3, 3)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "smith")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1),
    Width = c(1),
    Length = c(1),
    plots = c(x_dim)
  )
  expected$Vx <- c((sum(x^2) - (sum(x)^2 / x_dim)) / (x_dim - 1))
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("4x4 matrix exclusion='smith'", {
  x <- matrix(rnorm(16, 500, 60), 4, 4)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "smith")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1, 2, 2, 4),
    Width = c(1, 1, 2, 2),
    Length = c(1, 2, 1, 2),
    plots = c(16, 8, 8, 4)
  )
  tc <- sum(x)^2 / x_dim
  expected$Vx <- c(
    (sum(x^2) - tc) / (x_dim - 1),
    (sum((c(
      sum(x[1:2, 1]), sum(x[1:2, 2]), sum(x[1:2, 3]), sum(x[1:2, 4]),
      sum(x[3:4, 1]), sum(x[3:4, 2]), sum(x[3:4, 3]), sum(x[3:4, 4])
    )^2) / 2) - tc) / (x_dim - 1),
    (sum((c(
      sum(x[1, 1:2]), sum(x[1, 3:4]),
      sum(x[2, 1:2]), sum(x[2, 3:4]),
      sum(x[3, 1:2]), sum(x[3, 3:4]),
      sum(x[4, 1:2]), sum(x[4, 3:4])
    )^2) / 2) - tc) / (x_dim - 1),
    (sum((c(
      sum(x[1:2, 1:2]), sum(x[1:2, 3:4]), sum(x[3:4, 1:2]), sum(x[3:4, 3:4])
    )^2) / 4) - tc) / (x_dim - 1)
  )
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("1x3 == 3x1 matrix exclusion='smith'", {
  values <- rnorm(30, 500, 60)
  x1 <- matrix(values, 1, 30)
  x2 <- matrix(values, 30, 1)
  result1 <- spatial_variation_tiling(x1, exclusion = "smith")$res
  result2 <- spatial_variation_tiling(x2, exclusion = "smith")$res
  expect_equal(result1, result2)
})


### exclusion "only_full"

test_that("2x2 matrix only_full", {
  x <- matrix(rnorm(4, 500, 60), 2, 2)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "only_full")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1, 2, 2),
    Width = c(1, 1, 2),
    Length = c(1, 2, 1),
    plots = c(4, 2, 2)
  )
  tc <- sum(x)^2 / x_dim
  expected$Vx <- c(
    (sum(x^2) - (sum(x)^2 / x_dim)) / (x_dim - 1),
    (sum((c(sum(x[1:2, 1]), sum(x[1:2, 2]))^2) / 2) - tc) / (x_dim - 1),
    (sum((c(sum(x[1, 1:2]), sum(x[2, 1:2]))^2) / 2) - tc) / (x_dim - 1)
  )
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("3x2 matrix exclusion='only_full'", {
  x <- matrix(rnorm(6, 500, 60), 3, 2)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "only_full")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1, 2, 3),
    Width = c(1, 2, 1),
    Length = c(1, 1, 3),
    plots = c(6, 3, 2)
  )
  tc <- sum(x)^2 / x_dim
  expected$Vx <- c(
    (sum(x^2) - (sum(x)^2 / x_dim)) / (x_dim - 1),
    (sum((c(sum(x[1, 1:2]), sum(x[2, 1:2]), sum(x[3, 1:2]))^2) / 2) - tc) / (x_dim - 1),
    (sum((c(sum(x[1:3, 1]), sum(x[1:3, 2]))^2) / 3) - tc) / (x_dim - 1)
  )
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("2x3 matrix exclusion='only_full'", {
  x <- matrix(rnorm(6, 500, 60), 2, 3)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "only_full")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1, 2, 3),
    Width = c(1, 1, 3),
    Length = c(1, 2, 1),
    plots = c(6, 3, 2)
  )
  tc <- sum(x)^2 / x_dim
  expected$Vx <- c(
    (sum(x^2) - (sum(x)^2 / x_dim)) / (x_dim - 1),
    (sum((c(sum(x[1:2, 1]), sum(x[1:2, 2]), sum(x[1:2, 3]))^2) / 2) - tc) / (x_dim - 1),
    (sum((c(sum(x[1, 1:3]), sum(x[2, 1:3]))^2) / 3) - tc) / (x_dim - 1)
  )
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("1x3 matrix exclusion='only_full'", {
  x <- matrix(rnorm(3, 500, 60), 1, 3)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "only_full")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1),
    Width = c(1),
    Length = c(1),
    plots = c(x_dim)
  )
  expected$Vx <- c((sum(x^2) - (sum(x)^2 / x_dim)) / (x_dim - 1))
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("3x1 matrix exclusion='only_full'", {
  x <- matrix(rnorm(3, 500, 60), 3, 1)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "only_full")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1),
    Width = c(1),
    Length = c(1),
    plots = c(x_dim)
  )
  expected$Vx <- c((sum(x^2) - (sum(x)^2 / x_dim)) / (x_dim - 1))
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("3x3 matrix exclusion='only_full'", {
  x <- matrix(rnorm(9, 500, 60), 3, 3)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "only_full")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1, 3, 3),
    Width = c(1, 1, 3),
    Length = c(1, 3, 1),
    plots = c(9, 3, 3)
  )
  tc <- sum(x)^2 / x_dim
  expected$Vx <- c(
    (sum(x^2) - (sum(x)^2 / x_dim)) / (x_dim - 1),
    (sum((c(sum(x[1:3, 1]), sum(x[1:3, 2]), sum(x[1:3, 3]))^2) / 3) - tc) / (x_dim - 1),
    (sum((c(sum(x[1, 1:3]), sum(x[2, 1:3]), sum(x[3, 1:3]))^2) / 3) - tc) / (x_dim - 1)
  )
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})

test_that("4x4 matrix exclusion='only_full'", {
  x <- matrix(rnorm(16, 500, 60), 4, 4)
  x_dim <- nrow(x) * ncol(x)
  result <- spatial_variation_tiling(x, exclusion = "only_full")
  expect_s3_class(result, "sv_tiling")
  result <- result$res
  expected <- data.frame(
    Size = c(1, 2, 2, 4, 4, 4, 8, 8),
    Width = c(1, 1, 2, 1, 2, 4, 2, 4),
    Length = c(1, 2, 1, 4, 2, 1, 4, 2),
    plots = c(16, 8, 8, 4, 4, 4, 2, 2)
  )
  tc <- sum(x)^2 / x_dim
  expected$Vx <- c(
    (sum(x^2) - tc) / (x_dim - 1),
    (sum((c(
      sum(x[1:2, 1]), sum(x[1:2, 2]), sum(x[1:2, 3]), sum(x[1:2, 4]),
      sum(x[3:4, 1]), sum(x[3:4, 2]), sum(x[3:4, 3]), sum(x[3:4, 4])
    )^2) / 2) - tc) / (x_dim - 1),
    (sum((c(
      sum(x[1, 1:2]), sum(x[1, 3:4]),
      sum(x[2, 1:2]), sum(x[2, 3:4]),
      sum(x[3, 1:2]), sum(x[3, 3:4]),
      sum(x[4, 1:2]), sum(x[4, 3:4])
    )^2) / 2) - tc) / (x_dim - 1),
    (sum((c(
      sum(x[1:4, 1]), sum(x[1:4, 2]), sum(x[1:4, 3]), sum(x[1:4, 4])
    )^2) / 4) - tc) / (x_dim - 1),
    (sum((c(
      sum(x[1:2, 1:2]), sum(x[1:2, 3:4]), sum(x[3:4, 1:2]), sum(x[3:4, 3:4])
    )^2) / 4) - tc) / (x_dim - 1),
    (sum((c(
      sum(x[1, 1:4]), sum(x[2, 1:4]), sum(x[3, 1:4]), sum(x[4, 1:4])
    )^2) / 4) - tc) / (x_dim - 1),
    (sum((c(sum(x[1:4, 1:2]), sum(x[1:4, 3:4]))^2) / 8) - tc) / (x_dim - 1),
    (sum((c(sum(x[1:2, 1:4]), sum(x[3:4, 1:4]))^2) / 8) - tc) / (x_dim - 1)
  )
  expected$CV <- sqrt(expected$Vx) * 100 / mean(x)
  rownames(expected) <- paste(expected$Length, expected$Width, sep = "x")
  expect_equal(result, expected)
})


### return sums

test_that("2x2 return sums", {
  x <- matrix(rnorm(4, 500, 60), 2, 2)
  result <- spatial_variation_tiling(x, exclusion = "smith", return_sums = TRUE)$sums
  expected <- data.frame(
    r = c(1, 1, 2, 2),
    c = c(1, 2, 1, 2),
    h = c(1, 1, 1, 1),
    w = c(1, 1, 1, 1),
    sum = as.vector(t(x))
  )
  expect_equal(result, expected)
})
