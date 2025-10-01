#' Calculate 2D Cumulative Sum of a Matrix
#'
#' This function calculates the two-dimensional cumulative sum of a numeric matrix.
#' It first applies cumulative sum to each column, and then to each row of the resulting matrix.
#'
#' @param x A numeric matrix with all finite numbers. The matrix must have at least 2 rows.
#'   If the matrix has just 1 col, the result will be transposed (apply `t` after calculations).
#'
#' @return A numeric matrix representing the two-dimensional cumulative sum of `M`.
#'
#' @keywords internal
#'
cumulative_sum_matrix <- function(x) {
  t(matrix(apply(apply(x, 2, cumsum), 1, cumsum), nrow = ncol(x)))
}
