#' Calculate the Sum of a Rectangular Region Using an Integral Image
#'
#' This function computes the sum of a rectangular region within a matrix `x`, assuming `x` is an
#' integral image (also known as a summed-area table).
#' The sum is calculated efficiently using four lookups: `A - B - C + D`,
#' where `A` is `x[r2, c2]`, `B` is `x[r-1, c2]` (if `r > 1`), `C` is `x[r2, c-1]` (if `c > 1`),
#' and `D` is `x[r-1, c-1]` (if `r > 1` and `c > 1`).
#'
#' @param x A numeric matrix representing an integral image. It is assumed that `x[i,j]` contains
#'   the sum of all elements in the original matrix from `(1,1)` to `(i,j)`.
#' @param r The row index of the top-left corner of the desired region (1-indexed).
#' @param c The column index of the top-left corner of the desired region (1-indexed).
#' @param h The height of the desired region.
#' @param w The width of the desired region.
#'
#' @return A numeric scalar representing the sum of the elements in the specified rectangular
#'   region of the original (non-integral) image.
#'
#' @keywords internal
#'
integral_image_sum <- function(x, r, c, h, w) {
  r2 <- r + h - 1
  c2 <- c + w - 1
  upper_a <- x[r2, c2]
  upper_b <- if (r > 1) x[r - 1, c2] else 0
  upper_c <- if (c > 1) x[r2, c - 1] else 0
  upper_d <- if (r > 1 && c > 1) x[r - 1, c - 1] else 0
  upper_a - upper_b - upper_c + upper_d
}
