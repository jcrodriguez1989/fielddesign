#' Calculate Sliding Window Sums Over a Matrix
#'
#' This function computes the sum of elements within sliding rectangular windows over a numeric
#' matrix.
#' It leverages an integral image (cumulative sum matrix) for efficient calculation.
#' Optionally, it can combine results for both `h x w` and `w x h` window orientations.
#'
#' @param x A numeric matrix with all finite numbers.
#' @param h The height (number of rows) of the sliding window.
#' @param w The width (number of columns) of the sliding window.
#' @param combine_orientations A logical value. If `TRUE` (default), the function will also
#'   calculate sums for windows of size `w x h` if `h` and `w` are different, and append them to
#'   the results. If `FALSE`, only `h x w` windows are considered.
#' @param return_sums A logical value. If `TRUE`, the function includes a data.frame (`sums`)
#'   detailing the position and size of each window's sum, in addition to the vector of sums
#'   (`res`). If `FALSE` (default), only the vector of sums (`res`) is returned.
#'
#' @return A numeric vector containing the sums of all specified sliding windows.
#'
#' @keywords internal
#'
sliding_window_sum <- function(x, h, w, combine_orientations = TRUE, return_sums = FALSE) {
  nr <- nrow(x)
  nc <- ncol(x)
  csm <- cumulative_sum_matrix(x)
  sums <- c()
  sums_df <- data.frame(
    r = integer(), c = integer(), h = integer(), w = integer(), sum = numeric()
  )
  if (h <= nr && w <= nc) {
    nrw <- nr - h + 1
    ncw <- nc - w + 1
    if (nrw > 0 && ncw > 0) {
      out <- numeric(nrw * ncw)
      k <- 1
      for (r in seq_len(nrw)) {
        for (c in seq_len(ncw)) {
          out[k] <- sum <- integral_image_sum(csm, r, c, h, w)
          sums_df <- rbind(sums_df, data.frame(r, c, h, w, sum))
          k <- k + 1
        }
      }
      sums <- c(sums, out)
    }
  }
  if (combine_orientations && h != w && w <= nr && h <= nc) {
    nrw2 <- nr - w + 1
    ncw2 <- nc - h + 1
    if (nrw2 > 0 && ncw2 > 0) {
      out2 <- numeric(nrw2 * ncw2)
      k <- 1
      for (r in seq_len(nrw2)) {
        for (c in seq_len(ncw2)) {
          out2[k] <- sum <- integral_image_sum(csm, r, c, w, h)
          sums_df <- rbind(sums_df, data.frame(r, c, h = w, w = h, sum))
          k <- k + 1
        }
      }
      sums <- c(sums, out2)
    }
  }
  res <- list(res = sums)
  if (return_sums) {
    res$sums <- sums_df
  }
  res
}
