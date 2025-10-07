#' Calculate Exhaustive Spatial Variation for a Matrix
#'
#' This function computes spatial variation metrics (e.g., coefficient of variation) for all
#' possible rectangular sliding windows over a numeric matrix.
#' It iterates through various window heights and widths, calculates sums using
#' `sliding_window_sum`, and then derives the coefficient of variation from these sums.
#'
#' @param x A numeric matrix or an object coercible to a numeric matrix.
#' @param combine_orientations A logical value. If `TRUE`, the function considers both
#'   `rows x cols` and `cols x rows` window orientations.
#' @param max_area An optional numeric value specifying the maximum area of a window
#'   to consider. If `NULL` (default), the maximum area is `rows * cols`.
#' @param return_sums A logical value. If `TRUE`, the function includes a data.frame (`sums`)
#'   detailing the position and size of each window's sum, in addition to the vector of sums
#'   (`res`). If `FALSE` (default), only the vector of sums (`res`) is returned.
#'
#' @return A list containing the data frame (`res`) containing spatial variation metrics for each
#'   window size, including Length, Width, Size (area), number of windows, and Coefficient of
#'   Variation (CV). The results are ordered by Size, then Length, then Width.
#'
#' @examples
#' # Create a sample matrix
#' set.seed(420)
#' rows <- 7
#' cols <- 13
#' sample_matrix <- matrix(rnorm(rows * cols, 500, 60), nrow = rows, ncol = cols)
#'
#' # Calculate exhaustive spatial variation
#' sv_results <- spatial_variation_exhaustive(sample_matrix)
#' print(sv_results)
#'
#' # Calculate with specific max_area and no orientation combining
#' sv_results_subset <- spatial_variation_exhaustive(
#'   sample_matrix,
#'   combine_orientations = FALSE, max_area = 20
#' )
#' print(sv_results_subset)
#'
#' @export
#'
spatial_variation_exhaustive <- function(x, combine_orientations = TRUE, max_area = NULL,
                                         return_sums = FALSE) {
  x <- to_numeric_matrix(x)
  if (any(!is.finite(x))) {
    stop("There are non-finite values in your input matrix")
  }
  if (nrow(x) == 1) {
    x <- t(x)
    warning("Transposing the input matrix to improve calculations", call. = FALSE)
  }
  nr <- nrow(x)
  nc <- ncol(x)
  max_area <- min(max_area, nr * nc)
  tab <- lapply(seq_len(nr), function(h) {
    w_max_area <- floor(max_area / h)
    w_limit <- min(max(nr, nc), w_max_area)
    w_start <- if (combine_orientations) h else 1
    if (w_limit < w_start) {
      return()
    }
    lapply(w_start:w_limit, function(w) {
      feasible <- (h <= nr && w <= nc) || (w <= nr && h <= nc)
      if (!feasible) {
        return()
      }
      sums <- sliding_window_sum(x, h, w, combine_orientations, return_sums)
      sums_df <- sums$sums
      sums <- sums$res
      if (length(sums) < 2) {
        return()
      }
      list(res = data.frame(
        Size = h * w,
        Width = w,
        Length = h,
        plots = length(sums),
        CV = coefficient_of_variation(sums)
      ), sums = sums_df)
    })
  })
  sums_df <- do.call(rbind, lapply(tab, function(x) do.call(rbind, lapply(x, function(y) y$sums))))
  tab <- do.call(rbind, lapply(tab, function(x) do.call(rbind, lapply(x, function(y) y$res))))
  if (is.null(tab)) {
    res <- list(res = data.frame())
    class(res) <- "sv_exhaustive"
    return(res)
  }
  rownames(tab) <- paste(tab$Length, tab$Width, sep = "x")
  res <- list(res = tab[order(tab$Size, tab$Length, tab$Width), ])
  if (return_sums) {
    res$sums <- sums_df
  }
  class(res) <- "sv_exhaustive"
  res
}
