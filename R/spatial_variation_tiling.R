#' Calculate Spatial Variation Using a Tiling Method
#'
#' This function computes spatial variation metrics for a numeric matrix by dividing it into blocks
#' of various sizes (tiling). It calculates the coefficient of variation (CV) for each block size
#' based on the method described by Smith (1938) or by considering only full blocks.
#'
#' @param x A numeric matrix or an object coercible to a numeric matrix.
#' @param exclusion A character string specifying the exclusion method for block sizes.
#'   Can be `"smith"` (default), which excludes any block whose height `L` equals the matrix height
#'   `n` or whose width `W` equals the matrix width `m`. This means only blocks where `L < n` and
#'   `W < m` are considered.
#'   The alternative is `"only_full"`, which explicitly excludes only the case where block height
#'   `L` equals `n` and block width `W` equals `m` (i.e., the full matrix itself). All other
#'   blocks, including those of full height or full width, are considered.
#' @param return_sums A logical value. If `TRUE`, the function includes a data.frame (`sums`)
#'   detailing the position and size of each window's sum, in addition to the vector of sums
#'   (`res`). If `FALSE` (default), only the vector of sums (`res`) is returned.
#'
#' @return A list containing the data frame (`res`) containing spatial variation metrics for each
#'   tiling combination, including Length, Width, Size (area), plots (number of blocks),
#'   V (variance estimate), and C.V. (Coefficient of Variation).
#'
#' @references Smith, H. F. (1938). An empirical law describing heterogeneity in the yields of
#'  agricultural crops. Journal of Agricultural Science, 28(1), 1-23.
#'
#' @examples
#' # Create a sample matrix
#' set.seed(123)
#' rows <- 7
#' cols <- 13
#' sample_matrix <- matrix(rnorm(rows * cols, 500, 60), nrow = rows, ncol = cols)
#'
#' # Calculate spatial variation using tiling (Smith's method)
#' sv_results <- spatial_variation_tiling(sample_matrix)
#' print(sv_results)
#'
#' # Calculate spatial variation using tiling (excluding only full field)
#' sv_results_full <- spatial_variation_tiling(sample_matrix, exclusion = "only_full")
#' print(sv_results_full)
#'
#' @export
#'
spatial_variation_tiling <- function(x, exclusion = c("smith", "only_full"), return_sums = FALSE) {
  exclusion <- match.arg(exclusion)
  x <- to_numeric_matrix(x)
  n <- nrow(x)
  m <- ncol(x)
  tc <- sum(x)^2 / (n * m)
  x_mean <- mean(x)
  r_div <- (1:n)[n %% (1:n) == 0]
  c_div <- (1:m)[m %% (1:m) == 0]
  if (exclusion == "smith") {
    if (length(r_div) > 0) r_div <- r_div[-length(r_div)]
    if (length(c_div) > 0) c_div <- c_div[-length(c_div)]
  }
  # Create all combinations of h and w.
  hw_combinations <- expand.grid(h = r_div, w = c_div)
  # Filter out full matrix if excluding only_full matrix.
  if (exclusion == "only_full") {
    hw_combinations <- hw_combinations[!(hw_combinations$h == n & hw_combinations$w == m), ]
  }
  sums_df <- data.frame(
    r = integer(), c = integer(), h = integer(), w = integer(), sum = numeric()
  )
  res <- lapply(seq_len(nrow(hw_combinations)), function(idx) {
    h <- hw_combinations$h[idx]
    w <- hw_combinations$w[idx]
    block_sums <- lapply(seq_len(n / h), function(bi) {
      r_top <- (bi - 1) * h + 1
      r_bot <- r_top + h - 1
      lapply(seq_len(m / w), function(bj) {
        c_left <- (bj - 1) * w + 1
        c_right <- c_left + w - 1
        bsum <- sum(x[r_top:r_bot, c_left:c_right])
        list(
          sum_data = data.frame(
            r = r_top, c = c_left, h = h, w = w, sum = bsum
          ),
          ss_contrib = (bsum^2) / (w * h)
        )
      })
    })
    # Flatten and extract block sums
    all_blocks <- unlist(block_sums, recursive = FALSE)
    block_sum_dfs <- lapply(all_blocks, function(b) b$sum_data)
    ss_contribs <- sapply(all_blocks, function(b) b$ss_contrib)
    ss <- sum(ss_contribs)
    v <- (ss - tc) / (n * m - 1)
    cv <- 100 * sqrt(v) / x_mean
    list(
      result = data.frame(
        Size = h * w, Width = w, Length = h,
        plots = (n * m) / (h * w), Vx = v, CV = cv
      ),
      sums = do.call(rbind, block_sum_dfs)
    )
  })
  # Separate results and sums
  sums_df <- do.call(rbind, lapply(res, function(r) r$sums))
  res <- lapply(res, function(r) r$result)
  if (length(res) == 0) {
    return(list(res = data.frame()))
  }
  out <- do.call(rbind, res)
  rownames(out) <- paste(out$Length, out$Width, sep = "x")
  res <- list(res = out[order(out$Size, out$Width, out$Length), ])
  if (return_sums) {
    res$sums <- sums_df
  }
  res
}
