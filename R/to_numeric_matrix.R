#' Convert to a Numeric Matrix
#'
#' @param x An R object, typically a matrix or data.frame, whose elements can be coerced to numeric.
#'
#' @return A numeric matrix.
#'
#' @keywords internal
#'
to_numeric_matrix <- function(x) {
  if (is.matrix(x) && nrow(x) > 0 && ncol(x) > 0) {
    return(matrix(apply(x, 2, as.numeric), nrow = nrow(x), ncol = ncol(x)))
  }
  as.matrix(data.frame(lapply(x, as.numeric)))
}
