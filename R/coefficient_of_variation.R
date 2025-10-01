#' Calculate the Coefficient of Variation
#'
#' This function computes the coefficient of variation (CV) for a numeric vector.
#' The CV is a measure of relative variability, expressed as the ratio of the standard deviation to
#' the mean.
#'
#' @param x A numeric vector.
#'
#' @return A numeric scalar representing the coefficient of variation.
#'
#' @importFrom stats sd
#'
#' @keywords internal
#'
coefficient_of_variation <- function(x) {
  100 * (sd(x) / mean(x))
}
