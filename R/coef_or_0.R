#' Safely Extract Coefficient
#'
#' This is a helper function to safely extract a coefficient from a named numeric vector.
#' If the specified `name` is found in the vector `b`, its value is returned.
#' Otherwise, it returns 0. It also handles non-finite values by converting them to 0.
#'
#' @param b A named numeric vector, typically coefficients from a linear model.
#' @param name A character string, the name of the coefficient to extract.
#'
#' @return A numeric value, either the extracted coefficient or 0 if not found or non-finite.
#'
#' @keywords internal
#'
coef_or_0 <- function(b, name) {
  val <- if (name %in% names(b)) b[[name]] else 0
  if (!is.finite(val)) 0 else val
}
