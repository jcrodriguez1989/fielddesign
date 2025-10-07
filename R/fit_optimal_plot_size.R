#' Calculate Optimal Plot Size from Spatial Variation Data
#'
#' This function fits a quadratic model to spatial variation data (e.g., Coefficient of Variation)
#' obtained from exhaustive sliding window analysis, and then determines the optimal plot size
#' (height and width) based on the fitted model and a penalty (`tau`). It first attempts an
#' analytical solution, and if that produces negative or out-of-bounds values, falls back to
#' constrained numerical optimization.
#'
#' @param sv A data frame, typically the output of `spatial_variation_exhaustive`, containing
#'   `Length`, `Width`, and `CV` columns.
#' @param nr The number of rows of the original input matrix from which `sv` was derived.
#' @param nc The number of columns of the original input matrix from which `sv` was derived.
#' @param include_interaction A logical value. If `TRUE` (default), the quadratic model includes an
#'   interaction term between `Length` and `Width`.
#' @param tau A numeric vector of length 2, representing the penalty for deviations from an ideal
#'   square shape `c(tau_height, tau_width)` in the optimization.
#'
#' @return A list containing the fitted model, coefficients, Hessian matrix, gradient, `tau`
#'   values, continuous optimal `h_star` and `w_star`, integer optimal `h_opt` and `w_opt`,
#'   predicted CV at continuous and integer optima, the `include_interaction` flag, and
#'   `method_used` indicating whether "analytical" or "constrained" optimization was used.
#'
#' @importFrom stats as.formula lm
#'
#' @examples
#' \dontrun{
#' # Generate simulated data.
#' set.seed(420)
#' nr <- 10
#' nc <- 10
#' x <- matrix(rnorm(nr * nc, 500, 60), nrow = nr, ncol = nc)
#' # Calculate the exhaustive spatial variation.
#' sv_exh <- spatial_variation_exhaustive(x)$res
#' # Calculate the optimal plot size.
#' exh_ops <- fit_optimal_plot_size(sv_exh, nr, nc)
#' print(exh_ops)
#' # Calculate the tiling spatial variation.
#' sv_tiling <- spatial_variation_tiling(x)$res
#' # Calculate the optimal plot size.
#' tiling_ops <- fit_optimal_plot_size(sv_tiling, nr, nc)
#' print(tiling_ops)
#' }
#'
#' @export
#'
fit_optimal_plot_size <- function(sv, nr, nc, include_interaction = TRUE,
                                  tau = c(1, 1)) {
  form <- if (include_interaction) {
    as.formula("CV ~ Length + Width + I(Length^2) + I(Width^2) + Length:Width")
  } else {
    as.formula("CV ~ Length + Width + I(Length^2) + I(Width^2)")
  }
  fit <- lm(form, data = sv)
  # Try analytical solution first.
  analytical_result <- solve_analytical(fit, tau)
  h_star <- analytical_result$h_star
  w_star <- analytical_result$w_star
  method_used <- "analytical"
  # Check if analytical solution is valid (positive and within bounds).
  invalid_solution <- !is.finite(h_star) || !is.finite(w_star) || h_star < 1 || w_star < 1 ||
    h_star > nr || w_star > nc
  if (invalid_solution) {
    # Fall back to constrained optimization.
    message("Analytical solution invalid or out of bounds. Using constrained optimization...")
    constrained_result <- solve_constrained(fit, tau, nr, nc)
    h_star <- constrained_result$h_star
    w_star <- constrained_result$w_star
    method_used <- "constrained"
  }
  invalid_solution <- !is.finite(h_star) || !is.finite(w_star) || h_star < 1 || w_star < 1 ||
    h_star > nr || w_star > nc
  if (invalid_solution) {
    warning("Couldn't calculate optimal plot size")
    return(list(
      fit = fit, h_star = NA_real_, w_star = NA_real_, h_opt = NA_real_, w_opt = NA_real_,
      method_used = NA_character_
    ))
  }
  # Find integer optimum.
  opt_int_results <- find_integer_optimum(h_star, w_star, fit, tau, nr, nc)
  h_opt <- opt_int_results$h_opt
  w_opt <- opt_int_results$w_opt
  list(
    fit = fit, h_star = h_star, w_star = w_star, h_opt = h_opt, w_opt = w_opt,
    method_used = method_used
  )
}
