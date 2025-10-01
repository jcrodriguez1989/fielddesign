#' Find Optimal Integer Plot Dimensions
#'
#' This helper function identifies the best integer height and width for a plot based on a
#' continuous optimum and a fitted model. It considers candidate integer values around the
#' continuous optimum and evaluates an objective function (predicted CV plus penalties) to find the
#' integer pair that minimizes it.
#'
#' @param h_star The continuous optimal height from a quadratic model.
#' @param w_star The continuous optimal width from a quadratic model.
#' @param fit_model An `lm` object, the fitted model used to predict CV values.
#' @param tau_vec A numeric vector of length 2, representing the penalty for deviations from an
#'   ideal square shape `c(tau_height, tau_width)`.
#' @param nr_max The maximum possible height (number of rows in the original matrix).
#' @param nc_max The maximum possible width (number of columns in the original matrix).
#'
#' @return A list containing:
#'   \itemize{
#'     \item `h_opt`: The optimal integer height.
#'     \item `w_opt`: The optimal integer width.
#'     \item `pred_at_opt`: The unpenalized predicted CV at the integer optimum.
#'   }
#'
#' @importFrom stats predict
#'
#' @keywords internal
#'
find_integer_optimum <- function(h_star, w_star, fit_model, tau_vec, nr_max, nc_max) {
  if (!all(is.finite(c(h_star, w_star)))) {
    return(list(h_opt = NA_integer_, w_opt = NA_integer_, pred_at_opt = NA_real_))
  }
  h_candidates <- unique(c(floor(h_star), ceiling(h_star)))
  w_candidates <- unique(c(floor(w_star), ceiling(w_star)))
  best_h_opt <- NA_integer_
  best_w_opt <- NA_integer_
  min_objective_value <- Inf
  best_pred_cv_unpenalized <- NA_real_
  for (h_c in h_candidates) {
    for (w_c in w_candidates) {
      # Ensure candidates are within practical bounds (1 to nr_max/nc_max)
      if (h_c >= 1 && w_c >= 1 && h_c <= nr_max && w_c <= nc_max) {
        newdata_point <- data.frame(Length = h_c, Width = w_c)
        pred_cv_unpenalized <- predict(fit_model, newdata = newdata_point)
        if (is.finite(pred_cv_unpenalized)) {
          current_objective_value <- pred_cv_unpenalized + tau_vec[1] * h_c + tau_vec[2] * w_c
          if (is.finite(current_objective_value) && current_objective_value < min_objective_value) {
            min_objective_value <- current_objective_value
            best_h_opt <- h_c
            best_w_opt <- w_c
            best_pred_cv_unpenalized <- pred_cv_unpenalized
          }
        }
      }
    }
  }
  list(h_opt = best_h_opt, w_opt = best_w_opt, pred_at_opt = best_pred_cv_unpenalized)
}
