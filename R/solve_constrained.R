#' Calculate Optimal Plot Size Using Constrained Optimization
#'
#' @param fit Fitted linear model object
#' @param tau Numeric vector of length 2 with penalty terms
#' @param nr Number of rows (max height)
#' @param nc Number of columns (max width)
#'
#' @return List with h_star, w_star, and optimization details
#'
#' @importFrom stats optim predict
#'
#' @keywords internal
#'
solve_constrained <- function(fit, tau, nr, nc) {
  # Objective function: predicted CV + penalty
  objective_fn <- function(x) {
    pred <- predict(fit, newdata = data.frame(Length = x[1], Width = x[2]))
    # Match the analytical formulation.
    penalty <- tau[1] * x[1] + tau[2] * x[2]
    as.numeric(pred + penalty)
  }
  # Try optimization from multiple starting points to avoid local minima
  starting_points <- list(
    c(nr / 2, nc / 2), # center
    c(sqrt(nr * nc), sqrt(nr * nc)), # geometric mean (square)
    c(nr / 3, nc / 3), # lower third
    c(2 * nr / 3, 2 * nc / 3) # upper third
  )
  best_result <- NULL
  best_value <- Inf
  for (start in starting_points) {
    tryCatch(
      {
        opt_result <- optim(
          par = start,
          fn = objective_fn,
          method = "L-BFGS-B",
          lower = c(1, 1),
          upper = c(nr, nc)
        )
        if (opt_result$value < best_value) {
          best_value <- opt_result$value
          best_result <- opt_result
        }
      },
      error = function(e) {}
    )
  }
  if (is.null(best_result)) {
    return(list(h_star = NA_real_, w_star = NA_real_))
  }
  list(h_star = best_result$par[1], w_star = best_result$par[2])
}
