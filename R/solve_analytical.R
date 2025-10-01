#' Calculate Optimal Plot Size Using Analytical Solution
#'
#' @param fit Fitted linear model object
#' @param tau Numeric vector of length 2 with penalty terms
#'
#' @return List with h_star, w_star, and success status
#'
#' @importFrom stats coef
#'
#' @keywords internal
#'
solve_analytical <- function(fit, tau) {
  b <- coef(fit)
  bh <- coef_or_0(b, "Length")
  bw <- coef_or_0(b, "Width")
  bh2 <- coef_or_0(b, "I(Length^2)")
  bw2 <- coef_or_0(b, "I(Width^2)")
  bhw <- coef_or_0(b, "Length:Width")
  h <- matrix(c(2 * bh2, bhw, bhw, 2 * bw2), 2, 2)
  g <- c(bh, bw)
  deth <- determinant(h, logarithm = FALSE)$modulus
  h_star <- NA_real_
  w_star <- NA_real_
  if (is.finite(deth) && deth >= .Machine$double.eps^0.5) {
    sol <- -solve(h, g + tau)
    h_star <- sol[[1]]
    w_star <- sol[[2]]
  }
  list(h_star = h_star, w_star = w_star)
}
