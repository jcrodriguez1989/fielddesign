#' Plot Contour of Predicted Coefficient of Variation (CV)
#'
#' This function generates a filled contour plot visualizing the predicted Coefficient of Variation
#' (CV) across a grid of Length and Width values, based on a provided fitted model. It can
#' optionally highlight specific points on the contour.
#'
#' @param fit A fitted model object (e.g., from `fit_exhaustive_optimal_plot_size`) that can
#'   predict CV values.
#' @param nr An integer, specifying the maximum number of rows (Length) for the grid.
#' @param nc An integer, specifying the maximum number of columns (Width) for the grid.
#' @param title A character string for the main title of the plot.
#' @param xlab A character string for the x-axis label.
#' @param ylab A character string for the y-axis label.
#' @param mark A data frame with at least 'Length' and 'Width' columns, specifying points to mark
#'   on the plot.
#' @param mark_col A character string, the color to use for the marked points.
#' @param mark_lab A character string, a label to display near the first marked point.
#'
#' @return A `ggplot` object, representing the generated CV contour plot.
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
#' exh_ops_int <- fit_exhaustive_optimal_plot_size(sv_exh, nr, nc)
#' # Plot.
#' plot_cv_contour(
#'   exh_ops_int$fit,
#'   nr = nr, nc = nc,
#'   title = "EXHAUSTIVE — CV Contour (With interaction)",
#'   mark = data.frame(Length = exh_ops_int$h_opt, Width = exh_ops_int$w_opt),
#'   mark_col = "red",
#'   mark_lab = paste0("optimal (L=", exh_ops_int$h_opt, ", W=", exh_ops_int$w_opt, ")")
#' )
#' }
#'
#' @importFrom ggplot2 aes annotate coord_fixed geom_contour_filled geom_point ggplot labs
#' @importFrom ggplot2 scale_fill_viridis_d theme_minimal
#' @importFrom stats predict
#'
#' @export
#'
plot_cv_contour <- function(fit, nr, nc, title = "CV Contour",
                            xlab = "Width (number of plots)",
                            ylab = "Length",
                            mark = NULL, mark_col = "red", mark_lab = NULL) {
  grid_df <- expand.grid(Length = seq_len(nr), Width = seq_len(nc))
  grid_df$CV_pred <- as.numeric(predict(fit, newdata = grid_df))
  grid_df <- subset(grid_df, is.finite(CV_pred))
  p <- ggplot(grid_df, aes(Width, Length, z = CV_pred)) +
    geom_contour_filled(bins = 12) +
    scale_fill_viridis_d(name = "CV pred") +
    coord_fixed(xlim = c(1, nr), ylim = c(1, nc), expand = FALSE) +
    theme_minimal(base_size = 12) +
    labs(title = title, x = xlab, y = ylab)
  if (!is.null(mark)) {
    p <- p +
      geom_point(
        data = mark, aes(Width, Length),
        color = mark_col, size = 3, inherit.aes = FALSE
      )
    if (!is.null(mark_lab)) {
      p <- p + annotate("text",
        x = mark$Length[[1]], y = mark$Width[[1]],
        label = mark_lab, vjust = -1, color = mark_col
      )
    }
  }
  p
}
