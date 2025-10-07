#' Plot Yield Contour
#'
#' Generates a filled contour plot of yield data from a numeric matrix. The function visualizes
#' yield values across a 2D grid (Length and Width).
#'
#' @param x A numeric matrix representing yield data. Rows are treated as 'Length' and columns as
#'   'Width', with cell values being the 'Value'.
#'
#' @return A `ggplot` object displaying the filled contour plot.
#'
#' @examples
#' \dontrun{
#' # Generate simulated data.
#' set.seed(420)
#' nr <- 10
#' nc <- 10
#' x <- matrix(rnorm(nr * nc, 500, 60), nrow = nr, ncol = nc)
#' # Plot.
#' plot_yield_contour(x)
#' }
#'
#' @importFrom ggplot2 aes coord_fixed geom_tile ggplot scale_fill_viridis_c scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous theme_minimal
#'
#' @export
#'
plot_yield_contour <- function(x) {
  if (!is.matrix(x)) {
    stop("x must be a numeric matrix")
  }
  x <- as.data.frame(as.table(x))
  names(x) <- c("Length", "Width", "Value")
  x$Length <- as.numeric(x$Length)
  x$Width <- as.numeric(x$Width)
  ggplot(x, aes(Width, Length, fill = Value)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Value") +
    scale_x_continuous(breaks = unique(x$Width)) +
    scale_y_continuous(breaks = unique(x$Length), transform = "reverse") +
    coord_fixed(expand = FALSE) +
    theme_minimal(base_size = 12)
}
