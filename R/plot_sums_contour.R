#' Plot Contour of Sums
#'
#' This function generates a contour (tile) plot of sums based on spatial variation data obtained
#' from `spatial_variation_exhaustive` or `spatial_variation_tiling`.
#' It visualizes the sum values for a specific `width` and `length` combination.
#'
#' @param sv An object of class `sv`, created by `spatial_variation_exhaustive` or
#'   `spatial_variation_tiling` with parameter `return_sums = TRUE`.
#' @param width A numeric value specifying the width to filter the `sv` object.
#'   Must correspond to an existing width in `sv$res` and `sv$sums`.
#' @param length A numeric value specifying the length to filter the `sv` object.
#'   Must correspond to an existing length in `sv$res` and `sv$sums`
#'
#' @return A `ggplot` object representing the contour plot of sums, or `NULL` invisibly if no data
#'   matches the provided `width` and `length`, along with a message.
#'
#' @examples
#' \dontrun{
#' # Generate simulated data.
#' set.seed(420)
#' nr <- 10
#' nc <- 10
#' x <- matrix(rnorm(nr * nc, 500, 60), nrow = nr, ncol = nc)
#' # Calculate the exhaustive spatial variation.
#' sv_exh <- spatial_variation_exhaustive(x, return_sums = TRUE)
#' plot_sums_contour(sv_exh, 2, 1)
#' # Calculate the tiling spatial variation.
#' sv_tiling <- spatial_variation_tiling(x, return_sums = TRUE)
#' plot_sums_contour(sv_tiling, 2, 1)
#' }
#'
#' @importFrom ggplot2 aes coord_fixed geom_tile ggplot labs scale_fill_viridis_c scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous theme_minimal
#' @importFrom methods is
#'
#' @export
#'
plot_sums_contour <- function(sv, width = NULL, length = NULL) {
  if (!(is(sv, "sv_tiling") || is(sv, "sv_exhaustive"))) {
    stop("sv must be created by `spatial_variation_exhaustive` or `spatial_variation_tiling`")
  }
  if (!"sums" %in% names(sv)) {
    stop("`spatial_variation_` function must be executed with parameter `return_sums = TRUE`")
  }
  cv <- sv$res[sv$res$Width == width & sv$res$Length == length, ]
  sums <- sv$sums[sv$sums$w == width & sv$sums$h == length, ]
  # For exhaustive, it's the same `width*length` and `length*width`.
  if (is(sv, "sv_exhaustive") && nrow(cv) == 0) {
    cv <- sv$res[sv$res$Width == length & sv$res$Length == width, ]
  }
  sums_t <- sv$sums[sv$sums$w == length & sv$sums$h == width, ]
  if (is(sv, "sv_exhaustive") && nrow(sums_t) > 0 && width != length) {
    message("The CV calculation also took into account W=", length, ", L=", width)
  }
  if (nrow(sums) == 0) {
    sums <- unique(sv$sums[, c("w", "h")])
    message(
      "Please set `(width, length)` parameters as one of: ",
      paste0("(", paste(
        apply(sums[order(sums$w, sums$h), ], 1, function(row) paste(row, collapse = ", ")),
        collapse = ") - ("
      ), ")")
    )
    return(invisible())
  }
  ggplot(sums, aes(c, r, fill = sum)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Sum") +
    labs(
      title = paste0("W=", cv$Width, ", L=", cv$Length, ", CV = ", round(cv$CV, 2)),
      x = "Width", y = "Length"
    ) +
    scale_x_continuous(breaks = unique(sums$c)) +
    scale_y_continuous(breaks = unique(sums$r), transform = "reverse") +
    coord_fixed(expand = FALSE) +
    theme_minimal(base_size = 12)
}
