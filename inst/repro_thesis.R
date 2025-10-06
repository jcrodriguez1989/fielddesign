## =========================================================
##  TAMANO DE UE: EXHAUSTIVO (ventanas) vs TILING (agricolae)
##  - Exhaustivo: ventanas inside, SUMAS; CV clasico; grad(CV) = -tau
##  - Tiling: divisores exactos; CV clasico; modo de exclusion configurable
## =========================================================

library("fielddesign")

## ---------------- Parametros ----------------
ruta_blanco <- "blanco.txt" # si no existe, se simula
tau_vec <- c(1, 1) # grad(CV) = -tau ; usa c(1,1) para penalizacion unitaria
combine_orientations_exh <- TRUE # TRUE: 4x1 ≡ 1x4; 2x2 incluido
max_area_exh <- NULL # NULL => hasta nr*nc

## TILING: modo de exclusion
##   "smith": replica agricolae::index.smith (excluye L=n y/o W=m)
##   "only_full": solo excluye el bloque L=n & W=m
tiling_exclusion_mode <- "smith" # "smith" o "only_full"

make_plots <- TRUE

## =========================================================
##  1) Carga de datos
## =========================================================

if (file.exists(ruta_blanco)) {
  blanco <- as.matrix(read.table(ruta_blanco))
  message("Se cargo '", ruta_blanco, "' con dimensiones: ", nrow(blanco), " x ", ncol(blanco))
} else {
  set.seed(123)
  blanco <- matrix(rnorm(100, 500, 60), nrow = 10, ncol = 10) # ejemplo 10x10
  message(
    "No se encontro '", ruta_blanco, "'. Se genero matriz simulada ",
    nrow(blanco), " x ", ncol(blanco)
  )
}
nr <- nrow(blanco)
nc <- ncol(blanco)
message(sprintf("Dimensiones detectadas del ensayo: %d x %d", nr, nc))

## =========================================================
##  2) EXHAUSTIVO: tabla y ajustes
## =========================================================
tab_exh <- spatial_variation_exhaustive(
  blanco,
  combine_orientations = combine_orientations_exh,
  max_area = max_area_exh
)$res
message("\n[EXHAUSTIVO] Primeras filas:")
print(head(tab_exh, 10))

res_exh_int <- fit_exhaustive_optimal_plot_size(
  tab_exh, nr, nc,
  include_interaction = TRUE, tau = tau_vec
)
res_exh_no_int <- fit_exhaustive_optimal_plot_size(
  tab_exh, nr, nc,
  include_interaction = FALSE, tau = tau_vec
)

if (inherits(res_exh_no_int$fit, "lm") && inherits(res_exh_int$fit, "lm")) {
  message("\n[EXHAUSTIVO] Comparacion de modelos (ANOVA + AIC)")
  print(anova(res_exh_no_int$fit, res_exh_int$fit))
  print(AIC(res_exh_no_int$fit, res_exh_int$fit))
} else {
  warning("[EXHAUSTIVO] No se pudieron comparar modelos (fit no disponible).")
}

message("\n[EXHAUSTIVO] Optimos (grad CV = -tau)")
message(sprintf(
  " CON interaccion  (continuo): L*= %.2f W*= %.2f  | redondeado: %d %d",
  res_exh_int$h_star, res_exh_int$w_star,
  as.integer(ceiling(res_exh_int$h_star)), as.integer(ceiling(res_exh_int$w_star))
))
message(sprintf(
  " SIN interaccion  (continuo): L*= %.2f W*= %.2f  | redondeado: %d %d",
  res_exh_no_int$h_star, res_exh_no_int$w_star,
  as.integer(ceiling(res_exh_no_int$h_star)), as.integer(ceiling(res_exh_no_int$w_star))
))

## =========================================================
##  3) TILING (agricolae): tabla y ajustes
## =========================================================
datos_til <- spatial_variation_tiling(blanco, tiling_exclusion_mode)$res
if (nrow(datos_til) < 3) {
  warning(sprintf(
    "[TILING] Muy pocos tamanos (n=%d) con exclusion='%s'. Reintentando con exclusion='only_full'.",
    nrow(datos_til), tiling_exclusion_mode
  ))
  datos_til <- spatial_variation_tiling(blanco, exclusion = "only_full")$res
}
message("\n[TILING] Primeras filas:")
print(utils::head(datos_til, 10))

if (nrow(datos_til) >= 3) {
  # Óptimos continuos penalizados
  opt_til_int_cont <- fit_exhaustive_optimal_plot_size(
    datos_til, nr, nc,
    include_interaction = TRUE, tau = tau_vec
  )
  opt_til_no_int_cont <- fit_exhaustive_optimal_plot_size(
    datos_til, nr, nc,
    include_interaction = FALSE, tau = tau_vec
  )

  reg_int <- opt_til_int_cont$fit
  reg_no_int <- opt_til_no_int_cont$fit

  message("\n[TILING] Optimos (grad CV = -tau)")
  message(sprintf(
    " CON interaccion  (continuo): L*= %.2f W*= %.2f  | redondeado: %d %d",
    opt_til_int_cont$h_star, opt_til_int_cont$w_star,
    as.integer(ceiling(opt_til_int_cont$h_star)), as.integer(ceiling(opt_til_int_cont$w_star))
  ))
  message(sprintf(
    " SIN interaccion  (continuo): L*= %.2f W*= %.2f  | redondeado: %d %d",
    opt_til_no_int_cont$h_star, opt_til_no_int_cont$w_star,
    as.integer(ceiling(opt_til_no_int_cont$h_star)), as.integer(ceiling(opt_til_no_int_cont$w_star))
  ))

  # Comparación de modelos TILING
  message("\n[TILING] Comparacion de modelos (ANOVA + AIC)")
  print(anova(reg_no_int, reg_int))
  print(AIC(reg_no_int, reg_int))
} else {
  reg_int <- NULL
  reg_no_int <- NULL
  opt_til_int_cont <- NULL
  opt_til_no_int_cont <- NULL
  warning(sprintf(
    "[TILING] Solo %d tamaños. No se ajustan modelos cuadraticos; omitiendo optimizacion.",
    nrow(datos_til)
  ))
}

## =========================================================
##  4) Comparacion directa de CV entre metodos (tamaños comunes)
## =========================================================
tab_exh_cmp <- tab_exh[, c("Length", "Width", "Size", "CV")]
names(tab_exh_cmp)[4] <- "CV_exh"
tab_til_cmp <- datos_til[, c("Length", "Width", "CV")]
names(tab_til_cmp)[3] <- "CV_til"
comp <- merge(tab_exh_cmp, tab_til_cmp, by = c("Length", "Width"))
message("\n--- COMPARACION DE CV (Exhaustivo vs Tiling) ---")
message(sprintf("n comun = %d", nrow(comp)))
if (nrow(comp) > 0) {
  message(sprintf("Correlacion: %.4f", cor(comp$CV_exh, comp$CV_til, use = "complete.obs")))
  message(sprintf("Sesgo medio (exh - til): %.4f", mean(comp$CV_exh - comp$CV_til, na.rm = TRUE)))
  message(
    sprintf("Sesgo mediano (exh - til): %.4f", median(comp$CV_exh - comp$CV_til, na.rm = TRUE))
  )
}

## =========================================================
##  5) Graficos (objetos individuales)
## =========================================================

# ------ EXHAUSTIVO
plot_1 <- plot_cv_contour(
  res_exh_int$fit,
  nr = nr, nc = nc,
  title = "EXHAUSTIVO — Contorno CV (CON interaccion)",
  mark = data.frame(Length = res_exh_int$h_opt, Width = res_exh_int$w_opt),
  mark_col = "red",
  mark_lab = paste0("optimo (L=", res_exh_int$h_opt, ", W=", res_exh_int$w_opt, ")")
)

plot_2 <- plot_cv_contour(
  res_exh_no_int$fit,
  nr = nr, nc = nc,
  title = "EXHAUSTIVO — Contorno CV (SIN interaccion)",
  mark = data.frame(Length = res_exh_no_int$h_opt, Width = res_exh_no_int$w_opt),
  mark_col = "blue",
  mark_lab = paste0("optimo (L=", res_exh_no_int$h_opt, ", W=", res_exh_no_int$w_opt, ")")
)

# ------ TILING (solo si hay modelos)
plot_3 <- NULL
if (!is.null(reg_int)) {
  L_i <- ceiling(opt_til_int_cont$h_star)
  W_i <- ceiling(opt_til_int_cont$w_star)
  mark_i <- if (is.na(L_i) || is.na(W_i)) NULL else data.frame(Length = L_i, Width = W_i)
  plot_3 <- plot_cv_contour(
    reg_int,
    nr = nr, nc = nc,
    title = "TILING — Contorno CV (CON interaccion)",
    mark = mark_i,
    mark_col = "grey30",
    mark_lab = if (is.null(mark_i)) NULL else paste0("optimo (L=", L_i, ", W=", W_i, ")")
  ) +
    ggplot2::annotate("segment",
      x = 1, xend = L_i, y = W_i, yend = W_i,
      linetype = "dashed", colour = "grey30"
    ) +
    ggplot2::annotate("segment",
      x = L_i, xend = L_i, y = 1, yend = W_i,
      linetype = "dashed", colour = "grey30"
    ) +
    ggplot2::geom_point(
      data = mark_i, ggplot2::aes(x = Length, y = Width),
      inherit.aes = FALSE, shape = 16, size = 4, colour = "grey30"
    )
}

## Nota: plot_4 (tiling sin interacción) se deja para la sección dedicada al final.

# ------ Dispersion y sesgo (si hay pares en comun)
plot_5 <- plot_6 <- NULL
if (exists("comp") && is.data.frame(comp) && nrow(comp) > 0) {
  plot_5 <- ggplot2::ggplot(comp, ggplot2::aes(x = CV_exh, y = CV_til)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      title = "Comparacion de CV: Exhaustivo vs Tiling",
      x = "CV (metodo exhaustivo)", y = "CV (tiling agricolae)"
    )

  plot_6 <- ggplot2::ggplot(comp, ggplot2::aes(x = Size, y = CV_exh - CV_til)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      title = "Sesgo (CV_exh - CV_til) vs Size de UE",
      x = "Size (Length x Width)", y = "Diferencia de CV"
    )
}

## =========================================================
##  Impresion de graficos (excepto plot_4, que se define al final)
## =========================================================
if (make_plots) {
  if (exists("plot_1")) print(plot_1)
  if (exists("plot_2")) print(plot_2)
  if (exists("plot_3") && !is.null(plot_3)) print(plot_3)
  if (exists("plot_5") && !is.null(plot_5)) print(plot_5)
  if (exists("plot_6") && !is.null(plot_6)) print(plot_6)
}

#########################################################################
#######  Modificación solo plot_4 (tiling sin interacción)  #############
#########################################################################

# Óptimo continuo redondeado hacia arriba
L_n <- as.integer(ceiling(as.numeric(opt_til_no_int_cont$h_star)))
W_n <- as.integer(ceiling(as.numeric(opt_til_no_int_cont$w_star))) # ancho

# Grilla de predicción
grid_til_no <- expand.grid(Length = seq_len(nr), Width = seq_len(nc))
grid_til_no$CV_pred <- as.numeric(predict(reg_no_int, newdata = grid_til_no))

plot_4 <-
  ggplot2::ggplot(
    subset(grid_til_no, is.finite(CV_pred)),
    ggplot2::aes(x = Length, y = Width, z = CV_pred)
  ) +
  ggplot2::geom_contour_filled(bins = 12) +
  ggplot2::scale_fill_viridis_d(name = "CV", direction = -1) +
  ggplot2::coord_fixed(xlim = c(1, nr), ylim = c(1, nc), expand = FALSE) +
  ggplot2::scale_x_continuous(labels = function(x) format(x, decimal.mark = ",")) +
  ggplot2::labs(title = "", x = "Length", y = "Width") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::annotate("segment",
    x = 1, xend = L_n,
    y = W_n, yend = W_n,
    linetype = "dashed", linewidth = 0.6, colour = "black"
  ) +
  ggplot2::annotate("segment",
    x = L_n, xend = L_n,
    y = 1, yend = W_n,
    linetype = "dashed", linewidth = 0.6, colour = "black"
  ) +
  ggplot2::annotate("point",
    x = L_n, y = W_n,
    shape = 16, size = 4, colour = "black"
  ) +
  ggplot2::annotate("text",
    x = L_n, y = W_n,
    label = sprintf("Óptimo (L=%d, W=%d)", L_n, W_n),
    vjust = -1, colour = "black"
  )

print(plot_4)

## =========================================================
##  6) Verificacion rapida del exhaustivo (conteos)
## =========================================================

## Ayudas de verificacion (exhaustivo)
check_exhaustive_counts <- function(verbose = TRUE) {
  expected_windows <- function(nr, nc, L, W) max(nr - L + 1, 0) * max(nc - W + 1, 0)
  set.seed(1)
  Y <- matrix(sample(1:9, 20, replace = TRUE), nrow = 4, ncol = 5)
  s12 <- fielddesign:::sliding_window_sum(Y, 1, 2, combine_orientations = FALSE)$res
  s21 <- fielddesign:::sliding_window_sum(Y, 2, 1, combine_orientations = FALSE)$res
  s12c <- fielddesign:::sliding_window_sum(Y, 1, 2, combine_orientations = TRUE)$res
  nr <- nrow(Y)
  nc <- ncol(Y)
  exp_12 <- expected_windows(nr, nc, 1, 2)
  exp_21 <- expected_windows(nr, nc, 2, 1)
  if (verbose) {
    message(sprintf("Dims Y: %d x %d", nr, nc))
    message(sprintf("Esperado 1x2: %d ; obtenido: %d", exp_12, length(s12)))
    message(sprintf("Esperado 2x1: %d ; obtenido: %d", exp_21, length(s21)))
    message(
      sprintf("Combinado (1x2 + 2x1) esperado: %d ; obtenido: %d", exp_12 + exp_21, length(s12c))
    )
  }
  invisible(list(
    ok = length(s12) == exp_12 && length(s21) == exp_21 && length(s12c) == (exp_12 + exp_21)
  ))
}
check_exhaustive_counts()
