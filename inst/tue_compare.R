## =========================================================
##  TAMANO DE UE: EXHAUSTIVO (ventanas) vs TILING (agricolae)
##  - Exhaustivo: ventanas inside, SUMAS; CV clasico; grad(CV) = -tau
##  - Tiling: divisores exactos; CV clasico; modo de exclusion configurable
## =========================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(tidyr)
})

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

## ---------------- Utilidades base ----------------
to_matrix_numeric <- function(x) {
  if (is.matrix(x)) {
    return(apply(x, 2, as.numeric))
  }
  as.matrix(data.frame(lapply(x, as.numeric)))
}
integral2d <- function(M) {
  II <- apply(M, 2, cumsum)
  II <- t(apply(II, 1, cumsum))
  II
}
block_sum <- function(II, r, c, a, b) {
  r2 <- r + a - 1
  c2 <- c + b - 1
  A <- II[r2, c2]
  B <- if (r > 1) II[r - 1, c2] else 0
  C <- if (c > 1) II[r2, c - 1] else 0
  D <- if (r > 1 && c > 1) II[r - 1, c - 1] else 0
  A - B - C + D
}
cv_classic <- function(x, percent = TRUE) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(m) || m == 0) {
    return(NA_real_)
  }
  cv <- s / m
  if (percent) 100 * cv else cv
}
coef_or0 <- function(b, name) {
  val <- if (name %in% names(b)) unname(b[name]) else 0
  if (!is.finite(val)) 0 else val
}

## =========================================================
##  Helpers de óptimo (enteros y continuos)
## =========================================================

# Entero en vecindad 2x2 alrededor de (L*, W*)
Largo <- Ancho <- NULL
find_integer_optimum <- function(L_star, W_star, fit_model, tau_vec, nr_max, nc_max) {
  if (!all(is.finite(c(L_star, W_star)))) {
    return(list(L_opt = NA_integer_, W_opt = NA_integer_, pred_at_opt = NA_real_))
  }
  L_candidates <- unique(c(floor(L_star), ceiling(L_star)))
  W_candidates <- unique(c(floor(W_star), ceiling(W_star)))
  best_L_opt <- NA_integer_
  best_W_opt <- NA_integer_
  min_obj <- Inf
  best_pred <- NA_real_

  for (L_c in L_candidates) {
    for (W_c in W_candidates) {
      if (L_c >= 1 && W_c >= 1 && L_c <= nr_max && W_c <= nc_max) {
        pred_cv <- as.numeric(predict(fit_model, newdata = data.frame(Largo = L_c, Ancho = W_c)))
        if (is.finite(pred_cv)) {
          obj <- pred_cv + tau_vec[1] * L_c + tau_vec[2] * W_c
          if (obj < min_obj) {
            min_obj <- obj
            best_L_opt <- as.integer(L_c)
            best_W_opt <- as.integer(W_c)
            best_pred <- pred_cv
          }
        }
      }
    }
  }
  list(L_opt = best_L_opt, W_opt = best_W_opt, pred_at_opt = best_pred)
}

# Óptimo continuo penalizado para cualquier lm (nombres consistentes: L_* = Largo, W_/A_* = Ancho)
calc_optimo_penalizado <- function(model, tau = c(1, 1), nr = NULL, nc = NULL) {
  b <- coef(model)
  bL <- coef_or0(b, "Largo")
  bW <- coef_or0(b, "Ancho")
  bL2 <- coef_or0(b, "I(Largo^2)")
  bW2 <- coef_or0(b, "I(Ancho^2)")
  bLW <- coef_or0(b, "Largo:Ancho")

  H <- matrix(c(2 * bL2, bLW, bLW, 2 * bW2), 2, 2)
  g <- c(bL, bW)

  detH <- tryCatch(determinant(H, logarithm = FALSE)$modulus, error = function(e) NA_real_)
  if (!is.finite(detH) || detH < .Machine$double.eps^0.5) {
    return(list(
      L_star = NA_real_, W_star = NA_real_, A_star = NA_real_,
      pred_at_star = NA_real_,
      L_ceil = NA_integer_, W_ceil = NA_integer_, A_ceil = NA_integer_
    ))
  }

  sol <- -solve(H, g + tau)
  L_star <- as.numeric(sol[1])
  W_star <- as.numeric(sol[2])

  pred_at_star <- tryCatch(
    predict(model, newdata = data.frame(Largo = L_star, Ancho = W_star)),
    error = function(e) NA_real_
  )

  # Ceiling acotado a [1, nr/nc]
  L_ceil <- if (is.finite(L_star)) as.integer(ceiling(L_star)) else NA_integer_
  W_ceil <- if (is.finite(W_star)) as.integer(ceiling(W_star)) else NA_integer_
  if (!is.null(nr) && is.finite(L_ceil)) L_ceil <- max(1L, min(L_ceil, as.integer(nr)))
  if (!is.null(nc) && is.finite(W_ceil)) W_ceil <- max(1L, min(W_ceil, as.integer(nc)))

  list(
    L_star = L_star, W_star = W_star, A_star = W_star, # A_* alias de Ancho
    pred_at_star = as.numeric(pred_at_star),
    L_ceil = L_ceil, W_ceil = W_ceil, A_ceil = W_ceil
  )
}

ceil_safe <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (is.na(x)) NA_integer_ else as.integer(ceiling(x))
}

## =========================================================
##  1) METODO EXHAUSTIVO — ventanas inside, SUMAS, CV clasico
## =========================================================
collect_sums_arch <- function(Y, L, W, combine_orientations = TRUE) {
  Y <- to_matrix_numeric(Y)
  nr <- nrow(Y)
  nc <- ncol(Y)
  II <- integral2d(Y)
  sums <- c()
  if (L <= nr && W <= nc) {
    nrw <- nr - L + 1
    ncw <- nc - W + 1
    if (nrw > 0 && ncw > 0) {
      out <- numeric(nrw * ncw)
      k <- 1
      for (r in seq_len(nrw)) {
        for (c in seq_len(ncw)) {
          out[k] <- block_sum(II, r, c, L, W)
          k <- k + 1
        }
      }
      sums <- c(sums, out)
    }
  }
  if (combine_orientations && L != W && W <= nr && L <= nc) {
    nrw2 <- nr - W + 1
    ncw2 <- nc - L + 1
    if (nrw2 > 0 && ncw2 > 0) {
      out2 <- numeric(nrw2 * ncw2)
      k <- 1
      for (r in seq_len(nrw2)) {
        for (c in seq_len(ncw2)) {
          out2[k] <- block_sum(II, r, c, W, L)
          k <- k + 1
        }
      }
      sums <- c(sums, out2)
    }
  }
  sums
}

build_arch_table_exh <- function(blanco,
                                 combine_orientations = TRUE,
                                 percent = TRUE,
                                 max_area = NULL) {
  Y <- to_matrix_numeric(blanco)
  nr <- nrow(Y)
  nc <- ncol(Y)
  if (is.null(max_area)) max_area <- nr * nc
  max_area <- min(max_area, nr * nc)
  out <- list()
  for (L in 1:nr) {
    W_max_area <- floor(max_area / L)
    W_limit <- min(max(nr, nc), W_max_area)
    W_start <- if (combine_orientations) L else 1
    for (W in W_start:W_limit) {
      feasible <- (L <= nr && W <= nc) || (W <= nr && L <= nc)
      if (!feasible) next
      A <- L * W
      if (A > max_area) next
      key <- paste(L, W, sep = "x")
      if (!is.null(out[[key]])) next
      sums <- collect_sums_arch(Y, L, W, combine_orientations)
      if (length(sums) < 2) next
      out[[key]] <- data.frame(
        Largo      = L,
        Ancho      = W,
        Tamano     = A,
        n_ventanas = length(sums),
        CV         = cv_classic(sums, percent = percent)
      )
    }
  }
  if (length(out) == 0) {
    return(data.frame())
  }
  tab <- do.call(rbind, out)
  tab[order(tab$Tamano, tab$Largo, tab$Ancho), ]
}

# Modelos para exhaustivo (con y sin interacción)
fit_exh_models <- function(tabla_exh) {
  df <- as.data.frame(tabla_exh)
  form_int <- CV ~ Largo + Ancho + I(Largo^2) + I(Ancho^2) + Largo:Ancho
  form_no_int <- CV ~ Largo + Ancho + I(Largo^2) + I(Ancho^2)
  reg_int <- lm(form_int, data = df)
  reg_no_int <- lm(form_no_int, data = df)
  list(reg_int = reg_int, reg_no_int = reg_no_int)
}

# Wrapper compatible (mismos nombres)
fit_quadratic_with_tau <- function(tabla_arch,
                                   include_interaction = TRUE,
                                   tau = c(1, 1),
                                   nr = NULL, nc = NULL) {
  df <- as.data.frame(tabla_arch)
  form <- if (include_interaction) {
    as.formula("CV ~ Largo + Ancho + I(Largo^2) + I(Ancho^2) + Largo:Ancho")
  } else {
    as.formula("CV ~ Largo + Ancho + I(Largo^2) + I(Ancho^2)")
  }
  fit <- lm(form, data = df)
  opt <- calc_optimo_penalizado(fit, tau = tau, nr = nr, nc = nc)
  if (!is.null(nr) && !is.null(nc) && all(is.finite(c(opt$L_star, opt$W_star)))) {
    int_near <- find_integer_optimum(opt$L_star, opt$W_star, fit, tau, nr, nc)
    L_opt <- int_near$L_opt
    W_opt <- int_near$W_opt
  } else {
    L_opt <- opt$L_ceil
    W_opt <- opt$W_ceil
  }
  list(
    fit = fit,
    coef = coef(fit),
    H = NULL, grad_linear = NULL,
    tau = tau,
    L_star = opt$L_star, A_star = opt$A_star, W_star = opt$W_star,
    L_opt = L_opt, A_opt = W_opt, W_opt = W_opt,
    pred_at_star = opt$pred_at_star,
    include_interaction = include_interaction
  )
}

## Ayudas de verificacion (exhaustivo)
check_exhaustive_counts <- function(verbose = TRUE) {
  expected_windows <- function(nr, nc, L, W) max(nr - L + 1, 0) * max(nc - W + 1, 0)
  set.seed(1)
  Y <- matrix(sample(1:9, 20, replace = TRUE), nrow = 4, ncol = 5)
  s12 <- collect_sums_arch(Y, 1, 2, combine_orientations = FALSE)
  s21 <- collect_sums_arch(Y, 2, 1, combine_orientations = FALSE)
  s12c <- collect_sums_arch(Y, 1, 2, combine_orientations = TRUE)
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

## =========================================================
##  2) METODO TILING (agricolae::index.smith) — CV clasico
## =========================================================
cv_tiling_agricolae <- function(A, exclusion = c("smith", "only_full")) {
  exclusion <- match.arg(exclusion)
  A <- to_matrix_numeric(A)
  n <- nrow(A)
  m <- ncol(A)
  media <- mean(A)
  TC <- sum(A)^2 / (n * m)
  r_div <- (1:n)[n %% (1:n) == 0]
  c_div <- (1:m)[m %% (1:m) == 0]
  if (exclusion == "smith") {
    if (length(r_div) > 0) r_div <- r_div[-length(r_div)]
    if (length(c_div) > 0) c_div <- c_div[-length(c_div)]
  }
  res <- list()
  idx <- 1
  for (L in r_div) {
    for (W in c_div) {
      if (exclusion == "only_full" && L == n && W == m) next
      n_blocks_r <- n / L
      n_blocks_c <- m / W
      ss <- 0
      r_top <- 1
      for (bi in 1:n_blocks_r) {
        r_bot <- r_top + L - 1
        c_left <- 1
        for (bj in 1:n_blocks_c) {
          c_right <- c_left + W - 1
          bsum <- sum(A[r_top:r_bot, c_left:c_right])
          ss <- ss + (bsum^2) / (W * L)
          c_left <- c_right + 1
        }
        r_top <- r_bot + 1
      }
      V <- (ss - TC) / (n * m - 1)
      CV <- 100 * sqrt(V) / media
      res[[idx]] <- data.frame(
        Largo = L, Ancho = W, Size = L * W,
        plots = (n * m) / (L * W), V = V, C.V. = CV
      )
      idx <- idx + 1
    }
  }
  if (length(res) == 0) {
    return(data.frame())
  }
  out <- do.call(rbind, res)
  out[order(out$Largo, out$Ancho), ]
}

# Óptimo continuo para tiling (alias consistentes)
get_continuous_opt_from_fit_tiling <- function(fit_model, tau = c(1, 1), has_interaction = TRUE) {
  b <- coef(fit_model)
  bL <- coef_or0(b, "Largo")
  bW <- coef_or0(b, "Ancho")
  bL2 <- coef_or0(b, "I(Largo^2)")
  bW2 <- coef_or0(b, "I(Ancho^2)")
  bLW <- if (has_interaction) coef_or0(b, "Largo:Ancho") else 0
  H <- matrix(c(2 * bL2, bLW, bLW, 2 * bW2), 2, 2)
  g <- c(bL, bW)
  detH <- determinant(H, logarithm = FALSE)$modulus
  if (!is.finite(detH) || detH < .Machine$double.eps^0.5) {
    return(list(L_star = NA_real_, W_star = NA_real_, A_star = NA_real_))
  }
  sol <- -solve(H, g + tau)
  L_star <- as.numeric(sol[1])
  W_star <- as.numeric(sol[2])
  list(L_star = L_star, W_star = W_star, A_star = W_star) # A_* alias de ancho
}

## =========================================================
##  3) Carga de datos
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
##  4) EXHAUSTIVO: tabla y ajustes
## =========================================================
tab_exh <- build_arch_table_exh(
  blanco,
  combine_orientations = combine_orientations_exh,
  percent = TRUE,
  max_area = max_area_exh
)
message("\n[EXHAUSTIVO] Primeras filas:")
print(head(tab_exh, 10))
stopifnot(nrow(tab_exh) > 0, all(c("Largo", "Ancho", "CV") %in% names(tab_exh)))

### CancuCompare
sv_exh_new <- tab_exh_new <- fielddesign::spatial_variation_exhaustive(
  blanco, combine_orientations_exh, max_area_exh
)$res
colnames(tab_exh_new) <- c("Tamano", "Ancho", "Largo", "n_ventanas", "CV")
all.equal(tab_exh[, c("Tamano", "Ancho", "Largo", "n_ventanas", "CV")], tab_exh_new)

res_exh_int <- fit_quadratic_with_tau(
  tab_exh, include_interaction = TRUE, tau = tau_vec, nr = nr, nc = nc
)
res_exh_no_int <- fit_quadratic_with_tau(
  tab_exh, include_interaction = FALSE, tau = tau_vec, nr = nr, nc = nc
)

### CancuCompare
res_exh_int_new <- fielddesign::fit_exhaustive_optimal_plot_size(
  sv_exh_new, nr, nc,
  include_interaction = TRUE, tau = tau_vec
)
res_exh_int$A_star == res_exh_int_new$w_star && res_exh_int$L_star == res_exh_int_new$h_star &&
  res_exh_int$A_opt == res_exh_int_new$w_opt && res_exh_int$L_opt == res_exh_int_new$h_opt
all.equal(unname(res_exh_int$fit$coefficients), unname(res_exh_int_new$fit$coefficients))
res_exh_no_int_new <- fielddesign::fit_exhaustive_optimal_plot_size(
  sv_exh_new, nr, nc,
  include_interaction = FALSE, tau = tau_vec
)
res_exh_no_int$A_star == res_exh_no_int_new$w_star &&
  res_exh_no_int$L_star == res_exh_no_int_new$h_star &&
  res_exh_no_int$A_opt == res_exh_no_int_new$w_opt &&
  res_exh_no_int$L_opt == res_exh_no_int_new$h_opt
all.equal(unname(res_exh_no_int$fit$coefficients), unname(res_exh_no_int_new$fit$coefficients))

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
  res_exh_int$L_star, res_exh_int$W_star,
  as.integer(ceil_safe(res_exh_int$L_star)), as.integer(ceil_safe(res_exh_int$W_star))
))
message(sprintf(
  " SIN interaccion  (continuo): L*= %.2f W*= %.2f  | redondeado: %d %d",
  res_exh_no_int$L_star, res_exh_no_int$W_star,
  as.integer(ceil_safe(res_exh_no_int$L_star)), as.integer(ceil_safe(res_exh_no_int$W_star))
))

## =========================================================
##  5) TILING (agricolae): tabla y ajustes
## =========================================================
.can_fit_quad <- function(df, resp_col) {
  is.data.frame(df) && resp_col %in% names(df) && nrow(df) >= 3
}
reg_int <- NULL
reg_no_int <- NULL
opt_til_int_cont <- NULL
opt_til_no_int_cont <- NULL

datos_til <- cv_tiling_agricolae(blanco, exclusion = tiling_exclusion_mode)

### CancuCompare
sv_til_new <- datos_til_new <- fielddesign::spatial_variation_tiling(
  blanco, tiling_exclusion_mode
)$res
colnames(datos_til_new) <- c("Size", "Ancho", "Largo", "plots", "V", "C.V.")
datos_til_new <- datos_til_new[, colnames(datos_til)]
datos_til_new <- datos_til_new[order(datos_til_new$Largo, datos_til_new$Ancho), ]
rownames(datos_til_new) <- NULL
rownames(datos_til) <- NULL
all.equal(datos_til, datos_til_new)

if (nrow(datos_til) < 3) {
  warning(sprintf(
    "[TILING] Muy pocos tamanos (n=%d) con exclusion='%s'. Reintentando con exclusion='only_full'.",
    nrow(datos_til), tiling_exclusion_mode
  ))
  datos_til <- cv_tiling_agricolae(blanco, exclusion = "only_full")
}

### CancuCompare
datos_til_of <- cv_tiling_agricolae(blanco, exclusion = "only_full")
sv_til_of_new <- datos_til_of_new <- fielddesign::spatial_variation_tiling(
  blanco,
  exclusion = "only_full"
)$res
colnames(datos_til_of_new) <- c("Size", "Ancho", "Largo", "plots", "V", "C.V.")
datos_til_of_new <- datos_til_of_new[, colnames(datos_til_of)]
datos_til_of_new <- datos_til_of_new[order(datos_til_of_new$Largo, datos_til_of_new$Ancho), ]
rownames(datos_til_of_new) <- NULL
rownames(datos_til_of) <- NULL
all.equal(datos_til_of, datos_til_of_new)

message("\n[TILING] Primeras filas:")
print(head(datos_til, 10))

if (.can_fit_quad(datos_til, "C.V.")) {
  reg_int <- lm(C.V. ~ Largo + Ancho + I(Largo^2) + I(Ancho^2) + Largo:Ancho, data = datos_til)
  reg_no_int <- lm(C.V. ~ Largo + Ancho + I(Largo^2) + I(Ancho^2), data = datos_til)

  # Óptimos continuos penalizados
  opt_til_int_cont <- get_continuous_opt_from_fit_tiling(
    reg_int, tau = tau_vec, has_interaction = TRUE
  )
  ### CancuCompare
  opt_til_int_cont_new <- fielddesign::fit_exhaustive_optimal_plot_size(
    sv_til_new, nr, nc,
    include_interaction = TRUE, tau = tau_vec
  )
  all.equal(opt_til_int_cont$L_star, opt_til_int_cont_new$h_star)
  all.equal(opt_til_int_cont$A_star, opt_til_int_cont_new$w_star)

  opt_til_no_int_cont <- get_continuous_opt_from_fit_tiling(
    reg_no_int, tau = tau_vec, has_interaction = FALSE
  )
  opt_til_no_int_cont_new <- fielddesign::fit_exhaustive_optimal_plot_size(
    sv_til_new, nr, nc,
    include_interaction = FALSE, tau = tau_vec
  )
  all.equal(opt_til_no_int_cont$L_star, opt_til_no_int_cont_new$h_star)
  all.equal(opt_til_no_int_cont$A_star, opt_til_no_int_cont_new$w_star)

  message("\n[TILING] Optimos (grad CV = -tau)")
  message(sprintf(
    " CON interaccion  (continuo): L*= %.2f W*= %.2f  | redondeado: %d %d",
    opt_til_int_cont$L_star, opt_til_int_cont$W_star,
    as.integer(ceil_safe(opt_til_int_cont$L_star)), as.integer(ceil_safe(opt_til_int_cont$W_star))
  ))
  message(sprintf(
    " SIN interaccion  (continuo): L*= %.2f W*= %.2f  | redondeado: %d %d",
    opt_til_no_int_cont$L_star, opt_til_no_int_cont$W_star,
    as.integer(ceil_safe(opt_til_no_int_cont$L_star)),
    as.integer(ceil_safe(opt_til_no_int_cont$W_star))
  ))

  # Comparación de modelos TILING
  message("\n[TILING] Comparacion de modelos (ANOVA + AIC)")
  print(anova(reg_no_int, reg_int))
  print(AIC(reg_no_int, reg_int))
} else {
  warning(sprintf(
    "[TILING] Solo %d tamaños. No se ajustan modelos cuadraticos; omitiendo optimizacion.",
    nrow(datos_til)
  ))
}

## =========================================================
##  6) Comparacion directa de CV entre metodos (tamaños comunes)
## =========================================================
stopifnot("CV" %in% names(tab_exh))
tab_exh_cmp <- tab_exh[, c("Largo", "Ancho", "CV")]
names(tab_exh_cmp)[3] <- "CV_exh"
tab_til_cmp <- datos_til[, c("Largo", "Ancho", "C.V.")]
names(tab_til_cmp)[3] <- "CV_til"
comp <- merge(tab_exh_cmp, tab_til_cmp, by = c("Largo", "Ancho"))
if (!"Tamano" %in% names(comp)) comp$Tamano <- with(comp, Largo * Ancho)
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
##  7) Funciones de graficado
## =========================================================
make_pred_grid <- function(nr, nc, fit) {
  grid <- expand.grid(Largo = 1:nr, Ancho = 1:nc)
  grid$CV_pred <- as.numeric(predict(fit, newdata = grid))
  grid
}

build_contour_plot <- function(grid_df, title = "Contorno CV", nr, nc,
                               mark = NULL, mark_col = "red", mark_lab = NULL) {
  grid_df <- subset(grid_df, is.finite(CV_pred))
  p <- ggplot(grid_df, aes(Largo, Ancho, z = CV_pred)) +
    geom_contour_filled(bins = 12) +
    scale_fill_viridis_d(name = "CV pred") +
    coord_fixed(xlim = c(1, nr), ylim = c(1, nc), expand = FALSE) +
    theme_minimal(base_size = 12) +
    labs(
      title = title,
      x = "Largo (n° de microparcelas)",
      y = "Ancho (n° de microparcelas)"
    )

  if (!is.null(mark)) {
    p <- p +
      geom_point(
        data = mark, aes(Largo, Ancho),
        color = mark_col, size = 3, inherit.aes = FALSE
      )
    if (!is.null(mark_lab)) {
      p <- p + annotate("text",
        x = mark$Largo[1], y = mark$Ancho[1],
        label = mark_lab, vjust = -1, color = mark_col
      )
    }
  }
  p
}

## =========================================================
##  8) Graficos (objetos individuales)
## =========================================================

# ------ EXHAUSTIVO
grid_exh_int <- make_pred_grid(nr, nc, res_exh_int$fit)
plot_1 <- build_contour_plot(
  grid_exh_int,
  title = "EXHAUSTIVO — Contorno CV (CON interaccion)",
  nr = nr, nc = nc,
  mark = data.frame(Largo = res_exh_int$L_opt, Ancho = res_exh_int$W_opt),
  mark_col = "red",
  mark_lab = paste0("optimo (L=", res_exh_int$L_opt, ", W=", res_exh_int$W_opt, ")")
)

grid_exh_no <- make_pred_grid(nr, nc, res_exh_no_int$fit)
plot_2 <- build_contour_plot(
  grid_exh_no,
  title = "EXHAUSTIVO — Contorno CV (SIN interaccion)",
  nr = nr, nc = nc,
  mark = data.frame(Largo = res_exh_no_int$L_opt, Ancho = res_exh_no_int$W_opt),
  mark_col = "blue",
  mark_lab = paste0("optimo (L=", res_exh_no_int$L_opt, ", W=", res_exh_no_int$W_opt, ")")
)

# ------ TILING (solo si hay modelos)
plot_3 <- NULL
if (!is.null(reg_int)) {
  grid_til_int <- make_pred_grid(nr, nc, reg_int)
  L_i <- ceil_safe(opt_til_int_cont$L_star)
  W_i <- ceil_safe(opt_til_int_cont$W_star)
  mark_i <- if (is.na(L_i) || is.na(W_i)) NULL else data.frame(Largo = L_i, Ancho = W_i)

  plot_3 <- build_contour_plot(
    grid_til_int,
    title = "TILING — Contorno CV (CON interaccion)",
    nr = nr, nc = nc,
    mark = mark_i,
    mark_col = "grey30",
    mark_lab = if (is.null(mark_i)) NULL else paste0("optimo (L=", L_i, ", W=", W_i, ")")
  ) +
    annotate("segment",
      x = 1, xend = L_i, y = W_i, yend = W_i,
      linetype = "dashed", colour = "grey30"
    ) +
    annotate("segment",
      x = L_i, xend = L_i, y = 1, yend = W_i,
      linetype = "dashed", colour = "grey30"
    ) +
    geom_point(
      data = mark_i, aes(x = Largo, y = Ancho),
      inherit.aes = FALSE, shape = 16, size = 4, colour = "grey30"
    )
}

## Nota: plot_4 (tiling sin interacción) se deja para la sección dedicada al final.

# ------ Dispersion y sesgo (si hay pares en comun)
plot_5 <- plot_6 <- NULL
if (exists("comp") && is.data.frame(comp) && nrow(comp) > 0) {
  plot_5 <- ggplot(comp, aes(x = CV_exh, y = CV_til)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Comparacion de CV: Exhaustivo vs Tiling",
      x = "CV (metodo exhaustivo)", y = "CV (tiling agricolae)"
    )

  plot_6 <- ggplot(comp, aes(x = Tamano, y = CV_exh - CV_til)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point() +
    theme_minimal(base_size = 12) +
    labs(
      title = "Sesgo (CV_exh - CV_til) vs Tamano de UE",
      x = "Tamano (Largo x Ancho)", y = "Diferencia de CV"
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

## =========================================================
##  9) Verificacion rapida del exhaustivo (conteos)
## =========================================================
invisible(check_exhaustive_counts())

#########################################################################
#######  Modificación solo plot_4 (tiling sin interacción)  #############
#########################################################################

# Óptimo continuo redondeado hacia arriba
L_n <- as.integer(ceiling(as.numeric(opt_til_no_int_cont$L_star)))
W_n <- as.integer(ceiling(as.numeric(opt_til_no_int_cont$W_star))) # ancho

# Grilla de predicción
grid_til_no <- make_pred_grid(nr, nc, reg_no_int)

plot_4 <-
  ggplot(
    subset(grid_til_no, is.finite(CV_pred)),
    aes(x = Largo, y = Ancho, z = CV_pred)
  ) +
  geom_contour_filled(bins = 12) +
  scale_fill_viridis_d(name = "CV", direction = -1) +
  coord_fixed(xlim = c(1, nr), ylim = c(1, nc), expand = FALSE) +
  scale_x_continuous(labels = function(x) format(x, decimal.mark = ",")) +
  labs(title = "", x = "Largo", y = "Ancho") +
  theme_minimal(base_size = 12) +
  annotate("segment",
    x = 1, xend = L_n,
    y = W_n, yend = W_n,
    linetype = "dashed", linewidth = 0.6, colour = "black"
  ) +
  annotate("segment",
    x = L_n, xend = L_n,
    y = 1, yend = W_n,
    linetype = "dashed", linewidth = 0.6, colour = "black"
  ) +
  annotate("point",
    x = L_n, y = W_n,
    shape = 16, size = 4, colour = "black"
  ) +
  annotate("text",
    x = L_n, y = W_n,
    label = sprintf("Óptimo (L=%d, W=%d)", L_n, W_n),
    vjust = -1, colour = "black"
  )

print(plot_4)

# ---- Grafico 3D ----

# --- Grilla extendida a 15 x 15 ---
grid_til_no_ext <- expand.grid(Largo = 1:15, Ancho = 1:15)
grid_til_no_ext$CV_pred <- as.numeric(predict(reg_no_int, newdata = grid_til_no_ext))

# --- Pasar a matriz para plotly ---
Z_wide_ext <- grid_til_no_ext %>%
  arrange(Largo, Ancho) %>%
  tidyr::pivot_wider(names_from = Ancho, values_from = CV_pred) %>%
  arrange(Largo)

x_vals <- Z_wide_ext$Largo
y_vals <- sort(unique(grid_til_no_ext$Ancho))
Z_mat <- as.matrix(dplyr::select(Z_wide_ext, -Largo))

# --- Punto óptimo ---
ix <- match(L_n, x_vals)
iy <- match(W_n, y_vals)
z_point <- if (!is.na(ix) && !is.na(iy)) {
  Z_mat[ix, iy]
} else {
  as.numeric(predict(reg_no_int, newdata = data.frame(Largo = L_n, Ancho = W_n)))
}

# --- Definir rango deseado de CV ---
z_min <- 0
z_max <- 40

# --- Superficie 3D con eje Z y colorbar acotados ---
p3d <- plot_ly(
  x = x_vals, y = y_vals, z = ~Z_mat,
  type = "surface",
  colorscale = "Viridis",
  reversescale = TRUE, # claro = CV bajo
  cmin = z_min, cmax = z_max, # límites de color
  showscale = TRUE,
  colorbar = list(title = "CV", tickvals = seq(z_min, z_max, by = 10)),
  opacity = 0.95
) %>%
  add_trace(
    x = ~L_n, y = ~W_n, z = ~z_point,
    type = "scatter3d", mode = "markers",
    marker = list(size = 6, color = "black"),
    showlegend = FALSE
  ) %>%
  layout(
    title = "",
    scene = list(
      xaxis = list(title = "Largo"),
      yaxis = list(title = "Ancho"),
      zaxis = list(title = "CV pred", range = c(z_min, z_max)),
      aspectmode = "data"
    )
  )

p3d
