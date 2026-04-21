# psychometrics/correlation.R - IV Correlation
#
# References:
#   Theta: Billard & Diday, J. Am. Stat. Assoc., vol. 98, pp. 470-487, 2003.
#   Hausdorff: Kang et al., Stat. Comput., vol. 35, no. 207, 2025.

# Multi-segment utilities
normalize_interval <- function(interval) {
  if (is.list(interval)) {
    segs <- do.call(rbind, lapply(interval, function(s) if (is.matrix(s)) s else matrix(s, nrow = 1)))
  } else if (is.vector(interval) && length(interval) == 2) {
    segs <- matrix(interval, nrow = 1)
  } else if (is.matrix(interval)) {
    segs <- interval
  } else {
    stop("Unsupported interval format")
  }
  colnames(segs) <- c("L", "U")
  merge_segments(segs)
}

point_to_interval_dist <- function(omega, segs) {
  if (!is.matrix(segs)) segs <- matrix(segs, nrow = 1)
  dists <- sapply(1:nrow(segs), function(i) {
    L <- segs[i, 1]; U <- segs[i, 2]
    if (omega >= L && omega <= U) 0 else min(abs(omega - L), abs(omega - U))
  })
  min(dists)
}

hausdorff_dist <- function(A, B, eps = 1e-6) {
  A_mat <- normalize_interval(A)
  B_mat <- normalize_interval(B)
  if (nrow(A_mat) == 1 && nrow(B_mat) == 1) {
    return(max(abs(A_mat[1, 1] - B_mat[1, 1]), abs(A_mat[1, 2] - B_mat[1, 2])))
  }
  hausdorff_multiseg_algo1(A_mat, B_mat, eps = eps)
}

interval_endpoints <- function(interval) {
  as.vector(normalize_interval(interval))
}

interval_total_range <- function(interval) {
  segs <- normalize_interval(interval)
  sum(segs[, 2] - segs[, 1])
}

# SV correlation
calc_pearson_cor <- function(x, y) {
  cor(x, y, use = "complete.obs")
}

# IV correlations
calc_midpoint_cor <- function(x_L, x_U, y_L, y_U) {
  valid <- complete.cases(x_L, x_U, y_L, y_U)
  if (sum(valid) < 2) return(NA_real_)
  mid_x <- (x_L[valid] + x_U[valid]) / 2
  mid_y <- (y_L[valid] + y_U[valid]) / 2
  cor(mid_x, mid_y)
}

calc_theta_cor <- function(x_L, x_U, y_L, y_U, degenerate = FALSE, theta = 1) {
  valid <- complete.cases(x_L, x_U, y_L, y_U)
  x_L <- x_L[valid]; x_U <- x_U[valid]
  y_L <- y_L[valid]; y_U <- y_U[valid]
  n <- length(x_L)
  if (n < 2) return(NA_real_)
  
  mid_x <- (x_L + x_U) / 2
  spr_x <- (x_U - x_L) / 2
  mid_y <- (y_L + y_U) / 2
  spr_y <- (y_U - y_L) / 2
  
  var_spr_x <- var(spr_x)
  var_spr_y <- var(spr_y)
  x_has_zero_spr_var <- var_spr_x < 1e-12
  y_has_zero_spr_var <- var_spr_y < 1e-12
  
  if (x_has_zero_spr_var && y_has_zero_spr_var) return(cor(mid_x, mid_y))
  if (degenerate && (x_has_zero_spr_var || y_has_zero_spr_var)) return(cor(mid_x, mid_y))
  
  var_theta_x <- var(mid_x) + theta * var_spr_x
  var_theta_y <- var(mid_y) + theta * var_spr_y
  cov_theta_xy <- cov(mid_x, mid_y) + theta * cov(spr_x, spr_y)
  
  if (var_theta_x <= 0 || var_theta_y <= 0) return(NA_real_)
  cov_theta_xy / sqrt(var_theta_x * var_theta_y)
}

calc_hausdorff_cor <- function(x_L, x_U, y_L, y_U, use_sample_var = FALSE, degenerate = TRUE) {
  valid <- complete.cases(x_L, x_U, y_L, y_U)
  x_L <- x_L[valid]; x_U <- x_U[valid]
  y_L <- y_L[valid]; y_U <- y_U[valid]
  n <- length(x_L)
  if (n < 2) return(NA_real_)
  
  is_x_sv <- all(abs(x_U - x_L) < 1e-12)
  is_y_sv <- all(abs(y_U - y_L) < 1e-12)
  if (is_x_sv && is_y_sv) return(cor(x_L, y_L))
  
  h_x <- calc_hausdorff_stats(x_L, x_U, use_sample_var)
  h_y <- calc_hausdorff_stats(y_L, y_U, use_sample_var)
  mu_x_L <- h_x$mean_L; mu_x_U <- h_x$mean_U; var_x <- h_x$variance
  mu_y_L <- h_y$mean_L; mu_y_U <- h_y$mean_U; var_y <- h_y$variance
  
  if (is.na(var_x) || is.na(var_y) || var_x <= 0 || var_y <= 0) return(NA_real_)
  
  Cands <- cbind(x_L, x_U, y_L, y_U, mu_x_L, mu_x_U, mu_y_L, mu_y_U)
  
  get_dist_mat <- function(W, A_vec, B_vec) {
    n_row <- nrow(W); n_col <- ncol(W)
    A_mat <- matrix(A_vec, n_row, n_col)
    B_mat <- matrix(B_vec, n_row, n_col)
    dists <- pmin(abs(W - A_mat), abs(W - B_mat))
    inside <- (W >= A_mat) & (W <= B_mat)
    dists[inside] <- 0
    dists
  }
  
  D_Xi <- get_dist_mat(Cands, x_L, x_U)
  D_Xbar <- get_dist_mat(Cands, rep(mu_x_L, n), rep(mu_x_U, n))
  DX <- D_Xi - D_Xbar
  D_Yi <- get_dist_mat(Cands, y_L, y_U)
  D_Ybar <- get_dist_mat(Cands, rep(mu_y_L, n), rep(mu_y_U, n))
  DY <- D_Yi - D_Ybar
  
  Terms <- DX * DY
  max_idx <- max.col(abs(Terms), ties.method = "first")
  max_terms <- Terms[cbind(seq_len(n), max_idx)]
  
  cov_H <- if (use_sample_var && n > 1) sum(max_terms) / (n - 1) else sum(max_terms) / n
  cor_H <- cov_H / sqrt(var_x * var_y)
  if (!is.na(cor_H) && abs(cor_H) > 1) cor_H <- sign(cor_H)
  cor_H
}

# MIV correlation
calc_hausdorff_mean_multi <- function(intervals, use_sample_var = FALSE, eps = 1e-6) {
  n <- length(intervals)
  if (n == 0) return(list(mean_L = NA_real_, mean_U = NA_real_, variance = NA_real_))
  
  ranges <- sapply(intervals, interval_total_range)
  r_mean <- mean(ranges)
  all_points <- unlist(lapply(intervals, interval_endpoints))
  
  objective <- function(m) {
    candidate <- c(m - r_mean / 2, m + r_mean / 2)
    mean(sapply(intervals, function(x) hausdorff_dist(candidate, x, eps = eps)^2))
  }
  
  opt <- optimize(objective, interval = c(min(all_points), max(all_points)))
  m_opt <- opt$minimum
  mean_interval <- c(m_opt - r_mean / 2, m_opt + r_mean / 2)
  sq_dists <- sapply(intervals, function(x) hausdorff_dist(mean_interval, x, eps = eps)^2)
  variance <- if (use_sample_var && n > 1) sum(sq_dists) / (n - 1) else sum(sq_dists) / n
  
  list(mean_L = mean_interval[1], mean_U = mean_interval[2], variance = variance)
}

calc_hausdorff_cor_multi <- function(X, Y, use_sample_var = FALSE, eps = 1e-6) {
  n <- length(X)
  if (n != length(Y)) stop("X and Y must have same length")
  if (n < 2) return(NA_real_)
  
  h_x <- calc_hausdorff_mean_multi(X, use_sample_var = use_sample_var, eps = eps)
  h_y <- calc_hausdorff_mean_multi(Y, use_sample_var = use_sample_var, eps = eps)
  x_mean <- c(h_x$mean_L, h_x$mean_U)
  y_mean <- c(h_y$mean_L, h_y$mean_U)
  var_x <- h_x$variance
  var_y <- h_y$variance
  
  if (is.na(var_x) || is.na(var_y) || var_x <= 0 || var_y <= 0) return(NA_real_)
  
  compute_A_omega <- function(omega, x_i, y_i) {
    dx <- point_to_interval_dist(omega, normalize_interval(x_i)) -
      point_to_interval_dist(omega, matrix(x_mean, nrow = 1))
    dy <- point_to_interval_dist(omega, normalize_interval(y_i)) -
      point_to_interval_dist(omega, matrix(y_mean, nrow = 1))
    dx * dy
  }
  
  cov_terms <- sapply(1:n, function(i) {
    candidates <- unique(c(interval_endpoints(X[[i]]), interval_endpoints(Y[[i]]), x_mean, y_mean))
    A_values <- sapply(candidates, function(omega) compute_A_omega(omega, X[[i]], Y[[i]]))
    max_idx <- which.max(abs(A_values))
    sign(A_values[max_idx]) * max(abs(A_values))
  })
  
  cov_H <- if (use_sample_var && n > 1) sum(cov_terms) / (n - 1) else sum(cov_terms) / n
  cor_H <- cov_H / sqrt(var_x * var_y)
  if (!is.na(cor_H) && abs(cor_H) > 1) cor_H <- sign(cor_H)
  cor_H
}

# Unified interface
calc_cor <- function(x, y, method = c("auto", "pearson", "midpoint", "theta", "hausdorff"),
                     verbose = TRUE, use_sample_var = FALSE, degenerate = FALSE, theta = 1, eps = 1e-6) {
  method <- match.arg(method)
  
  type_x <- detect_data_type(x)
  type_y <- detect_data_type(y)
  type_order <- c("sv" = 1, "iv" = 2, "miv" = 3)
  effective_type <- names(type_order)[max(type_order[type_x], type_order[type_y])]
  
  if (method == "auto") {
    method <- get_default_method(effective_type, "correlation")
    if (verbose) message(sprintf("Auto-selected '%s' for %s data", method, effective_type))
  }
  
  compat <- check_method_compatibility(effective_type, method, "correlation")
  if (!compat$compatible && verbose) message(compat$message)
  final_method <- compat$suggested_method
  
  if (effective_type == "sv") {
    x_vals <- if (is.data.frame(x) || is.matrix(x)) x[, 1] else x
    y_vals <- if (is.data.frame(y) || is.matrix(y)) y[, 1] else y
    return(calc_pearson_cor(x_vals, y_vals))
  }
  
  if (effective_type == "iv") {
    x_mat <- standardize_intervals(x)
    y_mat <- standardize_intervals(y)
    if (final_method == "midpoint") return(calc_midpoint_cor(x_mat[,1], x_mat[,2], y_mat[,1], y_mat[,2]))
    if (final_method == "theta") return(calc_theta_cor(x_mat[,1], x_mat[,2], y_mat[,1], y_mat[,2], degenerate = degenerate, theta = theta))
    return(calc_hausdorff_cor(x_mat[,1], x_mat[,2], y_mat[,1], y_mat[,2], use_sample_var = use_sample_var, degenerate = degenerate))
  }
  
  if (effective_type == "miv") {
    x_miv <- standardize_miv(x)
    y_miv <- standardize_miv(y)
    return(calc_hausdorff_cor_multi(x_miv$intervals, y_miv$intervals, use_sample_var = use_sample_var, eps = eps))
  }
  
  NA_real_
}
