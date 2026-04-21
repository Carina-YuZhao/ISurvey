# descriptive_stats/arithmetic.R - Interval Arithmetic Operations

interval_subtract <- function(A, B) {
  A <- as_interval_matrix(A)
  B <- as_interval_matrix(B)
  cbind(A[, 1] - B[, 2], A[, 2] - B[, 1])
}

interval_scalar_multiply <- function(A, s) {
  A <- as_interval_matrix(A)
  s <- as.numeric(s)
  L_new <- ifelse(s >= 0, A[, 1] * s, A[, 2] * s)
  U_new <- ifelse(s >= 0, A[, 2] * s, A[, 1] * s)
  cbind(L_new, U_new)
}

# Interval mean (endpoint, theta, or hausdorff)
interval_mean <- function(intervals, method = c("theta", "hausdorff", "endpoint"),
                          theta = 1, use_sample_var = FALSE) {
  method <- match.arg(method)
  intervals <- as_interval_matrix(intervals)
  
  valid <- complete.cases(intervals)
  if (sum(valid) == 0) return(list(L = NA_real_, U = NA_real_, mid = NA_real_, variance = NA_real_))
  intervals <- intervals[valid, , drop = FALSE]
  L <- intervals[, 1]
  U <- intervals[, 2]
  
  if (method == "endpoint") {
    mean_L <- mean(L)
    mean_U <- mean(U)
    list(L = mean_L, U = mean_U, mid = (mean_L + mean_U) / 2, variance = NULL)
  } else if (method == "theta") {
    stats <- calc_theta_stats(L, U, theta = theta, use_sample_var = use_sample_var)
    list(L = stats$mean_L, U = stats$mean_U, mid = (stats$mean_L + stats$mean_U) / 2, variance = stats$variance)
  } else {
    stats <- calc_hausdorff_stats(L, U, use_sample_var = use_sample_var)
    list(L = stats$mean_L, U = stats$mean_U, mid = (stats$mean_L + stats$mean_U) / 2, variance = stats$variance)
  }
}

# Weighted mean with scalar or interval weights
interval_weighted_mean <- function(intervals, weights, 
                                    method = c("theta", "hausdorff", "endpoint"),
                                    theta = 1, normalize = TRUE) {
  method <- match.arg(method)
  intervals <- as_interval_matrix(intervals)
  n <- nrow(intervals)
  
  if (is.matrix(weights) || is.data.frame(weights)) {
    weights <- as.matrix(weights)
    if (ncol(weights) >= 2) {
      return(interval_weighted_mean_iv(intervals, weights, normalize))
    }
    weights <- as.numeric(weights[, 1])
  }
  weights <- as.numeric(weights)
  
  if (length(weights) != n) stop("Weights must match intervals")
  
  valid <- complete.cases(intervals) & !is.na(weights)
  if (sum(valid) == 0) return(list(L = NA_real_, U = NA_real_, mid = NA_real_, is_interval_weights = FALSE))
  
  intervals <- intervals[valid, , drop = FALSE]
  weights <- weights[valid]
  if (normalize) weights <- weights / sum(weights)
  
  L <- intervals[, 1]
  U <- intervals[, 2]
  
  if (method == "theta") {
    mid <- (L + U) / 2
    spr <- (U - L) / 2
    mean_mid <- sum(weights * mid)
    mean_spr <- sum(weights * spr)
    mean_L <- mean_mid - mean_spr
    mean_U <- mean_mid + mean_spr
  } else {
    mean_L <- sum(weights * L)
    mean_U <- sum(weights * U)
  }
  
  list(L = mean_L, U = mean_U, mid = (mean_L + mean_U) / 2, is_interval_weights = FALSE)
}

interval_weighted_mean_iv <- function(intervals, weights, normalize = TRUE) {
  intervals <- as_interval_matrix(intervals)
  weights <- as_interval_matrix(weights)
  n <- nrow(intervals)
  
  if (nrow(weights) != n) stop("Weight intervals must match data intervals")
  
  valid <- complete.cases(intervals) & complete.cases(weights)
  if (sum(valid) == 0) return(list(L = NA_real_, U = NA_real_, mid = NA_real_, is_interval_weights = TRUE))
  
  intervals <- intervals[valid, , drop = FALSE]
  weights <- weights[valid, , drop = FALSE]
  
  sat_L <- intervals[, 1]
  sat_U <- intervals[, 2]
  w_L <- weights[, 1]
  w_U <- weights[, 2]
  
  if (normalize) {
    w_L_norm <- w_L / sum(w_L)
    w_U_norm <- w_U / sum(w_U)
  } else {
    w_L_norm <- w_L
    w_U_norm <- w_U
  }
  
  w_min <- pmin(w_L_norm, w_U_norm)
  w_max <- pmax(w_L_norm, w_U_norm)
  
  mean_L <- sum(sat_L * w_min)
  mean_U <- sum(sat_U * w_max)
  
  list(L = mean_L, U = mean_U, mid = (mean_L + mean_U) / 2, is_interval_weights = TRUE)
}
