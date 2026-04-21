# descriptive_stats/distance.R - Interval Distance Metrics
#
# References:
#   Theta: Gil et al., Metrika, vol. 56, pp. 97-111, 2002.
#   Hausdorff: Kang et al., Stat. Comput., vol. 35, no. 207, 2025.

hd_interval <- function(a, b, c, d) {
  pmax(abs(a - c), abs(b - d))
}

# Theta-distance Frechet mean and variance
calc_theta_stats <- function(L, U, theta = 1, use_sample_var = FALSE) {
  valid <- !is.na(L) & !is.na(U)
  L <- L[valid]; U <- U[valid]
  n <- length(L)
  if (n < 1) return(list(mean_L = NA_real_, mean_U = NA_real_, variance = NA_real_))

  mid <- (L + U) / 2
  spr <- (U - L) / 2
  mean_mid <- mean(mid)
  mean_spr <- mean(spr)

  v_mid <- var(mid)
  v_spr <- var(spr)
  if (!use_sample_var && n > 1) {
    v_mid <- v_mid * (n - 1) / n
    v_spr <- v_spr * (n - 1) / n
  }
  if (is.na(v_spr)) v_spr <- 0

  list(mean_L = mean_mid - mean_spr, mean_U = mean_mid + mean_spr,
       variance = v_mid + theta * v_spr)
}

# Hausdorff-distance Frechet mean and variance
calc_hausdorff_stats <- function(L, U, use_sample_var = FALSE) {
  valid <- !is.na(L) & !is.na(U)
  L <- L[valid]; U <- U[valid]
  n <- length(L)
  if (n < 1) return(list(mean_L = NA_real_, mean_U = NA_real_, variance = NA_real_))

  avg_len <- mean(U - L)
  objective <- function(m) {
    mean(hd_interval(L, U, m - avg_len/2, m + avg_len/2)^2)
  }

  opt <- optimize(objective, interval = c(min(L), max(U)))
  m_opt <- opt$minimum
  variance <- opt$objective
  if (use_sample_var && n > 1) variance <- variance * n / (n - 1)

  list(mean_L = m_opt - avg_len/2, mean_U = m_opt + avg_len/2, variance = variance)
}

# Multi-segment utilities
merge_segments <- function(seg) {
  if (is.null(seg) || nrow(seg) == 0) return(matrix(numeric(0), ncol = 2))
  seg <- seg[order(seg[, 1], seg[, 2]), , drop = FALSE]
  if (nrow(seg) == 1) return(seg)
  
  out <- seg[1, , drop = FALSE]
  for (i in 2:nrow(seg)) {
    last <- nrow(out)
    if (seg[i, 1] <= out[last, 2]) {
      out[last, 2] <- max(out[last, 2], seg[i, 2])
    } else {
      out <- rbind(out, seg[i, , drop = FALSE])
    }
  }
  out
}

dilate_segments <- function(seg, r) {
  if (is.null(seg) || nrow(seg) == 0) return(matrix(numeric(0), ncol = 2))
  dil <- cbind(seg[, 1] - r, seg[, 2] + r)
  merge_segments(dil)
}

is_subset_segments <- function(A, B) {
  A <- merge_segments(A)
  B <- merge_segments(B)
  if (nrow(A) == 0) return(TRUE)
  if (nrow(B) == 0) return(FALSE)
  
  j <- 1
  for (i in 1:nrow(A)) {
    aL <- A[i, 1]; aU <- A[i, 2]
    while (j <= nrow(B) && B[j, 2] < aL) j <- j + 1
    if (j > nrow(B)) return(FALSE)
    if (B[j, 1] > aL) return(FALSE)
    if (B[j, 2] < aU) return(FALSE)
  }
  TRUE
}

# Hausdorff distance for multi-segment intervals (Algorithm 1, Kang et al. 2025)
hausdorff_multiseg_algo1 <- function(A, B, eps = 1e-6, max_iter = 100) {
  A <- merge_segments(A)
  B <- merge_segments(B)
  
  all_endpoints <- c(A[, 1], A[, 2], B[, 1], B[, 2])
  r1 <- max(all_endpoints) - min(all_endpoints)
  r0 <- r1
  
  iter <- 0
  while (r0 > eps && iter < max_iter) {
    r0 <- r0 / 2
    iter <- iter + 1
    
    Br1 <- dilate_segments(B, r1)
    Ar1 <- dilate_segments(A, r1)
    
    if (is_subset_segments(A, Br1) && is_subset_segments(B, Ar1)) {
      r1 <- r1 - r0
    } else {
      r1 <- r1 + r0
    }
  }
  r1
}
