# psychometrics/cronbach_alpha.R - Cronbach's Alpha for IV Data
#
# References:
#   Classic: Cronbach, Psychometrika, vol. 16, pp. 297-334, 1951.
#   Hausdorff: Kang et al., Stat. Comput., vol. 35, no. 207, 2025.

calc_alpha <- function(data, method = c("theta", "hausdorff", "classic"),
                       na_action = c("listwise", "pairwise", "mean", "median"),
                       theta = 1, use_sample_var = FALSE) {
  method <- match.arg(method)
  na_action <- match.arg(na_action)
  
  parsed <- parse_alpha_input(data)
  L <- parsed$L; U <- parsed$U; is_sv <- parsed$is_sv
  k <- ncol(L)
  if (k < 2L) return(NA_real_)
  
  if (is_sv && method != "classic") method <- "classic"
  
  result <- apply_na_action_alpha(L, U, na_action)
  L <- result$L; U <- result$U; use_cov <- result$use_cov
  if (nrow(L) < 2) return(NA_real_)
  
  switch(method,
    classic = calc_alpha_classic_internal(L, use_cov),
    theta = calc_alpha_theta_internal(L, U, theta, use_cov),
    hausdorff = calc_alpha_hausdorff_internal(L, U, use_sample_var)
  )
}

parse_alpha_input <- function(data) {
  if (is.list(data) && !is.data.frame(data)) {
    k <- length(data)
    if (k == 0) stop("Empty data list")
    first <- data[[1]]
    n <- if (is.data.frame(first) || is.matrix(first)) nrow(first) else stop("Items must be df/matrix")
    
    L <- U <- matrix(NA_real_, nrow = n, ncol = k)
    for (j in seq_len(k)) {
      item <- data[[j]]
      if (is.data.frame(item)) {
        cols <- tolower(names(item))
        l_idx <- which(cols %in% c("min", "l", "lower"))[1]
        u_idx <- which(cols %in% c("max", "u", "upper"))[1]
        if (is.na(l_idx)) l_idx <- 1
        if (is.na(u_idx)) u_idx <- 2
        L[, j] <- item[[l_idx]]; U[, j] <- item[[u_idx]]
      } else if (is.matrix(item)) {
        L[, j] <- item[, 1]; U[, j] <- item[, 2]
      }
    }
    is_sv <- all(abs(U - L) < 1e-12, na.rm = TRUE)
    return(list(L = L, U = U, is_sv = is_sv))
  }
  
  if (is.matrix(data) || is.data.frame(data)) {
    mat <- as.matrix(data)
    return(list(L = mat, U = mat, is_sv = TRUE))
  }
  stop("Unsupported data format")
}

apply_na_action_alpha <- function(L, U, na_action) {
  k <- ncol(L)
  if (na_action == "listwise") {
    complete_rows <- complete.cases(L) & complete.cases(U)
    L <- L[complete_rows, , drop = FALSE]; U <- U[complete_rows, , drop = FALSE]
    use_cov <- "everything"
  } else if (na_action %in% c("mean", "median")) {
    fn <- if (na_action == "mean") mean else median
    for (j in 1:k) {
      L[is.na(L[,j]), j] <- fn(L[,j], na.rm = TRUE)
      U[is.na(U[,j]), j] <- fn(U[,j], na.rm = TRUE)
    }
    use_cov <- "everything"
  } else {
    use_cov <- "pairwise.complete.obs"
  }
  list(L = L, U = U, use_cov = use_cov)
}

calc_alpha_classic_internal <- function(mat, use_cov) {
  k <- ncol(mat)
  cov_mat <- stats::cov(mat, use = use_cov)
  item_var <- diag(cov_mat)
  total_var <- sum(cov_mat, na.rm = TRUE)
  if (!is.finite(total_var) || total_var <= 0) return(NA_real_)
  as.numeric((k / (k - 1)) * (1 - sum(item_var, na.rm = TRUE) / total_var))
}

calc_alpha_theta_internal <- function(L, U, theta, use_cov) {
  k <- ncol(L)
  mid <- (L + U) / 2; spr <- (U - L) / 2
  cov_theta <- stats::cov(mid, use = use_cov) + theta * stats::cov(spr, use = use_cov)
  item_var <- diag(cov_theta)
  total_var <- sum(cov_theta, na.rm = TRUE)
  if (!is.finite(total_var) || total_var <= 0) return(NA_real_)
  as.numeric((k / (k - 1)) * (1 - sum(item_var, na.rm = TRUE) / total_var))
}

calc_alpha_hausdorff_internal <- function(L, U, use_sample_var) {
  k <- ncol(L)
  complete_rows <- complete.cases(L) & complete.cases(U)
  L <- L[complete_rows, , drop = FALSE]; U <- U[complete_rows, , drop = FALSE]
  if (nrow(L) < 2) return(NA_real_)
  
  item_vars <- vapply(1:k, function(j) calc_hausdorff_stats(L[,j], U[,j], use_sample_var)$variance, numeric(1))
  tot_L <- rowSums(L); tot_U <- rowSums(U)
  tot_var <- calc_hausdorff_stats(tot_L, tot_U, use_sample_var)$variance
  
  if (!is.finite(tot_var) || tot_var <= 0) return(NA_real_)
  as.numeric((k / (k - 1)) * (1 - sum(item_vars, na.rm = TRUE) / tot_var))
}
