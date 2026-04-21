# applied/csi.R - Customer Satisfaction Index
#
# Methods:
#   iwa: Interval Weighted Average with Karnik-Mendel algorithm (Liu 2008)
#   arithmetic: Separate L/U normalization

.km_lower <- function(A_L, w_L, w_U) {
  n <- length(A_L)
  ord <- order(A_L)
  A_sorted <- A_L[ord]; wL_sorted <- w_L[ord]; wU_sorted <- w_U[ord]
  
  w_current <- wU_sorted
  y <- sum(A_sorted * w_current) / sum(w_current)
  
  for (iter in 1:n) {
    k <- sum(A_sorted <= y)
    if (k == 0) w_new <- wL_sorted
    else if (k == n) w_new <- wU_sorted
    else w_new <- c(wU_sorted[1:k], wL_sorted[(k+1):n])
    
    y_new <- sum(A_sorted * w_new) / sum(w_new)
    if (abs(y_new - y) < 1e-10) {
      w_opt <- numeric(n); w_opt[ord] <- w_new
      return(list(value = y_new, weights = w_opt))
    }
    w_current <- w_new; y <- y_new
  }
  w_opt <- numeric(n); w_opt[ord] <- w_current
  list(value = y, weights = w_opt)
}

.km_upper <- function(A_U, w_L, w_U) {
  n <- length(A_U)
  ord <- order(A_U)
  A_sorted <- A_U[ord]; wL_sorted <- w_L[ord]; wU_sorted <- w_U[ord]
  
  w_current <- wL_sorted
  y <- sum(A_sorted * w_current) / sum(w_current)
  
  for (iter in 1:n) {
    k <- sum(A_sorted <= y)
    if (k == 0) w_new <- wU_sorted
    else if (k == n) w_new <- wL_sorted
    else w_new <- c(wL_sorted[1:k], wU_sorted[(k+1):n])
    
    y_new <- sum(A_sorted * w_new) / sum(w_new)
    if (abs(y_new - y) < 1e-10) {
      w_opt <- numeric(n); w_opt[ord] <- w_new
      return(list(value = y_new, weights = w_opt))
    }
    w_current <- w_new; y <- y_new
  }
  w_opt <- numeric(n); w_opt[ord] <- w_current
  list(value = y, weights = w_opt)
}

calc_iwa <- function(A_L, A_U, w_L, w_U) {
  n <- length(A_L)
  if (length(A_U) != n || length(w_L) != n || length(w_U) != n) stop("All inputs must have same length")
  if (n == 0) return(list(L = NA_real_, U = NA_real_, weights_L = NULL, weights_U = NULL))
  if (n == 1) return(list(L = A_L[1], U = A_U[1], weights_L = w_L, weights_U = w_U))
  
  result_L <- .km_lower(A_L, w_L, w_U)
  result_U <- .km_upper(A_U, w_L, w_U)
  list(L = result_L$value, U = result_U$value, weights_L = result_L$weights, weights_U = result_U$weights)
}

calc_csi <- function(importance, satisfaction, labels = NULL,
                     method = c("theta", "hausdorff"),
                     csi_method = c("iwa", "arithmetic"), theta = 1) {
  method <- match.arg(method)
  csi_method <- match.arg(csi_method)
  
  imp_info <- parse_sv_or_iv(importance, method)
  sat_info <- parse_sv_or_iv(satisfaction, method)
  
  is_iv <- imp_info$is_iv || sat_info$is_iv
  n <- imp_info$n
  if (n != sat_info$n) stop("importance and satisfaction must have same length")
  if (is.null(labels)) labels <- paste0("Item", 1:n)
  
  if (is_iv) {
    valid <- complete.cases(cbind(imp_info$L, imp_info$U, sat_info$L, sat_info$U))
  } else {
    valid <- !is.na(imp_info$values) & !is.na(sat_info$values)
  }
  if (sum(valid) == 0) return(list(CSI = NA_real_, CSI_L = NA_real_, CSI_U = NA_real_, details = NULL))
  labels <- labels[valid]
  
  if (!is_iv) {
    imp <- imp_info$values[valid]; sat <- sat_info$values[valid]
    weights <- imp / sum(imp)
    contribution <- weights * sat
    details <- data.frame(Aspect = labels, Importance = imp, Weight = weights,
                          Satisfaction = sat, Contribution = contribution, stringsAsFactors = FALSE)
    return(list(CSI = sum(contribution), details = details, method = method, csi_method = "sv"))
  }
  
  imp_L <- imp_info$L[valid]; imp_U <- imp_info$U[valid]
  sat_L <- sat_info$L[valid]; sat_U <- sat_info$U[valid]
  
  if (csi_method == "iwa") {
    result <- calc_iwa(sat_L, sat_U, imp_L, imp_U)
    w_norm_L <- result$weights_L / sum(result$weights_L)
    w_norm_U <- result$weights_U / sum(result$weights_U)
    details <- data.frame(
      Aspect = labels, Importance_L = imp_L, Importance_U = imp_U,
      Weight_Opt_L = result$weights_L, Weight_Opt_U = result$weights_U,
      Weight_Norm_L = w_norm_L, Weight_Norm_U = w_norm_U,
      Satisfaction_L = sat_L, Satisfaction_U = sat_U,
      Contribution_L = sat_L * w_norm_L, Contribution_U = sat_U * w_norm_U,
      stringsAsFactors = FALSE
    )
    return(list(CSI_L = result$L, CSI_U = result$U, details = details, method = method, csi_method = "iwa"))
  }
  
  # arithmetic method
  w_L <- imp_L / sum(imp_L); w_U <- imp_U / sum(imp_U)
  contribution_L <- w_L * sat_L; contribution_U <- w_U * sat_U
  details <- data.frame(
    Aspect = labels, Importance_L = imp_L, Importance_U = imp_U,
    Weight_L = w_L, Weight_U = w_U, Satisfaction_L = sat_L, Satisfaction_U = sat_U,
    Contribution_L = contribution_L, Contribution_U = contribution_U,
    stringsAsFactors = FALSE
  )
  list(CSI_L = sum(contribution_L), CSI_U = sum(contribution_U), details = details, method = method, csi_method = "arithmetic")
}
