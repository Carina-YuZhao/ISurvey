# applied/gap.R - Gap Analysis
#
# Gap = Perception - Expectation (interval subtraction for IV data)

calc_gap <- function(expectation, perception, labels = NULL, method = c("theta", "hausdorff")) {
  method <- match.arg(method)
  
  exp_info <- parse_sv_or_iv(expectation, method)
  per_info <- parse_sv_or_iv(perception, method)
  
  is_iv <- exp_info$is_iv || per_info$is_iv
  n <- exp_info$n
  if (n != per_info$n) stop("expectation and perception must have same length")
  if (is.null(labels)) labels <- paste0("Item", 1:n)
  
  if (!is_iv) {
    gap <- per_info$values - exp_info$values
    data.frame(Aspect = labels, Perception = per_info$values, Expectation = exp_info$values,
               Gap = gap, stringsAsFactors = FALSE)
  } else {
    gap_L <- per_info$L - exp_info$U
    gap_U <- per_info$U - exp_info$L
    data.frame(
      Aspect = labels,
      Perception_L = per_info$L, Perception_U = per_info$U,
      Perception_Mid = (per_info$L + per_info$U) / 2,
      Expectation_L = exp_info$L, Expectation_U = exp_info$U,
      Expectation_Mid = (exp_info$L + exp_info$U) / 2,
      Gap_L = gap_L, Gap_U = gap_U, Gap_Mid = (gap_L + gap_U) / 2,
      stringsAsFactors = FALSE
    )
  }
}

parse_sv_or_iv <- function(x, method = "theta") {
  if (is.data.frame(x) && "Item" %in% names(x)) {
    has_theta <- "Theta_Mean_L" %in% names(x)
    has_haus <- "Haus_Mean_L" %in% names(x)
    has_sv <- "SV_Mean" %in% names(x)
    
    if (has_theta || has_haus || has_sv) {
      if (has_theta || has_haus) {
        if (method == "theta" && has_theta) {
          L <- x$Theta_Mean_L; U <- x$Theta_Mean_U
        } else if (method == "hausdorff" && has_haus) {
          L <- x$Haus_Mean_L; U <- x$Haus_Mean_U
        } else if (has_theta) {
          L <- x$Theta_Mean_L; U <- x$Theta_Mean_U
        } else {
          L <- x$Haus_Mean_L; U <- x$Haus_Mean_U
        }
        is_iv <- !all(abs(L - U) < 1e-12, na.rm = TRUE)
        sv_values <- if (has_sv) x$SV_Mean else rep(NA_real_, nrow(x))
        return(list(is_iv = is_iv, n = nrow(x), L = L, U = U,
                    values = if (!is_iv) L else NULL, sv_values = sv_values))
      } else {
        L <- x$SV_Mean; U <- x$SV_Mean
        return(list(is_iv = FALSE, n = nrow(x), L = L, U = U, values = L, sv_values = x$SV_Mean))
      }
    }
  }
  
  if (is.data.frame(x) || is.matrix(x)) {
    x <- as.matrix(x)
    if (ncol(x) >= 2) {
      is_iv <- !all(abs(x[,1] - x[,2]) < 1e-12, na.rm = TRUE)
      return(list(is_iv = is_iv, n = nrow(x), L = x[,1], U = x[,2],
                  values = if (!is_iv) x[,1] else NULL, sv_values = NULL))
    }
    return(list(is_iv = FALSE, n = nrow(x), L = x[,1], U = x[,1], values = x[,1], sv_values = NULL))
  }
  
  x <- as.numeric(x)
  list(is_iv = FALSE, n = length(x), L = x, U = x, values = x, sv_values = NULL)
}
