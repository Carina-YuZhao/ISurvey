# ranking/fuzzy_ranking.R - Fuzzy Set Defuzzification
#
# Reference: Leekwijck & Kerre, Fuzzy Sets Syst., vol. 108, pp. 159-178, 1999.

defuzz_centroid <- function(x, y) {
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  sum_y <- sum(y, na.rm = TRUE)
  if (sum_y == 0 || is.na(sum_y)) return(NA_real_)
  sum(x * y, na.rm = TRUE) / sum_y
}

defuzz_mom <- function(x, y, tol = 0.001) {
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  max_y <- max(y, na.rm = TRUE)
  if (is.na(max_y) || max_y == 0) return(NA_real_)
  at_max <- which(abs(y - max_y) <= tol)
  if (length(at_max) == 0) return(NA_real_)
  mean(x[at_max], na.rm = TRUE)
}

defuzz_fom <- function(x, y, tol = 0.001) {
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  max_y <- max(y, na.rm = TRUE)
  if (is.na(max_y) || max_y == 0) return(NA_real_)
  at_max <- which(abs(y - max_y) <= tol)
  if (length(at_max) == 0) return(NA_real_)
  min(x[at_max], na.rm = TRUE)
}

defuzz_lom <- function(x, y, tol = 0.001) {
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  max_y <- max(y, na.rm = TRUE)
  if (is.na(max_y) || max_y == 0) return(NA_real_)
  at_max <- which(abs(y - max_y) <= tol)
  if (length(at_max) == 0) return(NA_real_)
  max(x[at_max], na.rm = TRUE)
}

defuzz_bisector <- function(x, y) {
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  total_area <- sum(y, na.rm = TRUE)
  if (total_area == 0 || is.na(total_area)) return(NA_real_)
  idx <- which(cumsum(y) >= total_area / 2)[1]
  if (is.na(idx)) return(NA_real_)
  x[idx]
}

defuzz_alc <- function(ivd, iaa_x = NULL, iaa_y = NULL) {
  if (is.null(ivd) || length(ivd) == 0) return(NA_real_)
  valid_ivd <- ivd[!sapply(ivd, is.null)]
  if (length(valid_ivd) == 0) return(NA_real_)
  
  if (!is.null(iaa_x) && !is.null(iaa_y)) {
    weights <- sapply(valid_ivd, function(iv) {
      in_range <- iaa_x >= iv[1] & iaa_x <= iv[2]
      if (any(in_range)) max(iaa_y[in_range], na.rm = TRUE) else 0
    })
  } else {
    weights <- rep(1, length(valid_ivd))
  }
  
  midpoints <- sapply(valid_ivd, mean)
  total_weight <- sum(weights, na.rm = TRUE)
  if (total_weight == 0) return(NA_real_)
  sum(midpoints * weights, na.rm = TRUE) / total_weight
}

defuzz_ivd_weighted_mid <- function(ivd, weights = NULL) {
  if (is.null(ivd) || length(ivd) == 0) return(NA_real_)
  valid_ivd <- ivd[!sapply(ivd, is.null)]
  if (length(valid_ivd) == 0) return(NA_real_)
  
  midpoints <- sapply(valid_ivd, mean)
  if (is.null(weights)) return(mean(midpoints, na.rm = TRUE))
  
  weights <- weights[!sapply(ivd, is.null)]
  if (length(weights) != length(midpoints)) return(mean(midpoints, na.rm = TRUE))
  
  total_weight <- sum(weights, na.rm = TRUE)
  if (total_weight == 0) return(mean(midpoints, na.rm = TRUE))
  sum(midpoints * weights, na.rm = TRUE) / total_weight
}

defuzz_ivd_mom <- function(ivd, weights) {
  if (is.null(ivd) || length(ivd) == 0 || is.null(weights) || length(weights) == 0) return(NA_real_)
  valid_idx <- !sapply(ivd, is.null)
  valid_ivd <- ivd[valid_idx]; valid_weights <- weights[valid_idx]
  if (length(valid_ivd) == 0) return(NA_real_)
  mean(valid_ivd[[which.max(valid_weights)]])
}

defuzz <- function(x, y = NULL, method = "centroid", ...) {
  method <- tolower(method)
  
  if (is.numeric(x) && !is.list(x)) {
    if (is.null(y)) stop("y required for IAA defuzzification")
    switch(method,
      "centroid" = , "cog" = defuzz_centroid(x, y),
      "mom" = , "mean_of_max" = defuzz_mom(x, y, ...),
      "fom" = , "first_of_max" = defuzz_fom(x, y, ...),
      "lom" = , "last_of_max" = defuzz_lom(x, y, ...),
      "bisector" = defuzz_bisector(x, y),
      stop("Unknown IAA method: ", method)
    )
  } else if (is.list(x)) {
    weights <- y
    switch(method,
      "alc" = , "alpha_level_centroid" = defuzz_alc(x, ...),
      "weighted_mid" = defuzz_ivd_weighted_mid(x, weights),
      "ivd_mom" = , "mean_of_max" = defuzz_ivd_mom(x, weights),
      "min" = { v <- x[!sapply(x, is.null)]; if (length(v) == 0) NA_real_ else min(sapply(v, `[`, 1)) },
      "max" = { v <- x[!sapply(x, is.null)]; if (length(v) == 0) NA_real_ else max(sapply(v, `[`, 2)) },
      stop("Unknown IVD method: ", method)
    )
  } else {
    stop("x must be numeric (IAA) or list (IVD)")
  }
}
