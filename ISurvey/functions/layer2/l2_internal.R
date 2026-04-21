# ==============================================================================
# l2_internal.R - Internal Helper Functions for Layer 2 API
# ==============================================================================
#
# Internal data processing functions. Plotting helpers are in viz/plot_helpers.R.
#
# Functions:
#   NA Handling: impute_mean()
#   Data Building: build_iv_items(), build_sv_matrix()
#   Computation: compute_mean()
#   Stats Extraction: extract_stats(), stats_to_viz()
#   IAA/IVD: compute_iaa(), denoise_iaa(), denoise_runmed(), denoise_ma(),
#            denoise_savgol(), compute_ivd(), compute_rfh()
#   Data Preparation: prepare_ivd_summary_data()
# ==============================================================================

# ==============================================================================
# NA HANDLING (Internal)
# ==============================================================================

#' Impute NA values with column means
impute_mean <- function(mat) {
  for (j in 1:ncol(mat)) {
    col_mean <- mean(mat[, j], na.rm = TRUE)
    mat[is.na(mat[, j]), j] <- col_mean
  }
  mat
}

# ==============================================================================
# DATA BUILDING (Internal)
# ==============================================================================

#' Build IV items list for calc_alpha
build_iv_items <- function(data, items, schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  
  all_parts <- NULL
  for (item in items) {
    iv_df <- extract_iv(data, item, schema)
    all_parts <- union(all_parts, iv_df$Participant)
  }
  all_parts <- sort(all_parts)
  n <- length(all_parts)
  
  result <- lapply(items, function(item) {
    iv_df <- extract_iv(data, item, schema)
    min_vals <- rep(NA_real_, n)
    max_vals <- rep(NA_real_, n)
    idx <- match(iv_df$Participant, all_parts)
    min_vals[idx] <- iv_df$Min
    max_vals[idx] <- iv_df$Max
    data.frame(Min = min_vals, Max = max_vals)
  })
  names(result) <- items
  result
}

#' Build SV matrix for calc_alpha
build_sv_matrix <- function(data, items, standardize = NULL, schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  
  all_parts <- NULL
  for (item in items) {
    sv_df <- extract_sv(data, item, standardize = standardize, schema = schema)
    all_parts <- union(all_parts, sv_df$Participant)
  }
  all_parts <- sort(all_parts)
  n <- length(all_parts)
  k <- length(items)
  
  mat <- matrix(NA_real_, nrow = n, ncol = k)
  rownames(mat) <- all_parts
  colnames(mat) <- items
  
  for (j in seq_along(items)) {
    sv_df <- extract_sv(data, items[j], standardize = standardize, schema = schema)
    idx <- match(sv_df$Participant, all_parts)
    mat[idx, j] <- sv_df$Value
  }
  
  mat
}

# ==============================================================================
# COMPUTATION HELPERS (Internal)
# ==============================================================================

#' Compute mean interval from IV data
compute_mean <- function(iv_df, method = c("theta", "hausdorff")) {
  method <- match.arg(method)
  if (nrow(iv_df) == 0) return(list(L = NA, U = NA, mid = NA, var = NA, n = 0))
  
  intervals <- cbind(iv_df$Min, iv_df$Max)
  result <- interval_mean(intervals, method = method)
  
  list(L = result$L, U = result$U, mid = result$mid, 
       var = result$variance, n = nrow(iv_df))
}

# ==============================================================================
# STATS EXTRACTION HELPERS (Internal)
# ==============================================================================

#' Extract interval or scalar data from calc_descriptives() bundle
extract_stats <- function(stats,
                          what = c("theta", "haus", "sv",
                                   "theta_mean", "haus_mean", "sv_mean",
                                   "hausdorff", "theta_var", "haus_var", "sv_var"),
                          as_matrix = FALSE) {
  what <- match.arg(what)
  
  what_norm <- switch(what,
                      "theta" = "theta_mean",
                      "haus" = "haus_mean",
                      "hausdorff" = "haus_mean",
                      "sv" = "sv_mean",
                      what)
  
  get_from_bundle <- function(key) {
    if (is.null(stats[[key]])) {
      stop("Requested data not available in bundle: ", key)
    }
    stats[[key]]
  }
  
  if (what_norm == "theta_mean") {
    res <- get_from_bundle("theta")
    if (as_matrix) res <- as.matrix(res)
    return(res)
  }
  if (what_norm == "haus_mean") {
    res <- get_from_bundle("hausdorff")
    if (as_matrix) res <- as.matrix(res)
    return(res)
  }
  if (what_norm == "sv_mean") return(get_from_bundle("sv"))
  if (what_norm == "theta_var") return(get_from_bundle("theta_var"))
  if (what_norm == "haus_var") return(get_from_bundle("haus_var"))
  if (what_norm == "sv_var") return(get_from_bundle("sv_var"))
  
  stop("Unknown 'what' value: ", what_norm)
}

#' Convert bundle to viz format
stats_to_viz <- function(stats) {
  .stats_to_viz_internal(stats)
}

# ==============================================================================
# IAA/IVD COMPUTATION (Internal)
# ==============================================================================

#' Compute IAA from IV data
compute_iaa <- function(iv_df, domain = c(1, 5), step = 0.01) {
  if (is.null(iv_df) || nrow(iv_df) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  IAA(iv_df, domain, step)
}

#' Denoise IAA curve
denoise_iaa <- function(iaa_df, method = c("runmed", "ma", "savgol"), ...) {
  method <- match.arg(method)
  
  if (is.null(iaa_df) || nrow(iaa_df) == 0) {
    return(iaa_df)
  }
  
  y_denoised <- switch(method,
                       "runmed" = denoise_runmed(iaa_df$y, ...),
                       "ma" = denoise_ma(iaa_df$y, ...),
                       "savgol" = denoise_savgol(iaa_df$y, ...))
  
  data.frame(x = iaa_df$x, y = y_denoised)
}

#' Running median denoising
denoise_runmed <- function(y, k = 51, ...) {
  n <- length(y)
  
  # Ensure k is odd
  if (k %% 2 == 0) k <- k + 1
  
  # Guard: k must be <= n and >= 3
  if (k > n) k <- if (n %% 2 == 1) n else n - 1
  if (k < 3) return(y)
  
  stats::runmed(y, k = k, algorithm = "Turlach", endrule = "constant")
}

#' Moving average denoising
denoise_ma <- function(y, k = 51, ...) {
  n <- length(y)
  
  if (k > n) k <- n
  if (k < 1) return(y)
  
  # Use filter for moving average
  y_smooth <- stats::filter(y, rep(1/k, k), sides = 2)
  
  # Handle edge NAs with progressively smaller windows
  half_k <- floor(k / 2)
  for (i in 1:half_k) {
    if (is.na(y_smooth[i])) {
      y_smooth[i] <- mean(y[1:(2*i - 1)], na.rm = TRUE)
    }
    if (is.na(y_smooth[n - i + 1])) {
      y_smooth[n - i + 1] <- mean(y[(n - 2*i + 2):n], na.rm = TRUE)
    }
  }
  
  as.numeric(y_smooth)
}

#' Savitzky-Golay filter denoising
denoise_savgol <- function(y, k = 51, p = 3) {
  n <- length(y)
  
  # Ensure k is odd
  if (k %% 2 == 0) k <- k + 1
  
  # Guard: k must be <= n
  if (k > n) k <- if (n %% 2 == 1) n else n - 1
  if (k < 3) return(y)
  
  # p must be < k
  if (p >= k) p <- k - 1
  if (p < 1) p <- 1
  
  # Compute Savitzky-Golay coefficients
  half_k <- (k - 1) / 2
  j <- (-half_k):half_k
  
  # Vandermonde matrix
  J <- outer(j, 0:p, "^")
  
  # Coefficients for smoothing (0th derivative)
  coeffs <- tryCatch({
    solve(t(J) %*% J) %*% t(J)
  }, error = function(e) {
    # Fallback to moving average if matrix is singular
    return(NULL)
  })
  
  if (is.null(coeffs)) {
    return(denoise_ma(y, k))
  }
  
  sg_coeffs <- coeffs[1, ]
  
  # Apply filter
  y_smooth <- rep(NA_real_, n)
  
  for (i in 1:n) {
    start_idx <- i - half_k
    end_idx <- i + half_k
    
    if (start_idx >= 1 && end_idx <= n) {
      # Full window available
      y_smooth[i] <- sum(sg_coeffs * y[start_idx:end_idx])
    } else {
      # Edge: use available data with reduced window
      actual_start <- max(1, start_idx)
      actual_end <- min(n, end_idx)
      y_smooth[i] <- mean(y[actual_start:actual_end])
    }
  }
  
  y_smooth
}

#' Compute IVD from denoised IAA
compute_ivd <- function(iaa_df, method = "DUW", alpha_levels = seq(0, 1, 0.1)) {
  if (is.null(iaa_df) || nrow(iaa_df) == 0) return(NULL)
  IVD(iaa_df$x, iaa_df$y, alpha_levels = alpha_levels, method = method)
}

#' Compute RFH from SV data
compute_rfh <- function(sv_df, domain = c(1, 5)) {
  if (is.null(sv_df) || nrow(sv_df) == 0) {
    return(data.frame(Rating = integer(0), Count = integer(0), RelativeFreq = numeric(0)))
  }
  
  ratings <- domain[1]:domain[2]
  counts <- table(factor(sv_df$Value, levels = ratings))
  data.frame(
    Rating = as.integer(names(counts)),
    Count = as.integer(counts),
    RelativeFreq = as.numeric(counts) / sum(counts)
  )
}

# ==============================================================================
# IVD SUMMARY DATA PREPARATION
# ==============================================================================

#' Prepare data for IVD summary plot
#'
#' Extracts IV data, computes IAA/IVD, and prepares segments for plotting.
#' This is the data preparation step, separated from plotting for modularity.
#'
#' @param data1 First data source
#' @param data2 Optional second data source
#' @param items Character vector of item names
#' @param domain Scale domain c(min, max)
#' @param ivd_method IVD method
#' @param denoise_method Denoising method
#' @param denoise_k Denoising window size
#' @param denoise_p Denoising polynomial order (for savgol)
#' @param sort_by Sorting method
#' @param sort_order Sort order ("desc" or "asc")
#' @param schema Optional schema
#'
#' @return List with: segments (data.frame), ordered_items, item_order, 
#'         sort_values, defuzz_values, y_pos_map
prepare_ivd_summary_data <- function(data1, data2 = NULL, items, 
                                      domain = c(1, 5),
                                      ivd_method = "DUW",
                                      denoise_method = "runmed", 
                                      denoise_k = 51, denoise_p = 3,
                                      sort_by = "none",
                                      sort_order = "desc",
                                      schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  palette <- default_ivd()$palette
  breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0001)
  
  # Normalize items
  if (is.list(items) && !is.data.frame(items)) {
    all_items <- unique(unlist(items))
  } else {
    all_items <- items
  }
  
  # Data sources
  data_sources <- list(data1)
  if (!is.null(data2)) {
    data_sources <- list(data1, data2)
  }
  
  segments <- list()
  sort_values <- list()
  defuzz_values <- list()
  item_order <- list()
  
  for (idx in seq_along(all_items)) {
    item <- all_items[idx]
    iv_df <- extract_iv_multi(data_sources, item, schema)
    if (nrow(iv_df) == 0) next
    
    iaa <- compute_iaa(iv_df, domain)
    iaa_d <- denoise_iaa(iaa, method = denoise_method, k = denoise_k, p = denoise_p)
    ivd <- compute_ivd(iaa_d, method = ivd_method)
    
    if (is.null(ivd)) next
    
    item_order[[item]] <- idx
    
    ivd_intervals <- list()
    ivd_weights <- c()
    
    for (j in seq_along(ivd)) {
      if (is.null(ivd[[j]])) next
      
      in_iv <- iaa$x >= ivd[[j]][1] & iaa$x <= ivd[[j]][2]
      max_val <- if (any(in_iv)) max(iaa$y[in_iv], na.rm = TRUE) else 0
      
      ivd_intervals[[length(ivd_intervals) + 1]] <- ivd[[j]]
      ivd_weights <- c(ivd_weights, max_val)
      
      bin_idx <- pmax(1, pmin(length(palette), findInterval(max_val/0.6, breaks, rightmost.closed = TRUE)))
      
      segments[[length(segments) + 1]] <- data.frame(
        item = item, x = ivd[[j]][1], xend = ivd[[j]][2], color = palette[bin_idx],
        max_val = max_val, stringsAsFactors = FALSE
      )
    }
    
    # Calculate sort and defuzz values using fuzzy_ranking functions
    defuzz_val <- NA_real_
    if (sort_by == "centroid") {
      defuzz_val <- defuzz_centroid(iaa$x, iaa$y)
      sort_val <- defuzz_val
    } else if (sort_by == "mom") {
      defuzz_val <- defuzz_mom(iaa$x, iaa$y)
      sort_val <- defuzz_val
    } else if (sort_by == "bisector") {
      defuzz_val <- defuzz_bisector(iaa$x, iaa$y)
      sort_val <- defuzz_val
    } else if (sort_by == "alc") {
      defuzz_val <- defuzz_alc(ivd, iaa$x, iaa$y)
      sort_val <- defuzz_val
    } else if (length(ivd_intervals) > 0 && sum(ivd_weights) > 0) {
      weights_norm <- ivd_weights / sum(ivd_weights)
      max_idx <- which.max(ivd_weights)
      dominant_interval <- ivd_intervals[[max_idx]]
      
      if (sort_by == "mean_of_max") {
        defuzz_val <- mean(dominant_interval)
        sort_val <- defuzz_val
      } else if (sort_by == "weighted_mid") {
        defuzz_val <- defuzz_ivd_weighted_mid(ivd_intervals, ivd_weights)
        sort_val <- defuzz_val
      } else if (sort_by == "min") {
        defuzz_val <- dominant_interval[1]
        sort_val <- defuzz_val
      } else if (sort_by == "max") {
        defuzz_val <- dominant_interval[2]
        sort_val <- defuzz_val
      } else {
        sort_val <- idx
        defuzz_val <- defuzz_centroid(iaa$x, iaa$y)
      }
    } else {
      sort_val <- idx
      defuzz_val <- defuzz_centroid(iaa$x, iaa$y)
    }
    sort_values[[item]] <- sort_val
    defuzz_values[[item]] <- defuzz_val
  }
  
  if (length(segments) == 0) {
    return(list(segments = NULL, ordered_items = character(0)))
  }
  
  df <- do.call(rbind, segments)
  
  # Apply sorting
  unique_items <- unique(df$item)
  if (sort_by == "none") {
    sort_vec <- sapply(unique_items, function(it) {
      idx <- item_order[[it]]
      if (is.null(idx)) which(unique_items == it) else idx
    })
    ordered_items <- unique_items[order(sort_vec)]
  } else if (length(sort_values) > 0) {
    sort_vec <- sapply(unique_items, function(it) {
      sv <- sort_values[[it]]
      if (is.null(sv)) 0 else sv
    })
    ordered_items <- unique_items[order(sort_vec, decreasing = (sort_order == "desc"))]
  } else {
    ordered_items <- unique_items
  }
  
  # Create y position mapping
  y_pos_map <- setNames(seq_along(ordered_items), ordered_items)
  df$y_pos <- y_pos_map[df$item]
  
  list(
    segments = df,
    ordered_items = ordered_items,
    item_order = item_order,
    sort_values = sort_values,
    defuzz_values = defuzz_values,
    y_pos_map = y_pos_map
  )
}
