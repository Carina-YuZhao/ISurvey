# data/type_detection.R - Data Type Detection and Conversion

DATA_TYPE_SV <- "sv"
DATA_TYPE_IV <- "iv"
DATA_TYPE_MIV <- "miv"

detect_data_type <- function(x) {
  if (is.null(x)) return(NA_character_)
  
  # MIV: list with intervals/weights or list of matrices
  if (is.list(x) && !is.data.frame(x)) {
    if (all(c("intervals", "weights") %in% names(x))) return(DATA_TYPE_MIV)
    if (length(x) > 0) {
      first <- x[[1]]
      if (is.matrix(first) && ncol(first) == 2) return(DATA_TYPE_MIV)
      if (is.numeric(first) && length(first) == 2) return(DATA_TYPE_MIV)
    }
  }
  
  # IV: matrix/df with 2 columns or L/U columns
  if (is.matrix(x) || is.data.frame(x)) {
    if (ncol(x) == 2) return(DATA_TYPE_IV)
    cols <- tolower(names(x))
    if (any(c("l", "min", "lower") %in% cols) && any(c("u", "max", "upper") %in% cols))
      return(DATA_TYPE_IV)
  }
  
  # SV: numeric vector
  if (is.numeric(x) && is.vector(x)) return(DATA_TYPE_SV)
  if (is.numeric(x) && length(x) == 2) return(DATA_TYPE_IV)
  
  warning("Could not detect data type")
  NA_character_
}

check_method_compatibility <- function(data_type, method, method_category) {
  result <- list(compatible = TRUE, action = "use", suggested_method = method, message = NULL)
  
  if (method_category == "correlation") {
    if (data_type == DATA_TYPE_MIV && method %in% c("theta", "midpoint", "pearson")) {
      result <- list(compatible = FALSE, action = "upgrade", suggested_method = "hausdorff",
                     message = "Multi-interval data: using Hausdorff correlation.")
    } else if (data_type == DATA_TYPE_SV && method %in% c("theta", "hausdorff")) {
      result <- list(compatible = FALSE, action = "downgrade", suggested_method = "pearson",
                     message = "Single-valued data: using Pearson correlation.")
    }
  }
  result
}

standardize_intervals <- function(x) {
  if (is.null(x)) return(NULL)
  
  if (is.matrix(x)) {
    if (ncol(x) == 2) { colnames(x) <- c("L", "U"); return(x) }
    if (ncol(x) == 1) { mat <- cbind(x[,1], x[,1]); colnames(mat) <- c("L", "U"); return(mat) }
    stop("Matrix must have 1 or 2 columns")
  }
  
  if (is.data.frame(x)) {
    if (ncol(x) == 1) { mat <- cbind(x[[1]], x[[1]]); colnames(mat) <- c("L", "U"); return(mat) }
    cols <- names(x)
    cols_lower <- tolower(cols)
    l_idx <- which(cols_lower %in% c("l", "min", "lower", "mean_l"))[1]
    u_idx <- which(cols_lower %in% c("u", "max", "upper", "mean_u"))[1]
    if (is.na(l_idx)) l_idx <- 1
    if (is.na(u_idx)) u_idx <- 2
    mat <- as.matrix(x[, c(l_idx, u_idx)])
    colnames(mat) <- c("L", "U")
    return(mat)
  }
  
  if (is.numeric(x)) {
    if (length(x) == 2 && x[1] <= x[2]) {
      mat <- matrix(x, nrow = 1, ncol = 2)
      colnames(mat) <- c("L", "U")
      return(mat)
    }
    mat <- cbind(x, x)
    colnames(mat) <- c("L", "U")
    return(mat)
  }
  
  stop("Cannot convert to interval format")
}

standardize_miv <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && all(c("intervals", "weights") %in% names(x))) return(x)
  
  if (is.list(x) && !is.data.frame(x)) {
    intervals <- lapply(x, function(m) if (is.matrix(m)) m else matrix(m, ncol = 2, byrow = TRUE))
    return(list(intervals = intervals, weights = rep(1, length(intervals))))
  }
  
  if (is.data.frame(x) || is.matrix(x)) {
    x <- as.matrix(x)
    if (ncol(x) >= 2) {
      intervals <- lapply(seq_len(nrow(x)), function(i) matrix(c(x[i,1], x[i,2]), ncol = 2))
      return(list(intervals = intervals, weights = rep(1, length(intervals))))
    }
  }
  
  if (is.numeric(x) && is.vector(x)) {
    intervals <- lapply(x, function(v) matrix(c(v, v), ncol = 2))
    return(list(intervals = intervals, weights = rep(1, length(intervals))))
  }
  
  stop("Cannot convert to MIV format")
}

get_default_method <- function(data_type, operation) {
  defaults <- list(
    sv = list(correlation = "pearson", ranking = "value", mean = "arithmetic", distance = "euclidean"),
    iv = list(correlation = "theta", ranking = "midpoint", mean = "theta", distance = "theta"),
    miv = list(correlation = "hausdorff", ranking = "weighted_midpoint", mean = "weighted_midpoint", distance = "hausdorff")
  )
  if (!(data_type %in% names(defaults))) return(NA_character_)
  if (!(operation %in% names(defaults[[data_type]]))) return(NA_character_)
  defaults[[data_type]][[operation]]
}
