# descriptive_stats/utils.R - Interval Utility Functions

# Convert input to interval matrix format
as_interval_matrix <- function(x) {
  if (is.matrix(x)) {
    if (ncol(x) < 2) stop("Matrix must have at least 2 columns")
    result <- x[, 1:2, drop = FALSE]
  } else if (is.data.frame(x)) {
    if ("Min" %in% names(x) && "Max" %in% names(x)) {
      result <- as.matrix(x[, c("Min", "Max")])
    } else if ("L" %in% names(x) && "U" %in% names(x)) {
      result <- as.matrix(x[, c("L", "U")])
    } else if (ncol(x) >= 2) {
      result <- as.matrix(x[, 1:2])
    } else {
      stop("Data frame must have Min/Max or L/U columns, or at least 2 columns")
    }
  } else if (is.numeric(x)) {
    if (length(x) == 2) {
      result <- matrix(x, nrow = 1)
    } else if (length(x) %% 2 == 0) {
      result <- matrix(x, ncol = 2, byrow = TRUE)
    } else {
      stop("Numeric vector must have even length (pairs of L, U)")
    }
  } else if (is.list(x)) {
    # List of intervals
    result <- do.call(rbind, lapply(x, function(iv) {
      if (length(iv) >= 2) c(iv[1], iv[2]) else c(NA, NA)
    }))
  } else {
    stop("Unsupported input type")
  }
  
  colnames(result) <- c("L", "U")
  result
}
