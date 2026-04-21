# ranking/interval_ranking.R - Interval Ranking
#
# References:
#   Wang & Yang, J. Comput. Appl. Math., vol. 198, pp. 253-267, 2007. (Hurwicz)
#   Moore, Interval Analysis, Prentice-Hall, 1966.

rank_intervals <- function(intervals, method = c("midpoint", "min", "max", "hurwicz"),
                           decreasing = TRUE, ...) {
  method <- match.arg(method)
  parsed <- parse_interval_bounds(intervals)
  L <- parsed$L; U <- parsed$U
  
  scores <- switch(method,
    "midpoint" = (L + U) / 2,
    "min" = L,
    "max" = U,
    "hurwicz" = score_hurwicz(L, U, ...)
  )
  rank(if (decreasing) -scores else scores, ties.method = "first", na.last = "keep")
}

order_intervals <- function(intervals, method = c("midpoint", "min", "max", "hurwicz"),
                            decreasing = TRUE, ...) {
  method <- match.arg(method)
  parsed <- parse_interval_bounds(intervals)
  L <- parsed$L; U <- parsed$U
  
  scores <- switch(method,
    "midpoint" = (L + U) / 2,
    "min" = L,
    "max" = U,
    "hurwicz" = score_hurwicz(L, U, ...)
  )
  order(scores, decreasing = decreasing, na.last = TRUE)
}

compare_intervals <- function(a, b, method = c("midpoint", "hurwicz"), ...) {
  method <- match.arg(method)
  intervals <- rbind(c(a[1], a[2]), c(b[1], b[2]))
  parsed <- parse_interval_bounds(intervals)
  
  scores <- switch(method,
    "midpoint" = (parsed$L + parsed$U) / 2,
    "hurwicz" = score_hurwicz(parsed$L, parsed$U, ...)
  )
  scores[1] - scores[2]
}

score_hurwicz <- function(L, U, gamma = 0.5) {
  if (gamma < 0 || gamma > 1) { warning("gamma clamped to [0,1]"); gamma <- max(0, min(1, gamma)) }
  gamma * U + (1 - gamma) * L
}

parse_interval_bounds <- function(intervals) {
  if (is.data.frame(intervals)) {
    if ("Min" %in% names(intervals) && "Max" %in% names(intervals)) {
      L <- intervals$Min; U <- intervals$Max
    } else if ("L" %in% names(intervals) && "U" %in% names(intervals)) {
      L <- intervals$L; U <- intervals$U
    } else {
      L <- intervals[,1]; U <- intervals[,2]
    }
  } else if (is.matrix(intervals)) {
    L <- intervals[,1]; U <- intervals[,2]
  } else if (is.numeric(intervals) && length(intervals) == 2) {
    L <- intervals[1]; U <- intervals[2]
  } else {
    stop("intervals must be matrix, data.frame, or length-2 vector")
  }
  list(L = as.numeric(L), U = as.numeric(U))
}

get_sort_value <- function(L, U, method, gamma = 0.5) {
  if (is.na(L) || is.na(U)) return(NA_real_)
  switch(method,
    "min" = L, "max" = U, "mid" = (L + U) / 2, "midpoint" = (L + U) / 2,
    "hurwicz" = score_hurwicz(L, U, gamma), NA_real_
  )
}

apply_sort_order <- function(vars, sort_values, sort_by, sort_order) {
  if (sort_by == "none" || length(sort_values) == 0) return(vars)
  sv <- sapply(vars, function(v) { val <- sort_values[[v]]; if (is.null(val) || is.na(val)) 0 else val })
  vars[order(sv, decreasing = (sort_order == "desc"))]
}
