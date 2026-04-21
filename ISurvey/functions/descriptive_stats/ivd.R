# descriptive_stats/ivd.R - Interval-Valued Defuzzification
#
# Reference: Zhao et al., IEEE Trans. Fuzzy Syst., under review, 2025.

IVD <- function(x, y, alpha_levels = seq(0, 1, 0.1), method = c("DUW", "DU", "NU", "NUW", "IALC")) {
  method <- match.arg(method)
  
  if (method == "IALC") {
    intervalSet <- AlphaCuts(x, y, alpha_levels)
    list(interval1 = IALC(intervalSet))
  } else if (method %in% c("NU", "NUW")) {
    weight <- ifelse(method == "NUW", "alpha", "equal")
    nested_intervals <- FindNestedIntervals(x, y, alpha_levels)
    aggregated <- NUW(nested_intervals, weight)
    UnionIntervals(aggregated)
  } else {
    weight <- ifelse(method == "DU", "equal", "alpha")
    troughs <- find_trough_points(x, y)
    subsets <- SubsetXandY(x, y, troughs)
    processSubsets(subsets, alpha_levels, weight)
  }
}

AlphaCuts <- function(x, y, alpha_levels) {
  step <- x[2] - x[1]
  tolerance <- .Machine$double.eps^0.5
  
  AlphaCut <- function(alpha) {
    if (alpha == 0) {
      Y0 <- as.integer(y > 0)
      if (!any(Y0 == 1)) return(NULL)
      transitions <- which(diff(c(0, Y0, 0)) != 0)
      starts <- transitions[c(TRUE, FALSE)]
      ends <- transitions[c(FALSE, TRUE)] - 1
      segs <- mapply(function(s, e) {
        L <- x[s] - step / 2; U <- x[e] + step / 2
        L <- max(L, min(x)); U <- min(U, max(x))
        c(L, U)
      }, starts, ends, SIMPLIFY = FALSE)
      segs <- Filter(function(iv) !is.null(iv) && length(iv) == 2 && is.finite(iv[1]) && is.finite(iv[2]) && iv[1] <= iv[2], segs)
      if (length(segs) == 0) return(NULL)
      return(segs)
    }
    
    Y <- ifelse(abs(y - alpha) < tolerance | y >= alpha, alpha, 0)
    transitions <- which(diff(c(0, Y, 0)) != 0)
    starts <- transitions[c(TRUE, FALSE)]
    ends <- transitions[c(FALSE, TRUE)] - 1
    if (length(starts) == 0 || length(ends) == 0) return(NULL)
    mapply(c, x[starts], x[ends], SIMPLIFY = FALSE)
  }
  
  results <- lapply(alpha_levels, AlphaCut)
  names(results) <- as.character(alpha_levels)
  results
}

IALC <- function(intervalSet) {
  aggregatedByAlpha <- list()
  for (alpha in names(intervalSet)) {
    intervals <- intervalSet[[alpha]]
    if (is.null(intervals) || length(intervals) == 0) next
    totalLength <- sum(sapply(intervals, function(iv) if (!is.null(iv) && !any(is.na(iv))) iv[2] - iv[1] else 0))
    if (totalLength > 0) {
      sumStarts <- sumEnds <- 0
      for (iv in intervals) {
        if (!is.null(iv) && !any(is.na(iv))) {
          w <- (iv[2] - iv[1]) / totalLength
          sumStarts <- sumStarts + iv[1] * w
          sumEnds <- sumEnds + iv[2] * w
        }
      }
      aggregatedByAlpha[[alpha]] <- c(sumStarts, sumEnds)
    }
  }
  valid <- Filter(Negate(is.null), aggregatedByAlpha)
  if (length(valid) == 0) return(NULL)
  c(mean(sapply(valid, `[`, 1)), mean(sapply(valid, `[`, 2)))
}

FindNestedIntervals <- function(x, y, alpha_levels) {
  cuts <- AlphaCuts(x, y, alpha_levels)
  isNested <- function(interval, parent) {
    if (is.null(interval) || is.null(parent) || any(is.na(interval)) || any(is.na(parent))) return(FALSE)
    interval[1] >= parent[1] && interval[2] <= parent[2]
  }
  nestedSets <- list()
  if (length(cuts) > 0 && !is.null(cuts[[1]])) {
    for (i in seq_along(cuts[[1]])) nestedSets[[i]] <- list(list(interval = cuts[[1]][[i]], alpha = alpha_levels[1]))
  }
  for (level in 2:length(alpha_levels)) {
    if (is.null(cuts[[level]]) || length(cuts[[level]]) == 0) next
    currentCuts <- cuts[[level]]
    previousSets <- nestedSets
    nestedSets <- list()
    setId <- 1
    for (prevSet in previousSets) {
      lastInterval <- tail(prevSet, 1)[[1]]$interval
      foundNested <- FALSE
      for (interval in currentCuts) {
        if (isNested(interval, lastInterval)) {
          foundNested <- TRUE
          nestedSets[[setId]] <- c(prevSet, list(list(interval = interval, alpha = alpha_levels[level])))
          setId <- setId + 1
        }
      }
      if (!foundNested) { nestedSets[[setId]] <- prevSet; setId <- setId + 1 }
    }
  }
  nestedSets
}

NUW <- function(nestedSets, weight = "alpha") {
  lapply(nestedSets, function(set) {
    weights <- if (weight == "alpha") sapply(set, `[[`, "alpha") else rep(1, length(set))
    starts <- sapply(set, function(s) s$interval[1])
    ends <- sapply(set, function(s) s$interval[2])
    c(sum(weights * starts) / sum(weights), sum(weights * ends) / sum(weights))
  })
}

UnionIntervals <- function(intervals) {
  intervals <- intervals[order(sapply(intervals, `[`, 1))]
  result <- list()
  current <- intervals[[1]]
  for (iv in intervals[-1]) {
    if (iv[1] <= current[2]) { current[2] <- max(current[2], iv[2]) }
    else { result[[length(result) + 1]] <- current; current <- iv }
  }
  result[[length(result) + 1]] <- current
  names(result) <- paste0("interval", seq_along(result))
  result
}

find_trough_points <- function(x, y, xonly = TRUE) {
  get_trend <- function(diff) if (diff > 0) "up" else if (diff < 0) "down" else "flat"
  chunks <- list()
  current <- list(type = NULL, points = list())
  for (i in 2:length(y)) {
    trend <- get_trend(y[i] - y[i - 1])
    if (is.null(current$type)) {
      current$type <- trend
      current$points <- list(c(x[i - 1], y[i - 1]), c(x[i], y[i]))
    } else if (trend == current$type) {
      current$points[[length(current$points) + 1]] <- c(x[i], y[i])
    } else {
      chunks[[length(chunks) + 1]] <- current
      current <- list(type = trend, points = list(c(x[i - 1], y[i - 1]), c(x[i], y[i])))
    }
  }
  if (!is.null(current$type)) chunks[[length(chunks) + 1]] <- current
  
  trough_points <- list()
  for (i in 1:(length(chunks) - 2)) {
    types <- c(chunks[[i]]$type, chunks[[i + 1]]$type, chunks[[i + 2]]$type)
    if (all(types == c("down", "flat", "up"))) {
      flat_pts <- do.call(rbind, chunks[[i + 1]]$points)
      mid_idx <- floor(nrow(flat_pts) / 2) + 1
      trough_points[[length(trough_points) + 1]] <- flat_pts[mid_idx, , drop = FALSE]
    }
  }
  for (i in 1:(length(chunks) - 1)) {
    types <- c(chunks[[i]]$type, chunks[[i + 1]]$type)
    if (all(types == c("down", "up"))) {
      down_pts <- do.call(rbind, chunks[[i]]$points)
      trough_points[[length(trough_points) + 1]] <- down_pts[nrow(down_pts), , drop = FALSE]
    }
  }
  if (length(trough_points) == 0) return(if (xonly) numeric(0) else data.frame(x = numeric(0), y = numeric(0)))
  result <- as.data.frame(do.call(rbind, trough_points))
  colnames(result) <- c("x", "y")
  result <- result[order(result$x), ]
  if (xonly) result$x else result
}

SubsetXandY <- function(x, y, troughs) {
  subsets <- list()
  troughs <- c(troughs, max(x))
  for (i in seq_along(troughs)) {
    mask <- if (i == 1) x <= troughs[i] else x > troughs[i - 1] & x <= troughs[i]
    y_subset <- numeric(length(y))
    y_subset[mask] <- y[mask]
    subsets[[paste0("x", i)]] <- x
    subsets[[paste0("y", i)]] <- y_subset
  }
  subsets
}

processSubsets <- function(subsets, alpha_levels, weight = "equal") {
  results <- list()
  n_subsets <- length(subsets) / 2
  counter <- 1
  for (i in seq_len(n_subsets)) {
    x <- subsets[[paste0("x", i)]]
    y <- subsets[[paste0("y", i)]]
    intervalSet <- AlphaCuts(x, y, alpha_levels)
    agg <- DU(intervalSet, weight)
    if (!is.null(agg)) { results[[paste0("interval", counter)]] <- agg; counter <- counter + 1 }
  }
  results
}

DU <- function(intervalSet, weight = "equal") {
  sumStarts <- sumEnds <- countStarts <- countEnds <- 0
  weightedSumStarts <- weightedSumEnds <- totalWeightStarts <- totalWeightEnds <- 0
  for (alpha in names(intervalSet)) {
    intervals <- intervalSet[[alpha]]
    if (is.null(intervals) || length(intervals) == 0) next
    w <- if (weight == "alpha") as.numeric(alpha) else 1
    for (iv in intervals) {
      if (!is.null(iv)) {
        if (!is.na(iv[1])) {
          if (weight == "alpha") { weightedSumStarts <- weightedSumStarts + iv[1] * w; totalWeightStarts <- totalWeightStarts + w }
          else { sumStarts <- sumStarts + iv[1]; countStarts <- countStarts + 1 }
        }
        if (!is.na(iv[2])) {
          if (weight == "alpha") { weightedSumEnds <- weightedSumEnds + iv[2] * w; totalWeightEnds <- totalWeightEnds + w }
          else { sumEnds <- sumEnds + iv[2]; countEnds <- countEnds + 1 }
        }
      }
    }
  }
  if (weight == "alpha") {
    avgStart <- if (totalWeightStarts > 0) weightedSumStarts / totalWeightStarts else NA
    avgEnd <- if (totalWeightEnds > 0) weightedSumEnds / totalWeightEnds else NA
  } else {
    avgStart <- if (countStarts > 0) sumStarts / countStarts else NA
    avgEnd <- if (countEnds > 0) sumEnds / countEnds else NA
  }
  if (is.na(avgStart) || is.na(avgEnd)) NULL else c(avgStart, avgEnd)
}
