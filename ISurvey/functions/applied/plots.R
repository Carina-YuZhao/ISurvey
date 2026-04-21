# ==============================================================================
# plots.R - Visualization Functions for Interval-Valued Data
# ==============================================================================
#
# Main plotting functions for analysis results:
#   - plot_means(): Horizontal interval means plot
#   - plot_correlation(): Scatter plot with IV/SV correlation
#   - plot_ipa(): Importance-Performance Analysis
#   - plot_gap(): Gap analysis (Perception vs Expectation)
#   - plot_ivd_summary_core(): IVD summary visualization
#
# Helper functions for batch plotting (IAA, RFH, IVD) are in plot_helpers.R
# ==============================================================================

# Internal helper: Convert bundle format to viz format for plotting
.stats_to_viz_internal <- function(stats) {
  # Expect bundle format (list with $theta, $sv, etc.)
  if (!is.list(stats) || is.data.frame(stats)) {
    stop("Expected bundle format from calc_descriptives()")
  }
  if (!any(c("theta", "hausdorff", "sv") %in% names(stats))) {
    stop("Bundle must contain at least one of: theta, hausdorff, sv")
  }
  
  items <- stats$items
  result_list <- list()
  
  # SV mean reference (NA if missing)
  sv_mean_ref <- if (!is.null(stats$sv)) {
    as.numeric(stats$sv[items])
  } else {
    rep(NA_real_, length(items))
  }
  
  if (!is.null(stats$theta)) {
    th <- stats$theta
    result_list$theta <- data.frame(
      Variable = items,
      Method = "theta",
      Mean_L = th[items, "L"],
      Mean_U = th[items, "U"],
      Mean_Mid = (th[items, "L"] + th[items, "U"]) / 2,
      Variance = if (!is.null(stats$theta_var)) as.numeric(stats$theta_var[items]) else NA_real_,
      SV_Mean = sv_mean_ref,
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(stats$hausdorff)) {
    ha <- stats$hausdorff
    result_list$hausdorff <- data.frame(
      Variable = items,
      Method = "hausdorff",
      Mean_L = ha[items, "L"],
      Mean_U = ha[items, "U"],
      Mean_Mid = (ha[items, "L"] + ha[items, "U"]) / 2,
      Variance = if (!is.null(stats$haus_var)) as.numeric(stats$haus_var[items]) else NA_real_,
      SV_Mean = sv_mean_ref,
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(stats$sv)) {
    sv <- as.numeric(stats$sv[items])
    result_list$sv <- data.frame(
      Variable = items,
      Method = "sv",
      Mean_L = sv,
      Mean_U = sv,
      Mean_Mid = sv,
      Variance = if (!is.null(stats$sv_var)) as.numeric(stats$sv_var[items]) else NA_real_,
      SV_Mean = sv,
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, result_list)
}

# =============================================================================
# Visualization functions for descriptive statistics
# =============================================================================

# Color palettes
METHOD_COLORS <- c(theta = "#B8B8B8", hausdorff = "#2E86C1", sv = "#D85040")

# Theme for interval plots
theme_interval <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.6),
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(size = base_size),
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

# Plot means landscape
#'
#' Plots interval-valued means (theta and/or Hausdorff frameworks) together with
#' single-valued (SV) means for each item on a common horizontal scale.
#'
#' The function returns a ggplot object. Users can further customise the plot
#' using standard ggplot2 additions (e.g., + labs(), + theme(), + scale_*()).
#'
#' @param data Data frame from calc_descriptives() output or in viz format.
#' @param labels Optional custom labels for items (must match the number of items).
#' @param sort_by Sorting criterion: "none" (original order), "min", "max", "mid".
#' @param sort_data Data source for sorting: "sv" (default), "theta", "hausdorff".
#' @param sort_order Sort direction: "desc", "asc".
#' @param title Plot title.
#' @param x_lim Optional x-axis limits; applied via coord_cartesian(xlim = x_lim).
#' @param scale Overall scale multiplier applied to linewidth and point size.
#' @param show Character vector of methods to plot. Any of c("sv","theta","hausdorff").
#' @param legend Logical; if TRUE, show legend; otherwise hide legend (default FALSE).
#' @param colors Named vector of colors for methods, e.g.
#'   c(theta = "#B8B8B8", hausdorff = "#2E86C1", sv = "#D85040").
#' @param sizes Named vector of sizes for methods, e.g.
#'   c(theta = 2, hausdorff = 0.5, sv = 1.8, sv_stroke = 0.7).
#'   For theta/hausdorff: linewidth; for sv: point size and outline stroke.
#'
#' @return A ggplot object.
plot_means <- function(data, labels = NULL,
                       sort_by = c("none", "min", "max", "mid", "hurwicz"),
                       sort_data = c("sv", "theta", "hausdorff"),
                       sort_order = c("desc", "asc"),
                       gamma = 0.5,
                       title = "Means",
                       x_lim = NULL,
                       scale = 1.0,
                       show = c("sv", "theta", "hausdorff"),
                       legend = FALSE,
                       colors = NULL,
                       sizes = NULL) {
  
  # Auto-convert from calc_descriptives bundle output to viz format
  is_bundle <- is.list(data) && !is.data.frame(data) && 
    any(c("theta", "hausdorff", "sv") %in% names(data))
  
  if (is_bundle) {
    data <- .stats_to_viz_internal(data)
  }
  
  # Default colors and sizes
  default_colors <- c(theta = "#B8B8B4", hausdorff = "#2E86C1", sv = "#D85040")
  default_sizes <- c(theta = 3, hausdorff = 0.5, sv = 1.8, sv_stroke = 0.7)
  
  # Merge user colors/sizes with defaults
  if (!is.null(colors)) {
    for (nm in names(colors)) default_colors[nm] <- colors[nm]
  }
  if (!is.null(sizes)) {
    for (nm in names(sizes)) default_sizes[nm] <- sizes[nm]
  }
  
  # Apply scale
  theta_lw <- unname(default_sizes["theta"]) * scale
  haus_lw <- unname(default_sizes["hausdorff"]) * scale
  sv_size <- unname(default_sizes["sv"]) * scale
  sv_stroke <- unname(default_sizes["sv_stroke"]) * scale
  
  sort_by <- match.arg(sort_by)
  sort_data <- match.arg(sort_data)
  sort_order <- match.arg(sort_order)
  
  # Validate show
  valid_show <- c("sv", "theta", "hausdorff")
  show <- unique(show)
  if (any(!show %in% valid_show)) {
    stop("Invalid 'show' values. Allowed: ", paste(valid_show, collapse = ", "))
  }
  
  data_wide <- data %>%
    dplyr::select(Variable, Method, Mean_L, Mean_U) %>%
    tidyr::pivot_wider(names_from = Method, values_from = c(Mean_L, Mean_U), names_sep = "_")
  
  # Get original variable order
  orig_vars <- unique(data$Variable)
  
  # Compute sort values based on selected data source
  sort_values <- list()
  for (v in orig_vars) {
    row <- data_wide[data_wide$Variable == v, ]
    L <- U <- NA
    
    # Try to get values from selected sort_data source
    if (sort_data == "sv" && "Mean_L_sv" %in% names(row) && !is.na(row$Mean_L_sv)) {
      L <- U <- row$Mean_L_sv
    } else if (sort_data == "theta" && "Mean_L_theta" %in% names(row) && !is.na(row$Mean_L_theta)) {
      L <- row$Mean_L_theta
      U <- row$Mean_U_theta
    } else if (sort_data == "hausdorff" && "Mean_L_hausdorff" %in% names(row) && !is.na(row$Mean_L_hausdorff)) {
      L <- row$Mean_L_hausdorff
      U <- row$Mean_U_hausdorff
    } else {
      # Fallback: try sv -> theta -> hausdorff
      if ("Mean_L_sv" %in% names(row) && !is.na(row$Mean_L_sv)) {
        L <- U <- row$Mean_L_sv
      } else if ("Mean_L_theta" %in% names(row) && !is.na(row$Mean_L_theta)) {
        L <- row$Mean_L_theta
        U <- row$Mean_U_theta
      } else if ("Mean_L_hausdorff" %in% names(row) && !is.na(row$Mean_L_hausdorff)) {
        L <- row$Mean_L_hausdorff
        U <- row$Mean_U_hausdorff
      }
    }
    sort_values[[v]] <- get_sort_value(L, U, sort_by, gamma = gamma)
  }
  
  # Apply sorting
  ordered_vars <- apply_sort_order(orig_vars, sort_values, sort_by, sort_order)
  
  # Apply custom labels if provided
  if (!is.null(labels)) {
    if (length(labels) == length(orig_vars)) {
      # Make labels unique by appending index if duplicates exist
      if (anyDuplicated(labels)) {
        labels <- make.unique(labels, sep = " ")
      }
      label_map <- setNames(labels, orig_vars)
      data_wide$Variable <- label_map[data_wide$Variable]
      ordered_vars <- label_map[ordered_vars]
    }
  }
  
  # Set factor levels for correct order (reversed for horizontal plot)
  data_wide$Variable <- factor(data_wide$Variable, levels = rev(ordered_vars))
  
  # Check which methods are present (have non-NA values)
  has_theta <- "Mean_L_theta" %in% names(data_wide) && any(!is.na(data_wide$Mean_L_theta))
  has_hausdorff <- "Mean_L_hausdorff" %in% names(data_wide) && any(!is.na(data_wide$Mean_L_hausdorff))
  has_sv <- "Mean_L_sv" %in% names(data_wide) && any(!is.na(data_wide$Mean_L_sv))
  
  p <- ggplot2::ggplot(data_wide, ggplot2::aes(y = Variable))
  
  # Add theta if present (thick line, behind)
  if ("theta" %in% show && has_theta) {
    p <- p + ggplot2::geom_segment(
      ggplot2::aes(
        x = Mean_L_theta, xend = Mean_U_theta,
        y = Variable, yend = Variable,
        color = "theta"
      ),
      linewidth = theta_lw,
      alpha = 0.6
    )
  }
  
  # Add hausdorff if present (thin line, on top)
  if ("hausdorff" %in% show && has_hausdorff) {
    p <- p + ggplot2::geom_segment(
      ggplot2::aes(
        x = Mean_L_hausdorff, xend = Mean_U_hausdorff,
        y = Variable, yend = Variable,
        color = "hausdorff"
      ),
      linewidth = haus_lw,
      alpha = 1.0
    )
  }
  
  # Add SV if present (circle)
  if ("sv" %in% show && has_sv) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = Mean_L_sv, color = "sv"),
      size = sv_size,
      shape = 1,
      stroke = sv_stroke
    )
  }
  
  # Manual colour scale (enables ggplot2 overrides + optional legend)
  # breaks controls legend ordering if legend is shown.
  p <- p + ggplot2::scale_color_manual(
    values = default_colors,
    breaks = c("theta", "hausdorff", "sv"),
    name = NULL
  )
  
  if (!isTRUE(legend)) {
    p <- p + ggplot2::guides(color = "none")
  }
  
  p <- p + ggplot2::labs(x = "Rating", y = NULL, title = title) +
    theme_interval()
  
  # Apply x_lim if provided (use coord_cartesian to avoid dropping data)
  if (!is.null(x_lim)) {
    p <- p + ggplot2::coord_cartesian(xlim = x_lim)
  }
  
  p
}

# Plot correlation
#'
#' Scatter plot showing correlation between x and y variables.
#' Supports SV (points), IV (lines/rectangles), and MIV (multiple rectangles/lines).
#'
#' The function returns a ggplot object. Users can further customise the plot
#' using standard ggplot2 additions (e.g., + scale_color_manual(), + scale_linewidth_manual()).
#'
#' @param x X variable. Can be:
#'   - A numeric vector (SV)
#'   - A data.frame/matrix with 2 columns [L, U] (IV)
#'   - A list of matrices, each matrix has rows of [L, U] intervals (MIV, requires miv=TRUE)
#'   - A named list of any of above for multiple variables (only when miv=FALSE)
#' @param y Y variable (single). Same format options as x.
#' @param method Correlation method: "theta" (default), "hausdorff", "pearson", "midpoint".
#'   Note: MIV data requires "hausdorff".
#' @param miv Logical; if TRUE, treat x and y as MIV data (default FALSE).
#'   When TRUE, x and y must each be a single variable (not a list of multiple variables).
#' @param show_diagonal Logical; if TRUE, show y=x diagonal line (default TRUE).
#' @param show_cor Logical; if TRUE, display correlation values on plot (default TRUE).
#' @param axis_lim Optional axis limits for both x and y axes.
#' @param title Plot title (default includes method name).
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param scale Overall scale multiplier for linewidth/point size.
#' @param legend Logical; if TRUE, show legend (default FALSE for single x, TRUE for multiple x).
#'
#' @return A ggplot object.
#'
#' @examples
#' # SV vs SV - points
#' plot_correlation(sv_x, sv_y)
#' 
#' # IV vs SV - horizontal lines
#' plot_correlation(data.frame(L = iv_L, U = iv_U), sv_y)
#' 
#' # IV vs IV - rectangles
#' plot_correlation(iv_x, iv_y, method = "theta")
#'
#' # MIV vs MIV - multiple rectangles per observation
#' x_miv <- list(matrix(c(1,2, 4,5), ncol=2, byrow=TRUE), ...)
#' y_miv <- list(matrix(c(1.5,2.5, 4.5,5.5), ncol=2, byrow=TRUE), ...)
#' plot_correlation(x_miv, y_miv, miv = TRUE, method = "hausdorff")
#'
#' # Multiple x variables (miv=FALSE)
#' x_vars <- list(theta = iv_theta, hausdorff = iv_haus)
#' plot_correlation(x_vars, sv_y)
#'
plot_correlation <- function(x, y, 
                             method = "theta",
                             miv = FALSE,
                             show_diagonal = TRUE,
                             show_cor = TRUE,
                             axis_lim = NULL,
                             title = NULL,
                             x_label = "X",
                             y_label = "Y",
                             scale = 1.0,
                             legend = NULL,
                             degenerate = FALSE,
                             theta = 1) {
  
  # Validate method(s)
  valid_methods <- c("theta", "hausdorff", "pearson", "midpoint")
  invalid <- setdiff(method, valid_methods)
  if (length(invalid) > 0) {
    stop("Invalid method(s): ", paste(invalid, collapse = ", "), 
         ". Valid options: ", paste(valid_methods, collapse = ", "))
  }
  
  # Default title includes method(s)
  if (is.null(title)) {
    title <- sprintf("Correlation (%s)", paste(unique(method), collapse = "/"))
  }
  
  # Default color and size
  default_color <- "#2E86C1"
  default_size <- 1.0
  
  # Colors and sizes for known method names (used in multi-variable mode)
  method_colors <- c(theta = "#B8B8B8", hausdorff = "#2E86C1")
  method_sizes <- c(theta = 3, hausdorff = 0.5)
  general_colors <- c("#E76F51", "#2A9D8F", "#E9C46A", "#9B2226", "#264653",
                      "#F4A261", "#D4A373", "#AE2012", "#BB3E03", "#CA6702")
  
  # ==========================================================================
  # MIV MODE (following Kang et al. 2025 visualization style)
  # ==========================================================================
  if (isTRUE(miv)) {
    # MIV mode: one variable is MIV, the other is SV or IV
    # MIV segments are drawn along one axis, SV/IV values on the other axis
    
    # Helper: detect data type for MIV mode
    detect_miv_type <- function(v) {
      if (is.list(v) && !is.data.frame(v)) {
        if (length(v) > 0 && (is.matrix(v[[1]]) || (is.numeric(v[[1]]) && length(v[[1]]) == 2))) {
          return("miv")
        }
      }
      if (is.data.frame(v) || is.matrix(v)) {
        v <- as.matrix(v)
        if (ncol(v) >= 2) {
          is_iv <- !all(abs(v[,1] - v[,2]) < 1e-10, na.rm = TRUE)
          return(if(is_iv) "iv" else "sv")
        }
      }
      if (is.numeric(v)) return("sv")
      return("unknown")
    }
    
    # Helper: parse MIV variable to list of matrices
    parse_miv <- function(v) {
      lapply(v, function(m) {
        if (is.matrix(m)) m else matrix(m, ncol = 2, byrow = TRUE)
      })
    }
    
    # Helper: parse IV/SV variable
    parse_iv_sv <- function(v) {
      if (is.data.frame(v) || is.matrix(v)) {
        v <- as.matrix(v)
        if (ncol(v) >= 2) {
          return(list(L = v[,1], U = v[,2]))
        } else {
          return(list(L = v[,1], U = v[,1]))
        }
      }
      v <- as.numeric(v)
      return(list(L = v, U = v))
    }
    
    x_type <- detect_miv_type(x)
    y_type <- detect_miv_type(y)
    
    # Validate: exactly one MIV
    if (x_type == "miv" && y_type == "miv") {
      stop("In MIV mode, only one of x or y can be MIV (the other must be SV or IV).")
    }
    if (x_type != "miv" && y_type != "miv") {
      stop("In MIV mode, one of x or y must be MIV data.")
    }
    
    # Determine which is MIV
    x_is_miv <- (x_type == "miv")
    
    if (x_is_miv) {
      # X is MIV, Y is SV/IV
      miv_data <- parse_miv(x)
      other_data <- parse_iv_sv(y)
      n <- length(miv_data)
      other_is_iv <- (y_type == "iv")
    } else {
      # Y is MIV, X is SV/IV
      miv_data <- parse_miv(y)
      other_data <- parse_iv_sv(x)
      n <- length(miv_data)
      other_is_iv <- (x_type == "iv")
    }
    
    # Build plot data: each MIV segment becomes a row
    # MIV segments are on one axis, other variable value on the other axis
    plot_rows <- list()
    for (i in seq_len(n)) {
      miv_mat <- miv_data[[i]]  # matrix with rows [L, U]
      other_L <- other_data$L[i]
      other_U <- other_data$U[i]
      
      for (r in seq_len(nrow(miv_mat))) {
        if (x_is_miv) {
          # X is MIV: segments are horizontal
          plot_rows[[length(plot_rows) + 1]] <- data.frame(
            obs = factor(i),
            x_L = miv_mat[r, 1],
            x_U = miv_mat[r, 2],
            y_L = other_L,
            y_U = other_U,
            y_mid = (other_L + other_U) / 2,
            stringsAsFactors = FALSE
          )
        } else {
          # Y is MIV: segments are vertical
          plot_rows[[length(plot_rows) + 1]] <- data.frame(
            obs = factor(i),
            x_L = other_L,
            x_U = other_U,
            x_mid = (other_L + other_U) / 2,
            y_L = miv_mat[r, 1],
            y_U = miv_mat[r, 2],
            stringsAsFactors = FALSE
          )
        }
      }
    }
    plot_data <- do.call(rbind, plot_rows)
    
    # Calculate correlation
    cor_val <- tryCatch({
      calc_cor(x, y, method = method, verbose = FALSE, degenerate = degenerate, theta = theta)
    }, error = function(e) NA_real_)
    
    # Calculate axis limits (separate for each axis)
    all_x <- c(plot_data$x_L, plot_data$x_U)
    all_y <- c(plot_data$y_L, plot_data$y_U)
    
    x_range <- range(all_x, na.rm = TRUE)
    y_range <- range(all_y, na.rm = TRUE)
    x_margin <- diff(x_range) * 0.05
    y_margin <- diff(y_range) * 0.05
    x_lim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
    y_lim <- c(y_range[1] - y_margin, y_range[2] + y_margin)
    
    # Override with user-specified limits if provided
    if (!is.null(axis_lim)) {
      x_lim <- axis_lim
      y_lim <- axis_lim
    }
    
    # Build plot
    p <- ggplot2::ggplot(plot_data)
    
    if (show_diagonal) {
      p <- p + ggplot2::geom_abline(intercept = 0, slope = 1, color = "grey70", 
                                    linetype = "dashed", linewidth = 0.5)
    }
    
    lw <- default_size * scale
    
    if (x_is_miv) {
      # X is MIV: draw horizontal segments (or rectangles if Y is IV)
      if (other_is_iv) {
        # MIV vs IV: rectangles with fill (paper style)
        p <- p + ggplot2::geom_rect(
          ggplot2::aes(xmin = x_L, xmax = x_U, ymin = y_L, ymax = y_U),
          fill = default_color, color = NA, alpha = 0.4
        )
      } else {
        # MIV vs SV: horizontal segments
        p <- p + ggplot2::geom_segment(
          ggplot2::aes(x = x_L, xend = x_U, y = y_mid, yend = y_mid),
          color = default_color, linewidth = lw * 2, alpha = 0.6
        )
      }
    } else {
      # Y is MIV: draw vertical segments (or rectangles if X is IV)
      if (other_is_iv) {
        # IV vs MIV: rectangles with fill
        p <- p + ggplot2::geom_rect(
          ggplot2::aes(xmin = x_L, xmax = x_U, ymin = y_L, ymax = y_U),
          fill = default_color, color = NA, alpha = 0.4
        )
      } else {
        # SV vs MIV: vertical segments
        p <- p + ggplot2::geom_segment(
          ggplot2::aes(x = x_mid, xend = x_mid, y = y_L, yend = y_U),
          color = default_color, linewidth = lw * 2, alpha = 0.6
        )
      }
    }
    
    # Add correlation annotation
    if (show_cor && !is.na(cor_val)) {
      p <- p + ggplot2::annotate(
        "text",
        x = x_lim[1] + 0.02 * diff(x_lim),
        y = y_lim[2] - 0.02 * diff(y_lim),
        label = sprintf("r = %.3f", cor_val),
        hjust = 0, vjust = 1,
        size = 3.5, color = "grey30"
      )
    }
    
    p <- p + ggplot2::coord_cartesian(xlim = x_lim, ylim = y_lim) +
      ggplot2::labs(x = x_label, y = y_label, title = title) +
      theme_interval()
    
    return(p)
  }
  
  # ==========================================================================
  # STANDARD MODE (non-MIV)
  # ==========================================================================
  
  # Helper: parse variable to get L, U bounds
  parse_var <- function(v) {
    if (is.data.frame(v) || is.matrix(v)) {
      v <- as.matrix(v)
      if (ncol(v) >= 2) {
        return(list(L = v[, 1], U = v[, 2], is_iv = !all(abs(v[,1] - v[,2]) < 1e-10, na.rm = TRUE)))
      } else {
        return(list(L = v[, 1], U = v[, 1], is_iv = FALSE))
      }
    } else {
      v <- as.numeric(v)
      return(list(L = v, U = v, is_iv = FALSE))
    }
  }
  
  # Helper: check if input is a named list of variables (not a data.frame)
  is_var_list <- function(v) {
    is.list(v) && !is.data.frame(v) && !is.matrix(v)
  }
  
  # Parse y (must be single variable)
  if (is_var_list(y)) {
    stop("y must be a single variable (vector or data.frame), not a list of variables.")
  }
  y_parsed <- parse_var(y)
  
  # Parse x - can be single variable or list of variables
  if (is_var_list(x)) {
    x_list <- x
    if (is.null(names(x_list))) {
      names(x_list) <- paste0("x", seq_along(x_list))
    }
  } else {
    x_list <- list(x = x)
  }
  
  x_parsed <- lapply(x_list, parse_var)
  x_names <- names(x_parsed)
  
  # Set default legend behavior
  if (is.null(legend)) {
    legend <- length(x_names) > 1
  }
  
  # Assign colors and sizes based on x names
  colors <- character(length(x_names))
  sizes <- numeric(length(x_names))
  names(colors) <- x_names
  names(sizes) <- x_names
  
  general_idx <- 1
  for (nm in x_names) {
    nm_lower <- tolower(nm)
    if (grepl("theta", nm_lower)) {
      colors[nm] <- method_colors["theta"]
      sizes[nm] <- method_sizes["theta"]
    } else if (grepl("hausdorff", nm_lower)) {
      colors[nm] <- method_colors["hausdorff"]
      sizes[nm] <- method_sizes["hausdorff"]
    } else {
      colors[nm] <- general_colors[((general_idx - 1) %% length(general_colors)) + 1]
      sizes[nm] <- default_size
      general_idx <- general_idx + 1
    }
  }
  
  # Build data frame for plotting
  plot_data <- do.call(rbind, lapply(x_names, function(nm) {
    xp <- x_parsed[[nm]]
    data.frame(
      var_name = nm,
      x_L = xp$L,
      x_U = xp$U,
      x_mid = (xp$L + xp$U) / 2,
      y_L = y_parsed$L,
      y_U = y_parsed$U,
      y_mid = (y_parsed$L + y_parsed$U) / 2,
      x_is_iv = xp$is_iv,
      y_is_iv = y_parsed$is_iv,
      stringsAsFactors = FALSE
    )
  }))
  
  # Calculate correlations for each x variable
  # Method matching: if length(method) == length(x_names), match 1:1
  # Otherwise, use first method for all and warn
  if (length(method) == length(x_names)) {
    use_methods <- method
  } else {
    if (length(method) > 1) {
      message(sprintf("Number of x variables (%d) does not match number of methods (%d). Using first method '%s' for all.", 
                      length(x_names), length(method), method[1]))
    }
    use_methods <- rep(method[1], length(x_names))
  }
  
  cor_results <- lapply(seq_along(x_names), function(i) {
    nm <- x_names[i]
    xp <- x_parsed[[nm]]
    x_df <- data.frame(L = xp$L, U = xp$U)
    y_df <- data.frame(L = y_parsed$L, U = y_parsed$U)
    
    tryCatch({
      calc_cor(x_df, y_df, method = use_methods[i], verbose = FALSE, degenerate = degenerate, theta = theta)
    }, error = function(e) {
      cor((xp$L + xp$U) / 2, (y_parsed$L + y_parsed$U) / 2, use = "complete.obs")
    })
  })
  names(cor_results) <- x_names
  
  # Start plot
  p <- ggplot2::ggplot(plot_data)
  
  # Add diagonal line
  if (show_diagonal) {
    p <- p + ggplot2::geom_abline(intercept = 0, slope = 1, color = "grey70", 
                                  linetype = "dashed", linewidth = 0.5)
  }
  
  # Determine geometry type based on data
  x_is_iv <- any(plot_data$x_is_iv)
  y_is_iv <- any(plot_data$y_is_iv)
  
  if (x_is_iv && y_is_iv) {
    # Both interval - draw rectangles
    p <- p + ggplot2::geom_rect(
      ggplot2::aes(xmin = x_L, xmax = x_U, ymin = y_L, ymax = y_U, 
                   color = var_name, linewidth = var_name),
      fill = NA, alpha = 0.8
    )
  } else if (x_is_iv && !y_is_iv) {
    # x interval, y point - horizontal segments
    p <- p + ggplot2::geom_segment(
      ggplot2::aes(x = x_L, xend = x_U, y = y_mid, yend = y_mid, 
                   color = var_name, linewidth = var_name),
      alpha = 0.8
    )
  } else if (!x_is_iv && y_is_iv) {
    # x point, y interval - vertical segments
    p <- p + ggplot2::geom_segment(
      ggplot2::aes(x = x_mid, xend = x_mid, y = y_L, yend = y_U, 
                   color = var_name, linewidth = var_name),
      alpha = 0.8
    )
  } else {
    # Both points
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = x_mid, y = y_mid, color = var_name, size = var_name),
      alpha = 0.8
    )
  }
  
  # Apply scale multiplier to sizes
  scaled_sizes <- sizes * scale
  
  # Set color and size/linewidth scales
  p <- p + ggplot2::scale_color_manual(
    values = colors,
    breaks = x_names,
    name = NULL
  )
  
  if (x_is_iv || y_is_iv) {
    # Use linewidth for segments/rectangles
    p <- p + ggplot2::scale_linewidth_manual(
      values = scaled_sizes,
      breaks = x_names,
      name = NULL
    )
  } else {
    # Use size for points
    p <- p + ggplot2::scale_size_manual(
      values = scaled_sizes * 2,
      breaks = x_names,
      name = NULL
    )
  }
  
  # Add correlation annotations (position based on data range)
  if (show_cor) {
    cor_text <- paste(sapply(x_names, function(nm) {
      sprintf("%s: r = %.3f", nm, cor_results[[nm]])
    }), collapse = "\n")
    
    p <- p + ggplot2::annotate(
      "text",
      x = -Inf, y = Inf,
      label = cor_text,
      hjust = -0.05, vjust = 1.1,
      size = 3.5, color = "grey30"
    )
  }
  
  # Legend control
  if (!isTRUE(legend)) {
    p <- p + ggplot2::guides(color = "none", linewidth = "none", size = "none")
  }
  
  # Apply axis limits only if explicitly provided
  if (!is.null(axis_lim)) {
    p <- p + ggplot2::coord_cartesian(xlim = axis_lim, ylim = axis_lim)
  }
  
  p + ggplot2::labs(x = x_label, y = y_label, title = title) +
    theme_interval()
}

# =============================================================================
# IPA (Importance-Performance Analysis) Plot
# =============================================================================

#' Plot IPA (Importance-Performance Analysis)
#' 
#' @param expectation Importance data: vector (SV) or data.frame with Min/Max (IV)
#' @param performance Performance data: vector (SV) or data.frame with Min/Max (IV)
#' @param labels Labels for each point/rectangle (e.g., 1:14 or c("S1", "S2", ...))
#' @param type "SV" for single-valued (points), "IV" for interval-valued (rectangles)
#' @param crosshair Method for crosshair: "mean" (default), "median", or numeric vector c(x, y)
#' @param colors Color palette for labels (auto-generated if NULL)
#' @param title Plot title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @return ggplot object
# Importance-Performance Analysis (IPA) Plot
#'
#' Creates an IPA quadrant plot with automatic data type detection.
#' 
#' The function accepts importance and performance as simple variables:
#' - SV: numeric vector
#' - IV: data.frame/matrix with 2 columns [L, U]
#' 
#' Data type is auto-detected. If either variable is IV, the plot uses IV mode
#' (rectangles). If both are SV, the plot uses SV mode (points/labels).
#' 
#' @param expectation Importance variable: numeric vector (SV) or data.frame with [L, U] columns (IV)
#' @param performance Performance variable: numeric vector (SV) or data.frame with [L, U] columns (IV)
#' @param labels Optional labels for each item. If NULL, uses 1:n.
#' @param method Method to use when auto-detected as IV: "theta" or "hausdorff". 
#'   Only affects the title display; actual intervals should be pre-computed.
#' @param crosshair Crosshair position for SV mode: "mean", "median", or c(x, y) values.
#' @param colors Optional color vector for items. If NULL, uses default palette.
#' @param title Plot title. Default includes method if IV mode.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param quadrant_size Font size for quadrant labels.
#' @param x_lim Optional x-axis limits.
#' @param y_lim Optional y-axis limits.
#' @param scale Overall scale multiplier for line widths and label sizes.
#'
#' @return A ggplot object.
#'
#' @examples
#' # SV mode: simple vectors
#' imp_sv <- c(4.2, 3.8, 4.5, 3.2)
#' perf_sv <- c(3.5, 4.1, 3.8, 4.3)
#' plot_ipa(imp_sv, perf_sv, labels = c("A", "B", "C", "D"))
#'
#' # IV mode: data.frames with L, U columns
#' imp_iv <- data.frame(L = c(3.8, 3.5, 4.2, 2.9), U = c(4.6, 4.1, 4.8, 3.5))
#' perf_iv <- data.frame(L = c(3.2, 3.8, 3.5, 4.0), U = c(3.8, 4.4, 4.1, 4.6))
#' plot_ipa(imp_iv, perf_iv, labels = c("A", "B", "C", "D"))
#'
#' # Mixed: IV importance, SV performance -> uses IV mode
#' plot_ipa(imp_iv, perf_sv, labels = c("A", "B", "C", "D"))
#'
plot_ipa <- function(importance, performance, 
                     labels = NULL,
                     method = c("theta", "hausdorff"),
                     crosshair = "mean",
                     crosshair_method = c("theta", "hausdorff"),
                     colors = NULL,
                     title = NULL,
                     x_label = "Performance",
                     y_label = "Importance",
                     quadrant_size = 3,
                     x_lim = NULL,
                     y_lim = NULL,
                     scale = 1.0) {
  
  method <- match.arg(method)
  crosshair_method <- match.arg(crosshair_method)
  
  # Scale controls linewidth and label size proportionally
  rect_lw <- 0.8 * scale
  label_size <- 3.5 * scale
  
  # Helper: detect data type and parse variable
  parse_var <- function(v) {
    if (is.data.frame(v) || is.matrix(v)) {
      v <- as.matrix(v)
      if (ncol(v) >= 2) {
        L <- v[, 1]
        U <- v[, 2]
        is_iv <- !all(abs(L - U) < 1e-10, na.rm = TRUE)
        return(list(L = L, U = U, is_iv = is_iv))
      } else {
        return(list(L = v[, 1], U = v[, 1], is_iv = FALSE))
      }
    } else {
      v <- as.numeric(v)
      return(list(L = v, U = v, is_iv = FALSE))
    }
  }
  
  # Parse importance and performance
  imp_parsed <- parse_var(importance)
  perf_parsed <- parse_var(performance)
  
  imp_L <- imp_parsed$L
  imp_U <- imp_parsed$U
  perf_L <- perf_parsed$L
  perf_U <- perf_parsed$U
  
  # Auto-detect mode: IV if either is IV, otherwise SV
  use_iv_mode <- imp_parsed$is_iv || perf_parsed$is_iv
  
  # If user selected SV but data is IV, use midpoints
  # (This happens automatically since we use mid for SV plotting)
  
  n <- length(imp_L)
  
  # Default labels
  if (is.null(labels)) labels <- as.character(1:n)
  
  # Default title
  if (is.null(title)) {
    if (use_iv_mode) {
      title <- sprintf("Importance-Performance Analysis (%s)", method)
    } else {
      title <- "Importance-Performance Analysis"
    }
  }
  
  # Warn about duplicate labels (they will overlap in the plot)
  if (anyDuplicated(labels)) {
    warning("Duplicate labels detected. Consider using unique labels for clarity.")
  }
  
  # Filter out NA values
  valid <- !is.na(imp_L) & !is.na(imp_U) & !is.na(perf_L) & !is.na(perf_U)
  if (sum(valid) == 0) {
    warning("No valid data for IPA plot")
    return(ggplot2::ggplot() + ggplot2::theme_void() + 
             ggplot2::labs(title = "No valid data"))
  }
  
  imp_L <- imp_L[valid]
  imp_U <- imp_U[valid]
  perf_L <- perf_L[valid]
  perf_U <- perf_U[valid]
  labels <- labels[valid]
  n <- length(imp_L)
  
  # Default colors - academic style
  if (is.null(colors)) {
    colors <- c(
      "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#6A3D9A", 
      "#A6761D", "#666666", "#B2DF8A", "#FB9A99", "#CAB2D6", 
      "#FDBF6F", "#1B9E77", "#D95F02", "#7570B3"
    )
  }
  colors <- rep(colors, length.out = n)
  
  # Calculate midpoints
  imp_mid <- (imp_L + imp_U) / 2
  perf_mid <- (perf_L + perf_U) / 2
  
  # Calculate crosshair position
  if (use_iv_mode) {
    if (crosshair_method == "hausdorff") {
      # Hausdorff mean: optimize midpoint with average range
      perf_stats <- calc_hausdorff_stats(perf_L, perf_U, use_sample_var = FALSE)
      cross_x_L <- perf_stats$mean_L
      cross_x_U <- perf_stats$mean_U
      imp_stats <- calc_hausdorff_stats(imp_L, imp_U, use_sample_var = FALSE)
      cross_y_L <- imp_stats$mean_L
      cross_y_U <- imp_stats$mean_U
    } else {
      # Theta (arithmetic mean of endpoints) - original behavior
      cross_x_L <- mean(perf_L, na.rm = TRUE)
      cross_x_U <- mean(perf_U, na.rm = TRUE)
      cross_y_L <- mean(imp_L, na.rm = TRUE)
      cross_y_U <- mean(imp_U, na.rm = TRUE)
    }
  } else {
    # For SV: use single line
    if (is.character(crosshair)) {
      if (crosshair == "mean") {
        cross_x <- mean(perf_mid, na.rm = TRUE)
        cross_y <- mean(imp_mid, na.rm = TRUE)
      } else if (crosshair == "median") {
        cross_x <- median(perf_mid, na.rm = TRUE)
        cross_y <- median(imp_mid, na.rm = TRUE)
      } else {
        cross_x <- mean(perf_mid, na.rm = TRUE)
        cross_y <- mean(imp_mid, na.rm = TRUE)
      }
    } else {
      cross_x <- crosshair[1]
      cross_y <- crosshair[2]
    }
  }
  
  # Create data frame
  df <- data.frame(
    label = labels,
    perf_L = perf_L, perf_U = perf_U,
    imp_L = imp_L, imp_U = imp_U,
    perf_mid = perf_mid,
    imp_mid = imp_mid,
    color = colors,
    stringsAsFactors = FALSE
  )
  
  # Calculate axis limits
  x_range <- range(c(perf_L, perf_U), na.rm = TRUE)
  y_range <- range(c(imp_L, imp_U), na.rm = TRUE)
  if (is.null(x_lim)) {
    x_margin <- diff(x_range) * 0.1
    x_lim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  }
  if (is.null(y_lim)) {
    y_margin <- diff(y_range) * 0.1
    y_lim <- c(y_range[1] - y_margin, y_range[2] + y_margin)
  }
  
  # Base plot
  p <- ggplot2::ggplot(df)
  
  if (use_iv_mode) {
    # IV: Draw cross region (grey intersection area)
    p <- p +
      # Horizontal band
      ggplot2::annotate(
        "rect",
        xmin = x_lim[1], xmax = x_lim[2], ymin = cross_y_L, ymax = cross_y_U,
        fill = "grey85", color = NA
      ) +
      # Vertical band
      ggplot2::annotate(
        "rect",
        xmin = cross_x_L, xmax = cross_x_U, ymin = y_lim[1], ymax = y_lim[2],
        fill = "grey85", color = NA
      ) +
      # Border lines
      ggplot2::geom_hline(yintercept = c(cross_y_L, cross_y_U), color = "grey60", linewidth = 0.3) +
      ggplot2::geom_vline(xintercept = c(cross_x_L, cross_x_U), color = "grey60", linewidth = 0.3)
  } else {
    # SV: Single crosshair lines
    p <- p +
      ggplot2::geom_hline(yintercept = cross_y, color = "grey50", linetype = "solid", linewidth = 0.5) +
      ggplot2::geom_vline(xintercept = cross_x, color = "grey50", linetype = "solid", linewidth = 0.5)
  }
  
  # Quadrant labels
  p <- p +
    ggplot2::annotate("text", x = x_lim[1] + 0.02 * diff(x_lim), y = y_lim[2] - 0.02 * diff(y_lim),
                      label = "Concentrate Here", hjust = 0, vjust = 1, size = quadrant_size + 1, color = "grey40") +
    ggplot2::annotate("text", x = x_lim[2] - 0.02 * diff(x_lim), y = y_lim[2] - 0.02 * diff(y_lim),
                      label = "Keep Up\nthe Good Work", hjust = 1, vjust = 1, size = quadrant_size + 1, color = "grey40") +
    ggplot2::annotate("text", x = x_lim[1] + 0.02 * diff(x_lim), y = y_lim[1] + 0.02 * diff(y_lim),
                      label = "Low Priority", hjust = 0, vjust = 0, size = quadrant_size + 1, color = "grey40") +
    ggplot2::annotate("text", x = x_lim[2] - 0.02 * diff(x_lim), y = y_lim[1] + 0.02 * diff(y_lim),
                      label = "Possible Overkill", hjust = 1, vjust = 0, size = quadrant_size + 1, color = "grey40")
  
  if (use_iv_mode) {
    # IV: Draw rectangles
    p <- p + ggplot2::geom_rect(
      ggplot2::aes(xmin = perf_L, xmax = perf_U, ymin = imp_L, ymax = imp_U),
      fill = NA, color = df$color, linewidth = rect_lw
    ) +
      ggplot2::geom_text(
        ggplot2::aes(x = perf_mid, y = imp_mid, label = label),
        color = df$color, size = label_size, fontface = "bold"
      )
  } else {
    # SV: Draw points with labels
    p <- p + ggplot2::geom_text(
      ggplot2::aes(x = perf_mid, y = imp_mid, label = label),
      color = df$color, size = label_size, fontface = "bold"
    )
  }
  
  p + ggplot2::scale_x_continuous(limits = x_lim) +
    ggplot2::scale_y_continuous(limits = y_lim) +
    ggplot2::labs(x = x_label, y = y_label, title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.title = ggplot2::element_text(size = 11)
    )
}

# =============================================================================
# Gap Comparison Plot (Perception vs Expectation)
# =============================================================================

#' Plot Gap Comparison between Perception and Expectation
#'
#' @param perception Perception data: numeric vector (SV) or data.frame/matrix (IV: L/U)
#' @param expectation Expectation data: numeric vector (SV) or data.frame/matrix (IV: L/U)
#' @param labels Labels for each aspect
#' @param sv_perception Optional SV perception values (points). If provided together with sv_expectation, points are shown.
#' @param sv_expectation Optional SV expectation values (points). If provided together with sv_perception, points are shown.
#' @param show_sv_points Whether to show SV points overlaid on IV segments (default: FALSE).
#'   If both sv_perception and sv_expectation are provided, this is forced to TRUE.
#' @param sort_by Sort order: "none", or one of:
#'   "expectation_min", "expectation_max", "expectation_mid", "gap_opt", "gap_pes", "gap_mid"
#' @param sort_order Sort direction: "desc" or "asc"
#' @param title Plot title
#' @param type Optional: "SV" or "IV". If NULL, auto-detect.
#' @param x_lim Optional x-axis limits
#' @param scale Size multiplier for line width and point size
#' @return ggplot object
#'
#' @export
plot_gap <- function(perception, expectation,
                     labels = NULL,
                     sv_perception = NULL,
                     sv_expectation = NULL,
                     show_sv_points = FALSE,
                     sort_by = c("none", "expectation_min", "expectation_max", "expectation_mid",
                                 "gap_opt", "gap_pes", "gap_mid"),
                     sort_order = c("desc", "asc"),
                     title = "Perception vs Expectation Gap",
                     type = NULL,
                     x_lim = NULL,
                     scale = 1.0) {
  
  sort_by <- match.arg(sort_by)
  sort_order <- match.arg(sort_order)
  
  # Scale controls linewidth and point size proportionally
  exp_lw <- 2 * scale
  per_lw <- 0.5 * scale
  sv_size <- 1.8 * scale
  sv_stroke <- 0.7 * scale
  
  # ---- helper: convert input to L/U ----
  to_LU <- function(x) {
    if (is.data.frame(x) || is.matrix(x)) {
      x <- as.data.frame(x)
      if (ncol(x) == 1) {
        L <- x[[1]]; U <- x[[1]]
      } else {
        if (all(c("L", "U") %in% names(x))) {
          L <- x[["L"]]; U <- x[["U"]]
        } else {
          L <- x[[1]]; U <- x[[2]]
        }
      }
      return(list(L = as.numeric(L), U = as.numeric(U)))
    }
    return(list(L = as.numeric(x), U = as.numeric(x)))
  }
  
  # ---- extract L/U arrays ----
  per <- to_LU(perception)
  exp_data <- to_LU(expectation)
  
  per_L <- per$L; per_U <- per$U
  exp_L <- exp_data$L; exp_U <- exp_data$U
  
  n <- length(per_L)
  if (length(per_U) != n || length(exp_L) != n || length(exp_U) != n) {
    stop("Input lengths do not match between perception and expectation.")
  }
  
  # Default labels: extract numbers from rownames if available, otherwise use 1:n
  if (is.null(labels)) {
    rn <- NULL
    if (is.data.frame(perception) || is.matrix(perception)) {
      rn <- rownames(perception)
    } else if (is.data.frame(expectation) || is.matrix(expectation)) {
      rn <- rownames(expectation)
    }
    
    if (!is.null(rn) && length(rn) == n && !all(rn == as.character(1:n))) {
      # Extract numbers from rownames (e.g., "I1" -> "1", "S3" -> "3")
      nums <- gsub("[^0-9]", "", rn)
      # Use extracted numbers if valid, otherwise fall back to 1:n
      if (all(nzchar(nums))) {
        labels <- nums
      } else {
        labels <- as.character(1:n)
      }
    } else {
      labels <- as.character(1:n)
    }
  }
  
  # Make labels unique if duplicates exist (required for factor conversion)
  if (anyDuplicated(labels)) labels <- make.unique(labels, sep = " ")
  
  # ---- SV points behaviour ----
  # Default: show_sv_points is FALSE
  # Rule: if both SV vectors are provided, force show_sv_points = TRUE
  if (!is.null(sv_perception) && !is.null(sv_expectation)) {
    show_sv_points <- TRUE
  }
  
  if (show_sv_points) {
    # If either one is missing, use midpoint as fallback
    if (is.null(sv_perception)) sv_perception <- (sat_L + sat_U) / 2
    if (is.null(sv_expectation)) sv_expectation <- (imp_L + imp_U) / 2
    sv_perception <- as.numeric(sv_perception)
    sv_expectation <- as.numeric(sv_expectation)
  } else {
    sv_perception <- rep(NA_real_, n)
    sv_expectation <- rep(NA_real_, n)
  }
  
  # ---- filter invalid rows ----
  valid <- !is.na(sat_L) & !is.na(sat_U) & !is.na(imp_L) & !is.na(imp_U)
  if (sum(valid) == 0) {
    warning("No valid data for gap plot")
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No valid data"))
  }
  
  per_L <- per_L[valid]; per_U <- per_U[valid]
  exp_L <- exp_L[valid]; exp_U <- exp_U[valid]
  sv_perception <- sv_perception[valid]
  sv_expectation <- sv_expectation[valid]
  labels <- labels[valid]
  n <- length(per_L)
  
  # ---- gap computations (Sat - Imp) ----
  # gap_opt: optimistic gap = best performance - lowest expectation = Sat_U - Imp_L
  # gap_pes: pessimistic gap = worst performance - highest expectation = Sat_L - Imp_U
  # gap_mid: midpoint gap
  gap_opt <- per_U - exp_L
  gap_pes <- per_L - exp_U
  gap_mid <- (per_L + per_U) / 2 - (exp_L + exp_U) / 2
  
  # Auto-detect type if not specified
  if (is.null(type)) {
    is_sv <- all(abs(per_L - per_U) < 1e-10) && all(abs(exp_L - exp_U) < 1e-10)
    type <- if (is_sv) "SV" else "IV"
  }
  
  # ---- sorting ----
  sort_values <- vector("list", n)
  names(sort_values) <- labels
  
  for (i in seq_len(n)) {
    if (grepl("^expectation_", sort_by)) {
      sort_stat <- sub("^expectation_", "", sort_by)
      sort_values[[labels[i]]] <- get_sort_value(exp_L[i], exp_U[i], sort_stat)
    } else if (sort_by == "gap_opt") {
      sort_values[[labels[i]]] <- gap_opt[i]
    } else if (sort_by == "gap_pes") {
      sort_values[[labels[i]]] <- gap_pes[i]
    } else if (sort_by == "gap_mid") {
      sort_values[[labels[i]]] <- gap_mid[i]
    } else {
      sort_values[[labels[i]]] <- NA_real_
    }
  }
  
  labels_sorted <- apply_sort_order(labels, sort_values, sort_by, sort_order)
  
  # Reverse for horizontal plot (so first item appears at top)
  labels_sorted <- rev(labels_sorted)
  
  # ---- long format for plotting ----
  df <- data.frame(
    Label = labels,
    Per_L = per_L, Per_U = per_U, Sat_Point = sv_perception,
    Exp_L = exp_L, Exp_U = exp_U, Imp_Point = sv_expectation,
    Gap_Opt = gap_opt, Gap_Pes = gap_pes, Gap_Mid = gap_mid,
    stringsAsFactors = FALSE
  )
  
  S_df <- data.frame(
    Label = df$Label, Measure = "P",
    Min = df$Per_L, Max = df$Per_U, Point = df$Per_Point,
    stringsAsFactors = FALSE
  )
  
  I_df <- data.frame(
    Label = df$Label, Measure = "E",
    Min = df$Exp_L, Max = df$Exp_U, Point = df$Exp_Point,
    stringsAsFactors = FALSE
  )
  
  both <- rbind(S_df, I_df)
  
  # ---- aesthetics ----
  col_P_segment <- "#2E86C1"  # perception interval
  col_P_point   <- "#D85040"  # perception point
  col_E_segment <- "#85C1E9"  # expectation interval
  col_E_point   <- "#E8A090"  # expectation point
  
  pd <- ggplot2::position_dodge(width = 0.55)
  
  # x limits
  if (is.null(x_lim)) {
    x_min <- min(c(per_L, exp_L), na.rm = TRUE) - 0.2
    x_max <- max(c(per_U, exp_U), na.rm = TRUE) + 0.2
  } else {
    x_min <- x_lim[1]
    x_max <- x_lim[2]
  }
  
  p <- ggplot2::ggplot()
  
  if (type == "IV") {
    p <- p +
      ggplot2::geom_segment(
        data = dplyr::filter(both, Measure == "E"),
        ggplot2::aes(
          y = factor(Label, levels = labels_sorted),
          yend = factor(Label, levels = labels_sorted),
          x = Min, xend = Max, group = Measure
        ),
        color = col_E_segment, position = pd, linewidth = exp_lw
      ) +
      ggplot2::geom_segment(
        data = dplyr::filter(both, Measure == "P"),
        ggplot2::aes(
          y = factor(Label, levels = labels_sorted),
          yend = factor(Label, levels = labels_sorted),
          x = Min, xend = Max, group = Measure
        ),
        color = col_P_segment, position = pd, linewidth = per_lw
      )
    
    if (show_sv_points) {
      p <- p +
        ggplot2::geom_point(
          data = dplyr::filter(both, Measure == "P"),
          ggplot2::aes(y = factor(Label, levels = labels_sorted), x = Point, group = Measure),
          shape = 1, stroke = sv_stroke, color = col_P_point, position = pd, size = sv_size
        ) +
        ggplot2::geom_point(
          data = dplyr::filter(both, Measure == "E"),
          ggplot2::aes(y = factor(Label, levels = labels_sorted), x = Point, group = Measure),
          shape = 1, stroke = sv_stroke, color = col_E_point, position = pd, size = sv_size
        )
    }
  } else {
    p <- p +
      ggplot2::geom_point(
        data = dplyr::filter(both, Measure == "P"),
        ggplot2::aes(y = factor(Label, levels = labels_sorted), x = Point),
        shape = 16, color = col_P_point, size = 3, position = pd
      ) +
      ggplot2::geom_point(
        data = dplyr::filter(both, Measure == "E"),
        ggplot2::aes(y = factor(Label, levels = labels_sorted), x = Point),
        shape = 16, color = col_E_point, size = 3, position = pd
      )
  }
  
  p +
    ggplot2::scale_x_continuous(limits = c(x_min, x_max)) +
    ggplot2::labs(x = "Rating", y = NULL, title = title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.6),
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}
# ==============================================================================
# IVD SUMMARY PLOT (Core plotting function)
# ==============================================================================

#' Plot IVD summary from prepared data
#'
#' Core plotting function for IVD summary. Takes pre-computed data from
#' prepare_ivd_summary_data() and creates the visualization.
#'
#' @param ivd_data List from prepare_ivd_summary_data() containing:
#'   segments, ordered_items, item_order, defuzz_values, y_pos_map
#' @param labels Optional custom labels for items
#' @param title Plot title
#' @param prefix Label prefix (used for default title)
#' @param domain Scale domain c(min, max)
#' @param show_sv Show defuzzified points
#'
#' @return ggplot object
plot_ivd_summary_core <- function(ivd_data, labels = NULL, title = NULL,
                                  prefix = "", domain = c(1, 5), 
                                  show_sv = TRUE) {
  
  # Handle empty data
  if (is.null(ivd_data$segments) || nrow(ivd_data$segments) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No IVD data"))
  }
  
  df <- ivd_data$segments
  ordered_items <- ivd_data$ordered_items
  item_order <- ivd_data$item_order
  defuzz_values <- ivd_data$defuzz_values
  y_pos_map <- ivd_data$y_pos_map
  
  # Create labels
  if (is.null(labels)) {
    y_labels <- ordered_items
  } else {
    y_labels <- sapply(ordered_items, function(it) {
      idx <- item_order[[it]]
      if (!is.null(idx) && idx <= length(labels)) labels[idx]
      else it
    })
  }
  
  if (is.null(title)) title <- paste0(prefix, " Series IVD Summary")
  
  # Build the plot
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x, xend = xend, y = y_pos, yend = y_pos, color = color),
      linewidth = 0.8
    ) +
    ggplot2::scale_color_identity()
  
  # Add defuzzified points
  if (show_sv && length(defuzz_values) > 0) {
    points_df <- data.frame(
      item = names(defuzz_values),
      defuzz_x = unlist(defuzz_values),
      stringsAsFactors = FALSE
    )
    points_df$y_pos <- y_pos_map[points_df$item]
    points_df <- points_df[!is.na(points_df$defuzz_x) & !is.na(points_df$y_pos), ]
    
    if (nrow(points_df) > 0) {
      p <- p + ggplot2::geom_point(
        data = points_df,
        ggplot2::aes(x = defuzz_x, y = y_pos),
        color = "darkblue", shape = 1, size = 1.4, stroke = 0.7
      )
    }
  }
  
  p +
    ggplot2::scale_y_continuous(
      breaks = seq_along(ordered_items), 
      labels = y_labels, 
      trans = "reverse"
    ) +
    ggplot2::scale_x_continuous(limits = domain, breaks = seq(domain[1], domain[2])) +
    ggplot2::labs(x = "Rating", y = "Question", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 13, hjust = 0.5, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}