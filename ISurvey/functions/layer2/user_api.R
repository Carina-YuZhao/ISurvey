# ==============================================================================
# user_api.R - Layer 2 User-Facing API Functions
# ==============================================================================
#
# USER API - Primary Functions:
# -----------------------------------------------------------------------------
#   Data Loading:
#     load_data()              - Load CSV data with schema
#
#   Analysis:
#     calc_descriptives()      - Compute IV/SV descriptive statistics
#     calc_reliability()       - Compute Cronbach's Alpha (IV and SV)
#     calc_gap()               - Gap analysis (from applied/gap.R)
#     calc_csi()               - Customer Satisfaction Index (from applied/csi.R)
#     calc_cor()               - Correlation coefficients (from psychometrics/)
#
#   Results Export:
#     extract_descriptives_df() - Convert calc_descriptives output to data.frame
#     save_results()            - Save multiple results to Excel workbook
#
#   Visualization:
#     plot_means()             - Interval means plot (from viz/plots.R)
#     plot_correlation()       - Correlation scatter plot (from viz/plots.R)
#     plot_ipa()               - Importance-Performance Analysis (from viz/plots.R)
#     plot_gap()               - Gap analysis plot (from viz/plots.R)
#     plot_ivd_summary()       - IVD summary for multiple items
#     plot_distributions()     - Batch IAA/RFH/IVD plots
#     save_plot()              - Save plot to file
#
#   Data Utilities:
#     filter_by_choice()       - Filter data by demographic labels
#     list_choices()           - List available demographic options
#     extract_iv()             - Extract IV data for single item
#     extract_sv()             - Extract SV data for single item
#
# Internal helpers are in l2_internal.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggpubr)
  library(openxlsx)
})

# Core functions
source("functions/data/type_detection.R")
source("functions/data/schema.R")
source("functions/data/extract.R")
source("functions/descriptive_stats/utils.R")
source("functions/descriptive_stats/distance.R")
source("functions/descriptive_stats/arithmetic.R")

# Analysis functions
source("functions/ranking/interval_ranking.R")
source("functions/ranking/fuzzy_ranking.R")
source("functions/descriptive_stats/iaa.R")
source("functions/descriptive_stats/ivd.R")
source("functions/applied/gap.R")
source("functions/applied/csi.R")
source("functions/psychometrics/cronbach_alpha.R")
source("functions/psychometrics/correlation.R")

# Visualization
source("functions/applied/plots.R")
source("functions/applied/plot_helpers.R")

# Internal helpers
source("functions/layer2/l2_internal.R")

# ==============================================================================
# DEFAULT CONSTANTS
# ==============================================================================

DEFAULT_PLOT_SIZE <- c(width = 8, height = 6)
DEFAULT_PLOT_DPI <- 300

# ==============================================================================
# DATA LOADING
# ==============================================================================

#' Load and prepare data from CSV
load_data <- function(path, schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (schema$trim_item && schema$item_col %in% names(df)) {
    df[[schema$item_col]] <- trimws(df[[schema$item_col]])
  }
  df
}

# ==============================================================================
# DATA EXTRACTION (User-facing)
# ==============================================================================

#' Extract raw IV data for one item
#' @return Data frame with: Participant, Min, Max
extract_iv <- function(data, item, schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  data %>%
    dplyr::filter(!!rlang::sym(schema$item_col) == item,
                  !!rlang::sym(schema$type_col) == schema$type_iv) %>%
    dplyr::select(Participant = !!rlang::sym(schema$id_col),
                  Min = !!rlang::sym(schema$iv_min_col),
                  Max = !!rlang::sym(schema$iv_max_col)) %>%
    dplyr::filter(!is.na(Min) & !is.na(Max))
}

#' Extract raw SV data for one item
#' @param data Data frame
#' @param item Item name
#' @param standardize Optional standardization: list(from = c(old_min, old_max), to = c(new_min, new_max))
#'   Example: list(from = c(0, 4), to = c(1, 5)) converts 0-4 scale to 1-5
#' @param schema Optional schema
#' @return Data frame with: Participant, Value
extract_sv <- function(data, item, standardize = NULL, schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  
  result <- data %>%
    dplyr::filter(!!rlang::sym(schema$item_col) == item,
                  !!rlang::sym(schema$type_col) == schema$type_sv) %>%
    dplyr::select(Participant = !!rlang::sym(schema$id_col),
                  Value = !!rlang::sym(schema$sv_col)) %>%
    dplyr::filter(!is.na(Value))
  
  # Apply standardization if provided
  if (!is.null(standardize) && is.list(standardize)) {
    from <- standardize$from
    to <- standardize$to
    if (!is.null(from) && !is.null(to) && length(from) == 2 && length(to) == 2) {
      # Linear transformation: new = (old - from[1]) / (from[2] - from[1]) * (to[2] - to[1]) + to[1]
      result$Value <- (result$Value - from[1]) / (from[2] - from[1]) * (to[2] - to[1]) + to[1]
    }
  }
  
  result
}

# ==============================================================================
# CHOOSE ONE (Categorical) DATA EXTRACTION AND FILTERING
# ==============================================================================

#' List all Choose One pages and their labels
#'
#' Identifies all pages with "Choose One" response type and lists
#' the available labels for each page.
#'
#' @param data Data frame from load_data()
#' @param schema Optional schema
#' @return Named list: page names as keys, character vectors of labels as values
#'
#' @examples
#' choices <- list_choices(data)
#' # $Gender: c("Female", "Male", "Other")
#' # $Age: c("18-24 years", "25-34 years", ...)
#'
#' @export
list_choices <- function(data, schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  
  # Filter to Choose One rows
  choice_data <- data[data[[schema$type_col]] == schema$type_choice, ]
  
  if (nrow(choice_data) == 0) {
    message("No 'Choose One' pages found in data.")
    return(list())
  }
  
  # Get unique page names
  pages <- unique(choice_data[[schema$item_col]])
  if (schema$trim_item) pages <- trimws(pages)
  
  # For each page, get unique labels
  result <- lapply(pages, function(page) {
    page_data <- choice_data[trimws(choice_data[[schema$item_col]]) == page, ]
    labels <- unique(page_data[[schema$choice_label_col]])
    labels <- labels[!is.na(labels) & labels != ""]
    sort(labels)
  })
  names(result) <- pages
  
  result
}

#' Filter data by choice labels
#'
#' Filters data to include only participants who selected ALL specified labels.
#' Labels can be from different pages (e.g., Gender="Female" AND Age="25-34 years").
#'
#' @param data Data frame from load_data()
#' @param labels Character vector of labels to filter by (must match ALL)
#' @param schema Optional schema
#' @return Filtered data frame containing only matching participants
#'
#' @examples
#' # Filter to females aged 25-34
#' data_subset <- filter_by_choice(data, c("Female", "25-34 years"))
#'
#' # Filter to male desktop users
#' data_subset <- filter_by_choice(data, c("Male", "Desktop"))
#'
#' @export
filter_by_choice <- function(data, labels, schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  
  if (length(labels) == 0) {
    return(data)
  }
  
  # Get all Choose One rows
  choice_data <- data[data[[schema$type_col]] == schema$type_choice, ]
  
  # Find participants who have ALL specified labels
  matching_participants <- NULL
  
  for (label in labels) {
    # Find participants with this label
    participants_with_label <- unique(
      choice_data[[schema$id_col]][choice_data[[schema$choice_label_col]] == label]
    )
    
    if (length(participants_with_label) == 0) {
      warning(sprintf("Label '%s' not found in data.", label))
      return(data[0, ])  # Return empty data frame with same structure
    }
    
    if (is.null(matching_participants)) {
      matching_participants <- participants_with_label
    } else {
      matching_participants <- intersect(matching_participants, participants_with_label)
    }
  }
  
  if (length(matching_participants) == 0) {
    warning("No participants match all specified labels.")
    return(data[0, ])
  }
  
  message(sprintf("Filtered to %d participants matching: %s", 
                  length(matching_participants), 
                  paste(labels, collapse = ", ")))
  
  # Return filtered data
  data[data[[schema$id_col]] %in% matching_participants, ]
}

# ==============================================================================
# RELIABILITY ANALYSIS (Cronbach's Alpha - Unified Interface)
# ==============================================================================

#' Calculate Cronbach's Alpha for multiple data sources and item groups
#'
#' Computes reliability (Cronbach's alpha) for all combinations of data sources
#' and item groups. When items have both IV and SV data, both are computed.
#'
#' Rules:
#' - If IV data exist: compute alpha using the requested IV method(s) ("theta", "hausdorff").
#' - If SV data exist: compute classic SV alpha.
#' - If both IV and SV exist for the same items: compute all applicable methods.
#'
#' @param ... One or more data sources (data frames), max 2
#' @param items Named list of item groups, e.g., list(S = ITEMS_S, I = ITEMS_I)
#'              Or a single character vector for one group
#' @param method Character vector of IV methods: "theta", "hausdorff"
#'               Default NULL = c("theta","hausdorff")
#' @param na_action NA handling: "listwise" (default), "pairwise", "mean", "median"
#' @param standardize Optional standardization for SV data
#' @param schema Optional schema
#'
#' @return Data frame with columns: data, items, method, alpha, n_items
#'
#' @export
calc_reliability <- function(...,
                             items,
                             method = NULL,
                             na_action = c("listwise", "pairwise", "mean", "median"),
                             standardize = NULL,
                             schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  na_action <- match.arg(na_action)
  
  # Collect data sources (max 2)
  data_list <- list(...)
  if (length(data_list) == 0) {
    stop("At least one data source must be provided.")
  }
  if (length(data_list) > 2) {
    stop("Maximum 2 data sources allowed.")
  }
  
  # Get data names from call
  call_args <- as.list(match.call())[-1]
  call_args <- call_args[!names(call_args) %in% c("items", "method", "na_action", "standardize", "schema")]
  data_names <- sapply(call_args, deparse)
  if (length(data_names) < length(data_list)) {
    data_names <- c(data_names, paste0("data", seq(length(data_names) + 1, length(data_list))))
  }
  
  # Store data sources in a list (do NOT rbind - they may have different columns)
  data_sources <- data_list
  combined_name <- paste(data_names, collapse = "+")
  
  # Helper: build IV items from multiple data sources (aligned by participant)
  build_iv_items_multi <- function(sources, items, schema) {
    # First, collect all IV data and find all unique participants
    all_iv_data <- list()
    all_participants <- c()
    
    for (item in items) {
      iv_df <- extract_iv_multi(sources, item, schema)
      if (nrow(iv_df) > 0) {
        all_iv_data[[item]] <- iv_df
        all_participants <- union(all_participants, iv_df$Participant)
      }
    }
    
    if (length(all_iv_data) == 0) return(list())
    
    all_participants <- sort(all_participants)
    n <- length(all_participants)
    
    # Build aligned list of items (same format as build_iv_items)
    result <- lapply(names(all_iv_data), function(item) {
      iv_df <- all_iv_data[[item]]
      
      # Align to all participants
      min_vals <- rep(NA_real_, n)
      max_vals <- rep(NA_real_, n)
      
      idx <- match(iv_df$Participant, all_participants)
      min_vals[idx] <- iv_df$Min
      max_vals[idx] <- iv_df$Max
      
      data.frame(Min = min_vals, Max = max_vals)
    })
    names(result) <- names(all_iv_data)
    result
  }
  
  # Helper: build SV matrix from multiple data sources
  build_sv_matrix_multi <- function(sources, items, standardize, schema) {
    sv_list <- list()
    all_participants <- c()
    
    for (item in items) {
      item_data <- lapply(sources, function(src) {
        tryCatch(extract_sv(src, item, standardize = standardize, schema = schema), error = function(e) NULL)
      })
      item_data <- item_data[!sapply(item_data, is.null)]
      if (length(item_data) > 0) {
        combined <- do.call(rbind, item_data)
        sv_list[[item]] <- combined
        all_participants <- union(all_participants, combined$Participant)
      }
    }
    
    if (length(sv_list) == 0 || length(all_participants) == 0) {
      return(matrix(nrow = 0, ncol = length(items)))
    }
    
    # Build matrix
    mat <- matrix(NA_real_, nrow = length(all_participants), ncol = length(items))
    colnames(mat) <- items
    rownames(mat) <- all_participants
    
    for (item in names(sv_list)) {
      df <- sv_list[[item]]
      for (i in seq_len(nrow(df))) {
        mat[as.character(df$Participant[i]), item] <- df$Value[i]
      }
    }
    
    mat
  }
  
  # Get all available items from all sources
  all_available_items <- unique(unlist(lapply(data_sources, function(src) {
    unique(src[[schema$item_col]])
  })))
  
  # Normalize items to named list
  if (!is.list(items) || is.data.frame(items)) {
    items <- list(items = items)
  }
  if (is.null(names(items))) {
    names(items) <- paste0("group", seq_along(items))
  }
  
  # Default IV methods
  if (is.null(method)) {
    method <- c("theta", "hausdorff")
  }
  method <- tolower(method)
  method <- unique(method)
  
  # Validate methods
  valid_methods <- c("theta", "hausdorff")
  bad <- setdiff(method, valid_methods)
  if (length(bad) > 0) {
    stop("Unknown method(s): ", paste(bad, collapse = ", "),
         ". Allowed: ", paste(valid_methods, collapse = ", "))
  }
  
  results <- list()
  
  # Iterate over item groups
  for (items_name in names(items)) {
    item_vec <- items[[items_name]]
    
    # Check which items exist in data sources
    available_items <- intersect(item_vec, all_available_items)
    if (length(available_items) == 0) {
      message(sprintf("   Note: No data for items '%s', skipping.", items_name))
      next
    }
    if (length(available_items) < length(item_vec)) {
      missing <- setdiff(item_vec, available_items)
      message(sprintf("   Note: Missing %d items from '%s': %s",
                      length(missing), items_name,
                      paste(head(missing, 3), collapse = ", ")))
    }
    
    # Check data types for these items by trying to extract
    iv_items <- build_iv_items_multi(data_sources, available_items, schema)
    sv_mat <- build_sv_matrix_multi(data_sources, available_items, standardize, schema)
    has_iv <- length(iv_items) > 0
    has_sv <- nrow(sv_mat) > 0
    
    # Report if both exist
    if (has_iv && has_sv) {
      message(sprintf("   Note: Items '%s' have both IV and SV data, computing all.", items_name))
    }
    
    # Compute IV alpha if IV data exists
    if (has_iv) {
      for (m in method) {
        alpha_val <- calc_alpha(iv_items, method = m, na_action = na_action)
        
        results[[length(results) + 1]] <- data.frame(
          data = combined_name,
          items = items_name,
          method = m,
          alpha = alpha_val,
          n_items = length(available_items),
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Compute SV alpha if SV data exists
    if (has_sv) {
      alpha_val <- calc_alpha(sv_mat, method = "classic", na_action = na_action)
      
      results[[length(results) + 1]] <- data.frame(
        data = combined_name,
        items = items_name,
        method = "sv",
        alpha = alpha_val,
        n_items = length(available_items),
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(results) == 0) {
    message("   No valid combinations found.")
    return(data.frame(
      data = character(),
      items = character(),
      method = character(),
      alpha = numeric(),
      n_items = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind, results)
}

# ==============================================================================
# DESCRIPTIVE STATISTICS (BUNDLE OUTPUT)
# ==============================================================================

#' Compute descriptive statistics for survey items (bundle output)
#'
#' Automatically detects data type and computes appropriate statistics.
#' Can handle IV data, SV data, or mixed data in one or two data sources.
#' If an item has both IV and SV responses (same Page Name), both are processed.
#' Supports multiple item groups via named list.
#'
#' Returned object is a bundle (list) so users can access:
#'   - stats$theta     (data.frame L/U, rownames = items)
#'   - stats$hausdorff (data.frame L/U, rownames = items)
#'   - stats$sv        (named numeric vector, names = items)
#'   - stats$*_var     (named numeric vectors)
#'   - stats$N_IV / stats$N_SV (named integer vectors)
#'
#' @param data1 First dataset
#' @param data2 Optional second dataset
#' @param items Character vector of item names, OR named list for multiple groups
#'              e.g., list(I = paste0("I", 1:14), S = paste0("S", 1:14))
#'              If list, results are combined from all groups.
#' @param method Which IV frameworks to compute: "both", "theta", "hausdorff"
#' @param standardize Optional standardization for SV data
#' @param schema Optional schema
#'
#' @return A named list (bundle) with elements such as $theta, $hausdorff, $sv, etc.
#'
#' @export
calc_descriptives <- function(data1, data2 = NULL, items = NULL,
                              method = c("both", "theta", "hausdorff"),
                              standardize = NULL,
                              schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  method <- match.arg(method)
  
  # Normalize items to character vector
  if (is.list(items) && !is.data.frame(items)) {
    all_items <- unique(unlist(items))
  } else if (is.null(items)) {
    all_items <- unique(c(
      data1[[schema$item_col]],
      if (!is.null(data2)) data2[[schema$item_col]] else NULL
    ))
  } else {
    all_items <- items
  }
  
  # Store data sources in a list (do NOT rbind - they may have different columns)
  data_sources <- list(data1)
  if (!is.null(data2)) {
    data_sources <- list(data1, data2)
  }
  
  # Check which items have IV/SV data and detect items with both
  has_iv <- has_sv <- FALSE
  items_with_both <- character(0)
  
  for (item in all_items) {
    iv_df <- extract_iv_multi(data_sources, item, schema)
    sv_df <- extract_sv_multi(data_sources, item, standardize, schema)
    item_has_iv <- nrow(iv_df) > 0
    item_has_sv <- nrow(sv_df) > 0
    
    if (item_has_iv) has_iv <- TRUE
    if (item_has_sv) has_sv <- TRUE
    if (item_has_iv && item_has_sv) {
      items_with_both <- c(items_with_both, item)
    }
  }
  
  # Report items with both IV and SV
  if (length(items_with_both) > 0) {
    message(sprintf("   Note: %d item(s) have both IV and SV data: %s",
                    length(items_with_both),
                    paste(head(items_with_both, 5), collapse = ", ")))
    if (length(items_with_both) > 5) message("   ...")
  }
  
  # Pre-allocate outputs
  n_items <- length(all_items)
  
  N_IV <- rep(0L, n_items); names(N_IV) <- all_items
  N_SV <- rep(0L, n_items); names(N_SV) <- all_items
  
  theta_L <- theta_U <- theta_var <- rep(NA_real_, n_items)
  names(theta_L) <- names(theta_U) <- names(theta_var) <- all_items
  haus_L  <- haus_U  <- haus_var  <- rep(NA_real_, n_items)
  names(haus_L)  <- names(haus_U)  <- names(haus_var)  <- all_items
  sv_mean <- sv_var  <- rep(NA_real_, n_items)
  names(sv_mean) <- names(sv_var) <- all_items
  
  # Compute stats per item
  for (item in all_items) {
    # Try IV
    iv_df <- extract_iv_multi(data_sources, item, schema)
    n_iv <- nrow(iv_df)
    N_IV[item] <- n_iv
    
    if (n_iv > 0) {
      if (method %in% c("both", "theta")) {
        th <- compute_mean(iv_df, method = "theta")
        theta_L[item] <- th$L
        theta_U[item] <- th$U
        theta_var[item] <- th$var
      }
      if (method %in% c("both", "hausdorff")) {
        ha <- compute_mean(iv_df, method = "hausdorff")
        haus_L[item] <- ha$L
        haus_U[item] <- ha$U
        haus_var[item] <- ha$var
      }
    }
    
    # Try SV
    sv_df <- extract_sv_multi(data_sources, item, standardize, schema)
    n_sv <- nrow(sv_df)
    N_SV[item] <- n_sv
    
    if (n_sv > 0) {
      sv_mean[item] <- mean(sv_df$Value, na.rm = TRUE)
      sv_var[item] <- if (n_sv > 1) var(sv_df$Value, na.rm = TRUE) else NA_real_
    }
  }
  
  out <- list(
    items = all_items,
    N_IV = if (has_iv) N_IV else NULL,
    N_SV = if (has_sv) N_SV else NULL
  )
  
  # Attach IV bundles (data.frame(L,U) with rownames=items)
  if (has_iv && method %in% c("both", "theta")) {
    out$theta <- data.frame(L = unname(theta_L), U = unname(theta_U), row.names = all_items)
    out$theta_var <- unname(theta_var); names(out$theta_var) <- all_items
  }
  if (has_iv && method %in% c("both", "hausdorff")) {
    out$hausdorff <- data.frame(L = unname(haus_L), U = unname(haus_U), row.names = all_items)
    out$haus_var <- unname(haus_var); names(out$haus_var) <- all_items
  }
  
  # Attach SV bundles
  if (has_sv) {
    out$sv <- unname(sv_mean); names(out$sv) <- all_items
    out$sv_var <- unname(sv_var); names(out$sv_var) <- all_items
  }
  
  # Meta
  out$meta <- list(
    has_iv = has_iv,
    has_sv = has_sv,
    method = method,
    standardize = standardize
  )
  
  out
}

#' Convert calc_descriptives bundle to data.frame (for Excel export)
#'
#' @param stats Bundle output from calc_descriptives()
#' @return Data frame with all statistics in columns
#' @export
extract_descriptives_df <- function(stats) {
  items <- stats$items
  n <- length(items)
  
  result <- data.frame(Item = items, stringsAsFactors = FALSE)
  
  # Add N counts
  if (!is.null(stats$N_IV)) result$N_IV <- as.integer(stats$N_IV[items])
  if (!is.null(stats$N_SV)) result$N_SV <- as.integer(stats$N_SV[items])
  
  # Add theta
  if (!is.null(stats$theta)) {
    result$Theta_Mean_L <- stats$theta[items, "L"]
    result$Theta_Mean_U <- stats$theta[items, "U"]
  }
  if (!is.null(stats$theta_var)) {
    result$Theta_Var <- as.numeric(stats$theta_var[items])
  }
  
  # Add hausdorff
  if (!is.null(stats$hausdorff)) {
    result$Haus_Mean_L <- stats$hausdorff[items, "L"]
    result$Haus_Mean_U <- stats$hausdorff[items, "U"]
  }
  if (!is.null(stats$haus_var)) {
    result$Haus_Var <- as.numeric(stats$haus_var[items])
  }
  
  # Add SV
  if (!is.null(stats$sv)) {
    result$SV_Mean <- as.numeric(stats$sv[items])
  }
  if (!is.null(stats$sv_var)) {
    result$SV_Var <- as.numeric(stats$sv_var[items])
  }
  
  result
}

# ==============================================================================
# IVD SUMMARY PLOT
# ==============================================================================

#' Plot IVD summary for multiple items
#'
#' High-level function that prepares data and creates IVD summary plot.
#' Internally calls prepare_ivd_summary_data() and plot_ivd_summary_core().
#'
#' @param data1 First data source
#' @param data2 Optional second data source
#' @param items Items to include (character vector or named list)
#' @param prefix Label prefix (used for default title)
#' @param domain Scale domain
#' @param ivd_method IVD method
#' @param denoise_method Denoising method
#' @param denoise_k Denoise window
#' @param denoise_p Polynomial order for savgol
#' @param sort_by Sorting/defuzzification method:
#'   - "none": original order
#'   - "centroid": center of gravity (most common)
#'   - "mom": mean of maximum
#'   - "bisector": bisector method
#'   - "alc": alpha-level centroid for IVD
#'   - "mean_of_max": midpoint of dominant interval
#'   - "weighted_mid": weighted midpoint of all intervals
#'   - "min": minimum of dominant interval
#'   - "max": maximum of dominant interval
#' @param sort_order Sort order: "desc" or "asc"
#' @param show_sv Show defuzzified point markers
#' @param labels Custom labels for items
#' @param title Plot title
#' @param schema Optional schema
#'
#' @return ggplot object
#' @export
plot_ivd_summary <- function(data1, data2 = NULL, items, prefix = "", domain = c(1, 5),
                             ivd_method = "DUW", 
                             denoise_method = "runmed", denoise_k = 51, denoise_p = 3,
                             sort_by = c("none", "centroid", "mom", "bisector", "alc",
                                         "mean_of_max", "weighted_mid", "min", "max"),
                             sort_order = c("desc", "asc"),
                             show_sv = TRUE,
                             labels = NULL, title = NULL, schema = NULL) {
  sort_by <- match.arg(sort_by)
  sort_order <- match.arg(sort_order)
  
  # Prepare data (in l2_internal.R)
  ivd_data <- prepare_ivd_summary_data(
    data1 = data1, data2 = data2, items = items,
    domain = domain, ivd_method = ivd_method,
    denoise_method = denoise_method, denoise_k = denoise_k, denoise_p = denoise_p,
    sort_by = sort_by, sort_order = sort_order, schema = schema
  )
  
  # Plot (in viz/plots.R)
  plot_ivd_summary_core(
    ivd_data = ivd_data, labels = labels, title = title,
    prefix = prefix, domain = domain, show_sv = show_sv
  )
}
# ==============================================================================
# CORRELATION - Now provided by fc_correlation.R
# ==============================================================================
# calc_cor() is now defined in functions/stat/fc_correlation.R
# It automatically detects data type (SV/IV/MIV) and selects appropriate method.
# Default: theta for IV, hausdorff for MIV (required for multi-interval)

# ==============================================================================
# SAVE PLOT
# ==============================================================================
#' Save ggplot object to file
#'
#' @param plot ggplot object
#' @param name File name without extension
#' @param dir Output directory
#' @param size Plot size; either c(width, height) or named c(width=, height=)
#' @param dpi Resolution (default from DEFAULT_PLOT_DPI)
#'
#' @return The input plot (invisibly)
#' @export
save_plot <- function(plot,
                      name,
                      dir = ".",
                      size = DEFAULT_PLOT_SIZE,
                      dpi  = DEFAULT_PLOT_DPI) {
  
  # ---- parse size ----
  if (!is.numeric(size) || length(size) != 2) {
    stop("`size` must be a numeric vector of length 2: c(width, height) or c(width=, height=).")
  }
  
  if (!is.null(names(size))) {
    if (!all(c("width", "height") %in% names(size))) {
      stop("Named `size` must have names 'width' and 'height'.")
    }
    width  <- unname(size["width"])
    height <- unname(size["height"])
  } else {
    width  <- size[1]
    height <- size[2]
  }
  
  # ---- build path ----
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file_path <- file.path(dir, paste0(name, ".png"))
  
  # ---- save ----
  ggplot2::ggsave(
    filename = file_path,
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi,
    bg       = "white"
  )
  
  message(sprintf("   Saved: %s", file_path))
  invisible(plot)
}

# ==============================================================================
# SAVE RESULTS TO EXCEL
# ==============================================================================

#' Save analysis results to Excel
#'
#' Flexible function to save one or more results to an Excel workbook.
#' Automatically detects data types and converts appropriately.
#'
#' @param ... Data to save. Can be:
#'   - Single unnamed argument: saved to sheet "Sheet1" (or use `sheet` param)
#'   - Named arguments: each name becomes a sheet name
#'   - calc_descriptives() bundle: auto-converted via extract_descriptives_df()
#'   - data.frame: written directly
#'   - Other lists: attempts conversion to data.frame
#' @param file Output filename (with or without .xlsx)
#' @param dir Output directory (default: ".")
#' @param sheet Sheet name for single unnamed data (default: "Sheet1")
#'
#' @return Invisible path to saved file
#'
#' @examples
#' # Save single result
#' save_results(df_S, file = "descriptive_S", dir = "output")
#'
#' # Save single result with custom sheet name
#' save_results(df_S, file = "results", sheet = "Satisfaction")
#'
#' # Save multiple results (each name = sheet name)
#' save_results(
#'   Descriptive_S = df_S,
#'   Descriptive_I = df_I,
#'   Gap = gap_results,
#'   file = "results",
#'   dir = "output"
#' )
#'
#' @export
save_results <- function(..., file, dir = ".", sheet = "Sheet1") {
  args <- list(...)
  
  if (length(args) == 0) {
    stop("At least one result must be provided")
  }
  
  # Create directory
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  
  # Add .xlsx extension if missing
  if (!grepl("\\.xlsx$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".xlsx")
  }
  
  file_path <- file.path(dir, file)
  
  # Handle single unnamed argument
  if (length(args) == 1 && is.null(names(args))) {
    names(args) <- sheet
  } else if (is.null(names(args))) {
    # Multiple unnamed args - generate names
    names(args) <- paste0("Sheet", seq_along(args))
  } else {
    # Fill in empty names
    for (i in seq_along(args)) {
      if (is.null(names(args)[i]) || names(args)[i] == "") {
        names(args)[i] <- paste0("Sheet", i)
      }
    }
  }
  
  # Helper: convert data to data.frame
  convert_to_df <- function(data, name) {
    if (is.data.frame(data)) {
      return(data)
    }
    
    # calc_descriptives bundle (has $items)
    if (is.list(data) && "items" %in% names(data)) {
      return(extract_descriptives_df(data))
    }
    
    # Try generic conversion
    tryCatch({
      as.data.frame(data)
    }, error = function(e) {
      warning(sprintf("Could not convert '%s' to data.frame, skipping", name))
      NULL
    })
  }
  
  # Create workbook
  wb <- openxlsx::createWorkbook()
  
  for (sheet_name in names(args)) {
    data <- args[[sheet_name]]
    df <- convert_to_df(data, sheet_name)
    
    if (!is.null(df) && nrow(df) > 0) {
      # Sanitize sheet name (max 31 chars, no special chars)
      safe_name <- substr(gsub("[\\[\\]\\*\\?:/\\\\]", "_", sheet_name), 1, 31)
      openxlsx::addWorksheet(wb, safe_name)
      openxlsx::writeData(wb, safe_name, df)
    }
  }
  
  # Save
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  message(sprintf("   Saved: %s", file_path))
  
  invisible(file_path)
}

# ==============================================================================
# BATCH DISTRIBUTION PLOTTING
# ==============================================================================

#' Plot Distribution(s) for Survey Items
#'
#' Unified function to plot IAA, RFH, or combined distributions.
#' Automatically detects IV/SV data types from data sources.
#'
#' @param data1 First data source
#' @param data2 Optional second data source
#' @param items Character vector of item names, OR named list for multiple groups
#' @param type Distribution type: "iaa" (default), "rfh", "iaa_rfh", "iaa_ivd"
#' @param output_path Output file path
#' @param domain Scale domain c(min, max)
#' @param ivd_method IVD method for type="iaa_ivd"
#' @param denoise_method Denoising method for IAA
#' @param denoise_k Denoising window size
#' @param denoise_p Denoising polynomial degree
#' @param standardize Standardization for SV data
#' @param step IAA step size
#' @param y_max Y-axis maximum
#' @param ncol Number of columns in grid
#' @param dpi Resolution
#' @param labels Custom labels for items
#' @param show_n Show sample size in titles
#' @param base_size Base font size
#' @param width Plot width
#' @param height Plot height
#' @param schema Optional schema
#'
#' @return Combined plot object
#'
#' @examples
#' # Single data source with auto-detection
#' plot_distributions(data, items = paste0("I", 1:7), type = "iaa")
#'
#' # Two data sources
#' plot_distributions(iv_data, sv_data, items = paste0("I", 1:7), type = "iaa_rfh")
#'
#' # Multiple item groups (list)
#' plot_distributions(data, items = list(I = paste0("I", 1:7), S = paste0("S", 1:7)))
#'
#' @export
plot_distributions <- function(data1, data2 = NULL, items,
                               type = c("iaa", "rfh", "iaa_rfh", "iaa_ivd"),
                               output_path = NULL,
                               domain = c(1, 5),
                               ivd_method = "DUW",
                               denoise_method = "runmed", denoise_k = 51, denoise_p = 3,
                               standardize = NULL,
                               step = 0.01, y_max = NULL, ncol = 7, dpi = 300,
                               labels = NULL, show_n = TRUE, base_size = 10,
                               width = NULL, height = NULL, verbose = FALSE, schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  type <- match.arg(type)
  
  # Normalize items to character vector
  if (is.list(items) && !is.data.frame(items)) {
    all_items <- unique(unlist(items))
  } else {
    all_items <- items
  }
  
  # Store data sources in a list (do NOT rbind - they may have different columns)
  data_sources <- list(data1)
  if (!is.null(data2)) {
    data_sources <- list(data1, data2)
  }
  
  # Detect items with both IV and SV data
  items_with_both <- character(0)
  for (item in all_items) {
    iv_df <- extract_iv_multi(data_sources, item, schema)
    sv_df <- extract_sv_multi(data_sources, item, standardize, schema)
    if (nrow(iv_df) > 0 && nrow(sv_df) > 0) {
      items_with_both <- c(items_with_both, item)
    }
  }
  
  if (length(items_with_both) > 0) {
    message(sprintf("   Note: %d item(s) have both IV and SV data: %s",
                    length(items_with_both),
                    paste(head(items_with_both, 5), collapse = ", ")))
    if (length(items_with_both) > 5) message("   ...")
  }
  
  plots <- lapply(seq_along(all_items), function(i) {
    item <- all_items[i]
    
    # Extract data from all sources
    iv_df <- extract_iv_multi(data_sources, item, schema)
    sv_df <- extract_sv_multi(data_sources, item, standardize, schema)
    n_iv <- nrow(iv_df)
    n_sv <- nrow(sv_df)
    
    # Determine title
    if (!is.null(labels) && i <= length(labels)) {
      title <- labels[i]
    } else if (show_n) {
      if (type == "iaa_rfh") {
        title <- sprintf("%s (IV:%d SV:%d)", item, n_iv, n_sv)
      } else if (type %in% c("iaa", "iaa_ivd")) {
        title <- sprintf("%s (n=%d)", item, n_iv)
      } else {  # rfh
        title <- sprintf("%s (n=%d)", item, n_sv)
      }
    } else {
      title <- item
    }
    
    # Generate plot based on type
    if (type == "iaa") {
      if (n_iv == 0) {
        message(sprintf("   Note: Item '%s' has no IV data, skipping.", item))
        return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle(title))
      }
      iaa <- compute_iaa(iv_df, domain, step)
      plot_iaa_single(iaa, domain, title, y_max = y_max, base_size = base_size)
      
    } else if (type == "rfh") {
      if (n_sv == 0) {
        message(sprintf("   Note: Item '%s' has no SV data, skipping.", item))
        return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle(title))
      }
      rfh <- compute_rfh(sv_df, domain)
      plot_rfh_single(rfh, domain, title, y_max = y_max, base_size = base_size)
      
    } else if (type == "iaa_rfh") {
      if (n_iv == 0 && n_sv == 0) {
        message(sprintf("   Note: Item '%s' has no data, skipping.", item))
        return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle(title))
      }
      iaa <- if (n_iv > 0) compute_iaa(iv_df, domain, step) else data.frame(x = numeric(0), y = numeric(0))
      rfh <- if (n_sv > 0) compute_rfh(sv_df, domain) else NULL
      plot_iaa_rfh_single(iaa, rfh, domain, title, y_max = y_max, base_size = base_size)
      
    } else if (type == "iaa_ivd") {
      if (n_iv == 0) {
        message(sprintf("   Note: Item '%s' has no IV data, skipping.", item))
        return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle(title))
      }
      iaa <- compute_iaa(iv_df, domain, step)
      iaa_d <- denoise_iaa(iaa, method = denoise_method, k = denoise_k, p = denoise_p)
      ivd <- compute_ivd(iaa_d, method = ivd_method)
      
      # Debug output (only when verbose = TRUE)
      if (verbose) {
        message(sprintf("   [DEBUG] Item '%s': IVD has %d interval(s)", item, length(ivd)))
        for (ii in seq_along(ivd)) {
          if (!is.null(ivd[[ii]])) {
            message(sprintf("      interval%d: [%.2f, %.2f]", ii, ivd[[ii]][1], ivd[[ii]][2]))
          }
        }
      }
      
      plot_iaa_ivd_single(iaa, iaa_d, ivd, domain, title, y_max = y_max, base_size = base_size)
    }
  })
  names(plots) <- all_items
  
  # Arrange plots into a combined figure
  p <- arrange_plots(plots, ncol)
  
  # Save only if output_path is explicitly provided
  if (!is.null(output_path)) {
    if (is.null(width)) width <- 2 * ncol
    if (is.null(height)) height <- 2 * ceiling(length(plots) / ncol)
    save_plot(p, 
              name = tools::file_path_sans_ext(basename(output_path)),
              dir = dirname(output_path),
              size = c(width = width, height = height),
              dpi = dpi)
  }
  
  # Return the combined plot
  p
}
