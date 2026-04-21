# ==============================================================================
# plot_helpers.R - Single-Item Plotting Helper Functions
# ==============================================================================
#
# Internal plotting functions for individual IAA/RFH/IVD plots.
# Used by plot_distributions() to create batch plots.
#
# Functions:
#   plot_iaa_single()     - Plot single IAA curve
#   plot_rfh_single()     - Plot single RFH histogram
#   plot_iaa_rfh_single() - Plot IAA + RFH overlay
#   plot_iaa_ivd_single() - Plot IAA + IVD intervals
#   arrange_plots()       - Arrange multiple plots in grid
# ==============================================================================

#' Plot single IAA curve
#'
#' @param iaa_data Data frame with x and y columns
#' @param domain Scale domain c(min, max)
#' @param title Plot title
#' @param y_max Maximum y-axis value (auto if NULL)
#' @param base_size Base font size
#' @return ggplot object
plot_iaa_single <- function(iaa_data, domain = c(1, 5), title = "", 
                            y_max = NULL, base_size = 10) {
  if (is.null(y_max)) y_max <- ceiling(max(iaa_data$y, na.rm = TRUE) * 5 + 1) / 5
  
  ggplot2::ggplot(iaa_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_step(color = "#3498db", linewidth = 1, direction = "hv") +
    ggplot2::scale_y_continuous(limits = c(-0.05, y_max), 
                                breaks = seq(0, ceiling(y_max * 5) / 5, 0.2)) +
    ggplot2::scale_x_continuous(limits = domain, breaks = seq(domain[1], domain[2])) +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = base_size, hjust = 0.5))
}

#' Plot single RFH histogram
#'
#' @param rfh_data Data frame with Rating and RelativeFreq columns
#' @param domain Scale domain c(min, max)
#' @param title Plot title
#' @param y_max Maximum y-axis value (auto if NULL)
#' @param base_size Base font size
#' @return ggplot object
plot_rfh_single <- function(rfh_data, domain = c(1, 5), title = "", 
                            y_max = NULL, base_size = 10) {
  if (is.null(y_max)) y_max <- ceiling(max(rfh_data$RelativeFreq, na.rm = TRUE) * 5 + 1) / 5
  
  ggplot2::ggplot(rfh_data, ggplot2::aes(x = Rating, y = RelativeFreq)) +
    ggplot2::geom_bar(stat = "identity", fill = "black", color = "black", 
                      alpha = 0.3, width = 1) +
    ggplot2::scale_y_continuous(limits = c(-0.05, y_max), 
                                breaks = seq(0, ceiling(y_max * 5) / 5, 0.2)) +
    ggplot2::scale_x_continuous(limits = c(domain[1] - 0.5, domain[2] + 0.5), 
                                breaks = seq(domain[1], domain[2])) +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = base_size, hjust = 0.5))
}

#' Plot IAA + RFH overlay
#'
#' @param iaa_data Data frame with x and y columns
#' @param rfh_data Data frame with Rating and RelativeFreq columns
#' @param domain Scale domain c(min, max)
#' @param title Plot title
#' @param y_max Maximum y-axis value (auto if NULL)
#' @param base_size Base font size
#' @return ggplot object
plot_iaa_rfh_single <- function(iaa_data, rfh_data, domain = c(1, 5), title = "", 
                                y_max = NULL, base_size = 10) {
  if (is.null(y_max)) {
    max_y <- max(c(iaa_data$y, rfh_data$RelativeFreq), na.rm = TRUE)
    y_max <- ceiling(max_y * 5 + 1) / 5
  }
  
  p <- ggplot2::ggplot()
  
  if (!is.null(rfh_data) && nrow(rfh_data) > 0) {
    p <- p + ggplot2::geom_bar(
      data = rfh_data,
      ggplot2::aes(x = Rating, y = RelativeFreq),
      stat = "identity", fill = "black", color = "black", alpha = 0.3, width = 1
    )
  }
  
  if (!is.null(iaa_data) && nrow(iaa_data) > 0) {
    p <- p + ggplot2::geom_step(
      data = iaa_data,
      ggplot2::aes(x = x, y = y),
      color = "#3498db", linewidth = 1, direction = "hv"
    )
  }
  
  p + ggplot2::scale_y_continuous(limits = c(-0.05, y_max), 
                                  breaks = seq(0, ceiling(y_max * 5) / 5, 0.2)) +
    ggplot2::scale_x_continuous(limits = c(domain[1] - 0.5, domain[2] + 0.5), 
                                breaks = seq(domain[1], domain[2])) +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = base_size, hjust = 0.5))
}

#' Plot IAA + IVD intervals
#'
#' @param iaa_data Original IAA data frame
#' @param iaa_denoised Denoised IAA data frame
#' @param ivd List of intervals from IVD
#' @param domain Scale domain c(min, max)
#' @param title Plot title
#' @param y_max Maximum y-axis value (auto if NULL)
#' @param base_size Base font size
#' @return ggplot object
plot_iaa_ivd_single <- function(iaa_data, iaa_denoised, ivd, domain = c(1, 5), 
                                title = "", y_max = NULL, base_size = 10) {
  if (is.null(y_max)) {
    y_max <- ceiling(max(c(iaa_data$y, iaa_denoised$y), na.rm = TRUE) * 5 + 1) / 5
  }
  palette <- default_ivd()$palette
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_step(data = iaa_data, ggplot2::aes(x = x, y = y),
                       color = "#3498db", linewidth = 0.8, direction = "hv") +
    ggplot2::geom_step(data = iaa_denoised, ggplot2::aes(x = x, y = y),
                       color = "#aaaaaa", linewidth = 0.4, direction = "hv")
  
  if (!is.null(ivd) && length(ivd) > 0) {
    for (i in seq_along(ivd)) {
      if (!is.null(ivd[[i]])) {
        interval <- ivd[[i]]
        in_interval <- iaa_data$x >= interval[1] & iaa_data$x <= interval[2]
        max_val <- if (any(in_interval)) max(iaa_data$y[in_interval], na.rm = TRUE) else 0
        breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0001)
        bin_idx <- pmax(1, pmin(length(palette), 
                                findInterval(max_val / 0.6, breaks, rightmost.closed = TRUE)))
        color <- palette[bin_idx]
        
        seg_df <- data.frame(
          x = interval[1], xend = interval[2],
          y = -0.03, yend = -0.03
        )
        
        p <- p + ggplot2::geom_segment(
          data = seg_df,
          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          inherit.aes = FALSE,
          color = color, linewidth = 1
        )
        
      }
    }
  }
  
  p + ggplot2::scale_y_continuous(limits = c(-0.08, y_max), 
                                  breaks = seq(0, ceiling(y_max * 5) / 5, 0.2)) +
    ggplot2::scale_x_continuous(limits = domain, breaks = seq(domain[1], domain[2])) +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = base_size, hjust = 0.5))
}

#' Arrange multiple plots in a grid
#'
#' @param plots List of ggplot objects
#' @param ncol Number of columns
#' @return Combined plot (ggarrange object)
arrange_plots <- function(plots, ncol = 7) {
  n_plots <- length(plots)
  if (n_plots == 0) return(NULL)
  
  nrow <- ceiling(n_plots / ncol)
  
  ggpubr::ggarrange(
    plotlist = plots,
    ncol = ncol,
    nrow = nrow
  )
}
