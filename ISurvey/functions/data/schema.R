# data/schema.R - Data Schema and Global Options

# CSV column mapping
default_schema <- function() {
  list(
    item_col = "Page Name",
    type_col = "Response Type",
    id_col = "Participant",
    iv_min_col = "Ellipse Scale_minRangeValue",
    iv_max_col = "Ellipse Scale_maxRangeValue",
    sv_col = "Discrete Scale_index",
    choice_label_col = "Choose One_label",
    choice_value_col = "Choose One_value",
    type_iv = "Ellipse Scale",
    type_sv = "Discrete Scale",
    type_choice = "Choose One",
    trim_item = TRUE
  )
}

default_scale <- function() {
  list(domain = c(1, 5))
}

default_iaa <- function() {
  list(step = 0.01, denoise = TRUE, denoise_k = 51)
}

default_ivd <- function() {
  list(
    alpha_levels = seq(0, 1, 0.1),
    method = "DUW",
    palette = c("#F3B39F", "#E07A6B", "#C64A4A", "#A5212E", "#7F0020")
  )
}

default_colors <- function() {
  list(
    iaa_line = "#3498db",
    iaa_denoised = "#999999",
    rfh_fill = "steelblue",
    rfh_alpha = 0.3,
    corr_point = "#2E86C1",
    method_colors = c(theta = "#7f7f7f", hausdorff = "#1f77b4", sv = "#d62728")
  )
}

default_export <- function() {
  list(dpi = 300, bg = "white")
}

# Global analysis options
default_options <- function() {
  list(
    method = "theta",
    theta = 1,
    csi_method = "iwa",
    degenerate = FALSE,
    use_sample_var = FALSE
  )
}
