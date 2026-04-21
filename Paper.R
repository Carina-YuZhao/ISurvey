# ==============================================================================
# Paper.R - Code Examples for Toolkit Paper
# ==============================================================================

source("ISurvey/functions/layer2/user_api.R")

# ==============================================================================
# Configuration
# ==============================================================================

IV_PATH <- "Study/Rail_user_survey__interval_.csv"
SV_PATH <- "Study/Rail_user_survey__5-point_.csv"
OUTPUT_DIR <- "output/paper"

ITEMS_S <- paste0("S", 1:14)
ITEMS_I <- paste0("I", 1:14)
ITEMS_6 <- paste0("I", 1:6)

LABELS <- c(
  "1 Journey Info", "2 Wait Area", "3 Cleanliness", "4 Staff Avail.",
  "5 Staff Att.", "6 Toilet Clean.", "7 Seat Avail.", "8 Cleanliness",
  "9 WiFi Qual.", "10 Seat Space", "11 Staff Avail.", "12 Staff Att.",
  "13 Toilet Clean.", "14 Info Prov."
)
LABELS_6 <- LABELS[1:6]

# Explicit method parameters
GAMMA <- 0.5
IVD_SORT_BY <- "centroid"
GAP_SORT_BY <- "gap_pes"

# Plot settings
Y_MAX <- 0.65
FIG_WIDTH <- 3.5
FIG_HEIGHT <- 1.5

# Make top/bottom margins as small as possible (not zero), keep everything else unchanged
PLOT_MARGIN <- ggallows <- ggplot2::margin(t = 1, r = 5, b = 1, l = 5, unit = "pt")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Apply a global theme update so *all* ggplots (including those created inside helper functions)
# inherit smaller margins unless explicitly overridden inside those helpers.
ggplot2::theme_update(plot.margin = PLOT_MARGIN)

# ==============================================================================
# 1. Load Data
# ==============================================================================

iv_data <- load_data(IV_PATH)
sv_data <- load_data(SV_PATH)

# ==============================================================================
# 2. IAA + RFH
# ==============================================================================

plot_distributions(
  iv_data, sv_data, items = paste0("I", 1:3),
  type = "iaa_rfh",
  output_path = file.path(OUTPUT_DIR, "IAA_RFH_I_1-3.png"),
  y_max = Y_MAX, ncol = 3, labels = LABELS[1:3],
  base_size = 7, width = FIG_WIDTH, height = 1.0
)

# ==============================================================================
# 3. IAA + IVD
# ==============================================================================

plot_distributions(
  iv_data, items = paste0("I", 1:3),
  type = "iaa_ivd",
  output_path = file.path(OUTPUT_DIR, "IAA_IVD_I_1-3.png"),
  y_max = Y_MAX, ncol = 3,
  labels = LABELS[1:3], base_size = 7, width = FIG_WIDTH, height = 1.0
)

# IVD summary (1-6 only)
p <- plot_ivd_summary(
  iv_data, items = ITEMS_6,
  prefix = "I", labels = LABELS_6, ivd_method = "DUW",
  sort_by = IVD_SORT_BY, sort_order = "desc"
) +
  ggplot2::labs(title = NULL) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 7.5),
    axis.title = ggplot2::element_text(size = 8.5),
    plot.margin = PLOT_MARGIN
  )

save_plot(p, "IVD_Summary_I", dir = OUTPUT_DIR, size = c(FIG_WIDTH, FIG_HEIGHT - 0.3))

# ==============================================================================
# 4. Descriptive Statistics
# ==============================================================================

df_S <- calc_descriptives(iv_data, sv_data, items = ITEMS_S)
df_I <- calc_descriptives(iv_data, sv_data, items = ITEMS_I)
df_S_6 <- calc_descriptives(iv_data, sv_data, items = paste0("S", 1:6))
df_I_6 <- calc_descriptives(iv_data, sv_data, items = ITEMS_6)

# Extract IV and SV data (new bundle format)
theta_S <- df_S$theta
theta_I <- df_I$theta
haus_S <- df_S$hausdorff
haus_I <- df_I$hausdorff
sv_S <- df_S$sv
sv_I <- df_I$sv

theta_S_6 <- df_S_6$theta
theta_I_6 <- df_I_6$theta
sv_S_6 <- df_S_6$sv
sv_I_6 <- df_I_6$sv

# Calculate x-axis range (based on 1-6)
all_vals_6 <- c(sv_S_6, sv_I_6, theta_S_6$L, theta_S_6$U, theta_I_6$L, theta_I_6$U)
MEAN_X_LIM <- range(all_vals_6, na.rm = TRUE) + c(-1, 1) * diff(range(all_vals_6)) * 0.05

# Interval means plot (1-6, hurwicz sorting explicit)
p <- plot_means(
  df_I_6, LABELS_6,
  show = c("theta", "hausdorff", "sv"),
  sort_by = "hurwicz", sort_data = "theta", sort_order = "desc",
  gamma = GAMMA,
  x_lim = MEAN_X_LIM, scale = 0.7
) +
  ggplot2::labs(title = NULL) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 7.5),
    axis.title = ggplot2::element_text(size = 8.5),
    plot.margin = PLOT_MARGIN
  )

save_plot(p, "Fig_I_Means", dir = OUTPUT_DIR, size = c(FIG_WIDTH, FIG_HEIGHT - 0.2))

# ==============================================================================
# 5. Validity and Reliability
# ==============================================================================

cor_theta_S <- calc_cor(theta_S, sv_S, method = "theta")
cor_theta_I <- calc_cor(theta_I, sv_I, method = "theta")
cor_haus_S <- calc_cor(haus_S, sv_S, method = "hausdorff")
cor_haus_I <- calc_cor(haus_I, sv_I, method = "hausdorff")

# Reliability - one line for all combinations
alpha <- calc_reliability(
  iv_data, sv_data,
  items = list(S = ITEMS_S, I = ITEMS_I),
  method = "theta"
)

# Correlation plot
p <- plot_correlation(
  x = list(theta = theta_I, hausdorff = haus_I),
  y = sv_I,
  scale = 0.7,
  legend = FALSE
) +
  ggplot2::labs(title = NULL) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 7.5),
    axis.title = ggplot2::element_text(size = 8.5),
    plot.margin = PLOT_MARGIN
  )

save_plot(p, "Fig_I_Corr", dir = OUTPUT_DIR, size = c(FIG_WIDTH+0.4, 2.0))

# ==============================================================================
# 6. Gap Analysis
# ==============================================================================

gap_sv <- calc_gap(sv_I, sv_S, LABELS)
gap_iv <- calc_gap(theta_I, theta_S, LABELS)

p <- plot_gap(
  satisfaction = theta_S_6,
  importance = theta_I_6,
  labels = LABELS_6,
  sv_satisfaction = sv_S_6,
  sv_importance = sv_I_6,
  show_sv_points = TRUE,
  sort_by = GAP_SORT_BY,
  x_lim = MEAN_X_LIM, scale = 0.7
) +
  ggplot2::labs(title = NULL, y = NULL) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 7.5),
    axis.title = ggplot2::element_text(size = 8.5),
    plot.margin = PLOT_MARGIN
  )

save_plot(p, "Gap_Comparison", dir = OUTPUT_DIR, size = c(FIG_WIDTH, FIG_HEIGHT - 0.2))

# ==============================================================================
# 7. IPA and CSI
# ==============================================================================

# IPA limits based on IV data range (same for both plots)
ipa_x_lim <- range(c(theta_S$L, theta_S$U), na.rm = TRUE) + c(-0.5, 0.5) * 0.2
ipa_y_lim <- range(c(theta_I$L, theta_I$U), na.rm = TRUE) + c(-0.5, 0.5) * 0.2

IPA_SIZE <- c(5.5, 3.4)

p <- plot_ipa(sv_I, sv_S, x_lim = ipa_x_lim, y_lim = ipa_y_lim) +
  ggplot2::labs(title = NULL) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 11.5),
    axis.title = ggplot2::element_text(size = 13.5),
    plot.margin = PLOT_MARGIN
  )

save_plot(p, "IPA_SV", dir = OUTPUT_DIR, size = IPA_SIZE)

p <- plot_ipa(
  theta_I, theta_S, method = "theta",
  x_lim = ipa_x_lim, y_lim = ipa_y_lim
) +
  ggplot2::labs(title = NULL) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 11.5),
    axis.title = ggplot2::element_text(size = 13.5),
    plot.margin = PLOT_MARGIN
  )

save_plot(p, "IPA_IV", dir = OUTPUT_DIR, size = IPA_SIZE)

csi_sv <- calc_csi(sv_I, sv_S, LABELS)
csi_iv <- calc_csi(theta_I, theta_S, LABELS)

# ==============================================================================
# 8. Export to Excel
# ==============================================================================

save_results(
  Descriptive_S = df_S,
  Descriptive_I = df_I,
  Gap_SV = gap_sv,
  Gap_IV = gap_iv,
  CSI = data.frame(
    Method = c("SV", "IV_theta"),
    CSI_L = c(csi_sv$CSI, csi_iv$CSI_L),
    CSI_U = c(csi_sv$CSI, csi_iv$CSI_U)
  ),
  Correlation = data.frame(
    Items = c("S", "I"),
    Cor_Theta = c(cor_theta_S, cor_theta_I),
    Cor_Hausdorff = c(cor_haus_S, cor_haus_I)
  ),
  Reliability = alpha,
  file = "results",
  dir = OUTPUT_DIR
)