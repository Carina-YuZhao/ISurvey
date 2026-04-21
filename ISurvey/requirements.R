# ==============================================================================
# requirements.R
# Install/check required R packages for ISurvey 
# ==============================================================================

required_pkgs <- c(
  "readr","dplyr","tidyr","stringr","purrr","ggplot2","scales","openxlsx"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, dependencies = TRUE)
} else {
  message("All required packages are already installed.")
}
