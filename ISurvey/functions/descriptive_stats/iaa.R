# descriptive_stats/iaa.R - Interval Agreement Approach
#
# Reference: Wagner et al., IEEE Trans. Fuzzy Syst., vol. 23, no. 2, pp. 248-269, 2015.

IAA <- function(data, domain = c(1, 5), step = 0.01) {
  x_values <- seq(from = domain[1], to = domain[2], by = step)
  y_values <- sapply(x_values, function(x_val) {
    sum((data$Min <= x_val) & (data$Max >= x_val)) / nrow(data)
  })
  data.frame(x = x_values, y = y_values)
}
