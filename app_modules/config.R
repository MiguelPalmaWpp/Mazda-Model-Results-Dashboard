options(shiny.maxRequestSize = 300 * 1024^2)

DEFAULT_CUTOFF_DATE <- as.Date("2026-01-31")
DEFAULT_REVENUE_PER_UNIT <- 30090
DEFAULT_ROI_FROM <- as.Date("2026-02-01")
DEFAULT_ROI_TO <- as.Date("2026-12-31")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
