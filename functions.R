library(dplyr)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(readr)
library(tidyr)
library(gridExtra)

# ═══════════════════════════════════════════════════════════════════════════
# CONSTANTS
# ═══════════════════════════════════════════════════════════════════════════

# Column widths for all ROI-style sheets (12 columns)
# 1-Variable 2-Units 3-% Contribution 4-Model Contribution
# 5-Expected Contribution 6-Spend(F) 7-Revenue(G) 8-ROI(H)
# 9-Category 10-Sub-Category 11-Funnel 12-Channel
ROI_COL_WIDTHS <- c(60, 12, 16, 18, 20, 14, 14, 10, 25, 25, 10, 20)

sort_order_map <- c(
  "T1 Paid Media Nameplate"      = 0,  "T1 Paid Media Halo"           = 1,
  "Dealer Direct"                = 2,  "Shift Digital CAP"            = 3,
  "VML CAP"                      = 4,  "Variable Marketing"           = 5,
  "Sinergy"                      = 6,  "Retail Inventory"             = 7,
  "Brand Consideration"          = 8,
  "Earned Media - KBB"           = 9,  "Earned Media - Google Trends" = 10,
  "Earned Media - SOV"           = 11, "Owned Media - MUSA"           = 12,
  "Competitive Spend"            = 13, "Competitive Inventory"        = 14,
  "Retail Production"            = 15, "Tariffs"                      = 16,
  "Base"                         = 99
)

# ═══════════════════════════════════════════════════════════════════════════
# 1. LOAD DATA FROM ZIP
# ═══════════════════════════════════════════════════════════════════════════
load_data_from_zip <- function(zip_file) {
  temp_dir <- file.path(tempdir(), "model_output")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  unzip(zip_file, exdir = temp_dir)
  
  files     <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
  csv_files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  
  find_file <- function(pattern) {
    match <- csv_files[grepl(pattern, basename(csv_files), ignore.case = TRUE)]
    if (length(match) == 0) { cat("WARNING: No file found matching:", pattern, "\n"); return(NA) }
    cat("Found:", basename(match[1]), "\n")
    match[1]
  }
  
  # ── Only these 3 files ──────────────────────────────────────────────────
  data_input_file  <- find_file("data_input")   # Date + KPI + spend columns
  med_contrib_file <- find_file("med_contrib")   # Date + Pred + Contrib_ columns
  pct_contrib_file <- find_file("pct_contrib")   # Full model contribution percentages
  
  if (is.na(data_input_file))  stop("data_input file not found in ZIP")
  if (is.na(med_contrib_file)) stop("med_contrib file not found in ZIP")
  if (is.na(pct_contrib_file)) stop("pct_contrib file not found in ZIP")
  
  # ── Read with Date ───────────────────────────────────────────────────────
  safe_read_with_date <- function(filepath) {
    df       <- read.csv(filepath, check.names = FALSE, stringsAsFactors = FALSE)
    bad_cols <- grepl("^X$|^X\\.\\d+$|^\\.\\.\\.\\d+$|^$", colnames(df))
    df       <- df[, !bad_cols, drop = FALSE]
    date_idx <- which(tolower(colnames(df)) == "date")
    if (length(date_idx) == 0) stop(paste("No Date column found in:", basename(filepath)))
    colnames(df)[date_idx[1]] <- "Date"
    df$Date <- as.Date(df$Date)
    dplyr::as_tibble(df)
  }
  
  # ── Load files ───────────────────────────────────────────────────────────
  cat("\nLoading data_input...\n")
  df_actual <- safe_read_with_date(data_input_file)   # ALL columns preserved
  
  cat("Loading med_contrib...\n")
  df_med <- safe_read_with_date(med_contrib_file)
  
  cat("Loading pct_contrib...\n")
  df_pct_raw <- read.csv(
    pct_contrib_file,
    header           = FALSE,
    col.names        = c("Variable", "Pct"),
    stringsAsFactors = FALSE
  )
  
  if (ncol(df_pct_raw) < 2) stop("pct_contrib must have at least 2 columns (Variable, Pct)")
  df_pct <- df_pct_raw %>%
    dplyr::as_tibble() %>%
    mutate(Pct = suppressWarnings(as.numeric(Pct))) %>%
    filter(!is.na(Variable), Variable != "")
  
  # ── Preview ──────────────────────────────────────────────────────────────
  cat("\ndata_input columns:\n");              print(colnames(df_actual))
  cat("\nmed_contrib columns (first 10):\n"); print(colnames(df_med)[1:min(10, ncol(df_med))])
  cat("\npct_contrib preview:\n");             print(head(df_pct, 5))
  
  # ── Auto-detect KPI column (no readline) ────────────────────────────────
  kpi_candidates <- setdiff(colnames(df_actual), "Date")
  kpi_cols       <- kpi_candidates[grepl("KPI", kpi_candidates, ignore.case = TRUE)]
  
  if (length(kpi_cols) >= 1) {
    kpi_col <- kpi_cols[1]
    cat("\nKPI column auto-detected:", kpi_col, "\n")
  } else if (length(kpi_candidates) == 1) {
    kpi_col <- kpi_candidates[1]
    cat("\nSingle non-Date column used as KPI:", kpi_col, "\n")
  } else {
    stop(paste(
      "Could not auto-detect KPI column. Rename the target column to include 'KPI'.",
      "\nAvailable columns:", paste(kpi_candidates, collapse = ", ")
    ))
  }
  
  # Rename KPI → Actual; all other non-Date columns stay as spend columns
  df_actual <- df_actual %>% rename(Actual = !!sym(kpi_col))
  
  spend_cols_found <- setdiff(colnames(df_actual), c("Date", "Actual"))
  cat("Spend columns found in data_input:", length(spend_cols_found), "\n")
  if (length(spend_cols_found) > 0)
    cat(paste(" ", spend_cols_found, collapse = "\n"), "\n")
  
  # ── Pred column ──────────────────────────────────────────────────────────
  pred_col <- if ("Pred" %in% colnames(df_med)) "Pred" else
    colnames(df_med)[grepl("^pred$", colnames(df_med), ignore.case = TRUE)][1]
  cat("Prediction column:", pred_col, "\n")
  
  # ── Merge Actual + Pred (for metrics / plots only) ───────────────────────
  df <- inner_join(
    df_actual %>% select(Date, Actual),
    df_med    %>% select(Date, Pred = !!sym(pred_col)),
    by = "Date"
  ) %>% arrange(Date)
  
  cat(nrow(df), "records loaded\n")
  cat("Date range:", as.character(min(df$Date)), "to", as.character(max(df$Date)), "\n")
  
  list(
    df       = df,         # Date + Actual + Pred        → metrics / plots
    df_med   = df_med,     # full med_contrib             → contributions
    df_pct   = df_pct,     # full model contribution %    → Full Period sheet
    df_input = df_actual   # Date + Actual + spend cols   → ROI spend
  )
}

# ═══════════════════════════════════════════════════════════════════════════
# 2. METRIC FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════
prepare_metric_vectors <- function(df, actual_col = "Actual", pred_col = "Pred") {
  if (!actual_col %in% colnames(df)) {
    stop("Metrics require an Actual column.")
  }
  if (!pred_col %in% colnames(df)) {
    stop("Metrics require a prediction column: ", pred_col)
  }

  metric_df <- df %>%
    transmute(
      Actual = suppressWarnings(as.numeric(.data[[actual_col]])),
      Pred = suppressWarnings(as.numeric(.data[[pred_col]]))
    ) %>%
    filter(is.finite(Actual), is.finite(Pred))

  list(actual = metric_df$Actual, pred = metric_df$Pred)
}

calc_mae     <- function(a, p) { if (length(a) == 0) NA_real_ else mean(abs(a - p), na.rm = TRUE) }
calc_rmse    <- function(a, p) { if (length(a) == 0) NA_real_ else sqrt(mean((a - p)^2, na.rm = TRUE)) }
calc_mape    <- function(a, p) { m <- a != 0; if (!any(m)) NA_real_ else mean(abs((a[m]-p[m])/a[m]))*100 }
calc_smape   <- function(a, p) { d <- (abs(a)+abs(p))/2; m <- d != 0; if (!any(m)) NA_real_ else mean(abs(a[m]-p[m])/d[m])*100 }
calc_r2      <- function(a, p) { if (length(a) == 0) return(NA_real_); ss_r <- sum((a-p)^2); ss_t <- sum((a-mean(a))^2); if (ss_t==0) NA_real_ else 1-(ss_r/ss_t) }
calc_pearson <- function(a, p) { if (length(a) < 2) NA_real_ else cor(a, p, use = "complete.obs") }
calc_mase    <- function(a, p) { if (length(a) < 2) return(NA_real_); mae_m <- mean(abs(a-p)); mae_n <- mean(abs(diff(a))); if (mae_n==0) NA_real_ else mae_m/mae_n }

calculate_all_metrics <- function(df, actual_col = "Actual", pred_col = "Pred") {
  vectors <- prepare_metric_vectors(df, actual_col = actual_col, pred_col = pred_col)
  actual <- vectors$actual
  pred <- vectors$pred

  list(
    MAE         = calc_mae(actual, pred),
    RMSE        = calc_rmse(actual, pred),
    `MAPE (%)`  = calc_mape(actual, pred),
    `SMAPE (%)` = calc_smape(actual, pred),
    R2          = calc_r2(actual, pred),
    `Pearson R` = calc_pearson(actual, pred),
    MASE        = calc_mase(actual, pred)
  )
}

calculate_metrics_over_time <- function(df, pred_col = "Pred") {
  df %>%
    mutate(Period = format(Date, "%Y-%m")) %>%
    group_by(Period) %>%
    filter(n() >= 2) %>%
    group_modify(~ {
      metrics <- calculate_all_metrics(.x, pred_col = pred_col)
      tibble(
        N_Days = nrow(.x),
        MAE = metrics$MAE,
        RMSE = metrics$RMSE,
        `MAPE (%)` = metrics$`MAPE (%)`,
        `SMAPE (%)` = metrics$`SMAPE (%)`,
        R2 = metrics$R2,
        `Pearson R` = metrics$`Pearson R`,
        MASE = metrics$MASE
      )
    }) %>%
    ungroup()
}

# aggregate_data actualizado para propagar Pred_Gradient si existe
aggregate_data <- function(df, freq, method = "sum", week_start = 7) {
  agg_fn       <- if (method == "sum") sum else mean
  has_gradient <- "Pred_Gradient" %in% colnames(df)
  
  df_grouped <- df %>%
    mutate(Period = if (freq == "week") {
      floor_date(Date, freq, week_start = week_start)
    } else {
      floor_date(Date, freq)
    }) %>%
    group_by(Period)
  
  if (has_gradient) {
    df_agg <- df_grouped %>%
      summarise(
        Actual        = agg_fn(Actual,        na.rm = TRUE),
        Pred          = agg_fn(Pred,          na.rm = TRUE),
        Pred_Gradient = agg_fn(Pred_Gradient, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    df_agg <- df_grouped %>%
      summarise(
        Actual = agg_fn(Actual, na.rm = TRUE),
        Pred   = agg_fn(Pred,   na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  df_agg %>% rename(Date = Period) %>% drop_na()
}

# ═══════════════════════════════════════════════════════════════════════════
# 3. CORRELATION BEFORE / AFTER
# ═══════════════════════════════════════════════════════════════════════════
calculate_correlation_split <- function(df, cutoff_date, output_file) {
  before <- df %>% filter(Date <= cutoff_date)
  after  <- df %>% filter(Date >  cutoff_date)
  
  cor_before <- calc_pearson(before$Actual, before$Pred)
  cor_after  <- calc_pearson(after$Actual,  after$Pred)
  
  cat("Correlation BEFORE", as.character(cutoff_date), ":", round(cor_before, 3), "\n")
  cat("Correlation AFTER",  as.character(cutoff_date), ":", round(cor_after,  3), "\n")
  
  result <- data.frame(
    Period      = c("Before", "After"),
    Date_From   = c(as.character(min(before$Date)), as.character(min(after$Date))),
    Date_To     = c(as.character(max(before$Date)), as.character(max(after$Date))),
    N_Rows      = c(nrow(before), nrow(after)),
    Correlation = c(round(cor_before, 3), round(cor_after, 3))
  )
  
  write_csv(result, output_file)
  cat("Correlation saved to:", output_file, "\n")
  result
}

# ═══════════════════════════════════════════════════════════════════════════
# 4. PLOTS
# ═══════════════════════════════════════════════════════════════════════════

# Dynamic date breaks based on date range
get_date_breaks <- function(date_vec) {
  n_months <- as.numeric(difftime(max(date_vec), min(date_vec), units = "days")) / 30
  if      (n_months <= 12) "1 month"
  else if (n_months <= 24) "2 months"
  else if (n_months <= 48) "3 months"
  else                     "6 months"
}

plot_actual_vs_pred <- function(df, title, filepath) {
  resid_sd <- sd(df$Actual - df$Pred)
  df_plot  <- df %>% mutate(CI_Upper = Pred + 1.96 * resid_sd,
                            CI_Lower = Pred - 1.96 * resid_sd)
  date_brk <- get_date_breaks(df$Date)
  
  p1 <- ggplot(df_plot, aes(x = Date)) +
    geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), fill = "#FF5722", alpha = 0.12) +
    geom_ribbon(aes(ymin = pmin(Actual, Pred), ymax = pmax(Actual, Pred)),
                fill = "gray", alpha = 0.08) +
    geom_line(aes(y = Actual, color = "Actual"),    linewidth = 1) +
    geom_line(aes(y = Pred,   color = "Predicted"), linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("Actual" = "#2196F3", "Predicted" = "#FF5722")) +
    scale_x_date(date_labels = "%b %Y", date_breaks = date_brk) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
    labs(title = "Time Series", x = "Date", y = "Value", color = "")
  
  min_val <- min(df$Actual, df$Pred) * 0.95
  max_val <- max(df$Actual, df$Pred) * 1.05
  
  p2 <- ggplot(df, aes(x = Actual, y = Pred)) +
    geom_point(color = "#9C27B0", alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
    xlim(min_val, max_val) + ylim(min_val, max_val) +
    theme_minimal() +
    labs(title = "Scatter Plot (Actual vs Predicted)", x = "Actual", y = "Predicted")
  
  combined <- arrangeGrob(p1, p2, nrow = 2, top = paste("Actual vs Predicted -", title))
  ggsave(filepath, plot = combined, width = 14, height = 10, dpi = 150)
  cat("Plot saved:", filepath, "\n")
}

plot_residuals <- function(df, title, filepath) {
  df_res   <- df %>% mutate(Residual = Actual - Pred)
  mean_res <- mean(df_res$Residual)
  date_brk <- get_date_breaks(df$Date)
  
  p1 <- ggplot(df_res, aes(x = Date, y = Residual, fill = Residual >= 0)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    scale_fill_manual(values = c("TRUE" = "#2196F3", "FALSE" = "#FF5722"), guide = "none") +
    geom_hline(yintercept = 0,        color = "black",  linewidth = 1) +
    geom_hline(yintercept = mean_res, color = "orange", linewidth = 1, linetype = "dashed") +
    scale_x_date(date_labels = "%b %Y", date_breaks = date_brk) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Residuals Over Time", x = "Date", y = "Residual")
  
  p2 <- ggplot(df_res, aes(x = Residual)) +
    geom_histogram(bins = 30, fill = "#9C27B0", color = "white", alpha = 0.7) +
    geom_vline(xintercept = 0,        color = "red",    linewidth = 1, linetype = "dashed") +
    geom_vline(xintercept = mean_res, color = "orange", linewidth = 1, linetype = "dashed") +
    theme_minimal() +
    labs(title = "Residuals Distribution", x = "Residual Value", y = "Frequency")
  
  combined <- arrangeGrob(p1, p2, nrow = 2, top = paste("Residuals Analysis -", title))
  ggsave(filepath, plot = combined, width = 14, height = 10, dpi = 150)
  cat("Residuals plot saved:", filepath, "\n")
}

# ═══════════════════════════════════════════════════════════════════════════
# 5. CHANNEL MAPPING
# ═══════════════════════════════════════════════════════════════════════════
normalize_mapping_key <- function(x) {
  x <- sub("^Contrib_", "", x)
  x <- trimws(tolower(x))
  gsub("[^a-z0-9]+", "_", x)
}

is_spend_column <- function(x) {
  grepl("(^|_)spend($|_)", normalize_mapping_key(x))
}

default_master_mapping_paths <- function() {
  paths <- c(
    if(exists("app_path", mode = "function")) app_path("FY160 B2D Master Mapping File.xlsx") else NA_character_,
    if(exists("app_path", mode = "function")) app_path("inputs", "FY160 B2D Master Mapping File.xlsx") else NA_character_,
    file.path(Sys.getenv("USERPROFILE"), "Downloads", "FY160 B2D Master Mapping File.xlsx")
  )
  unique(paths[!is.na(paths) & nzchar(paths)])
}

mapping_cache <- new.env(parent = emptyenv())

load_master_mapping <- function(mapping_path = NULL) {
  if(!is.null(mapping_cache$mapping)) return(mapping_cache$mapping)
  
  candidate_paths <- if(!is.null(mapping_path)) mapping_path else default_master_mapping_paths()
  mapping_file <- candidate_paths[file.exists(candidate_paths)][1]
  
  if(is.na(mapping_file) || length(mapping_file) == 0) {
    mapping_cache$mapping <- data.frame()
    return(mapping_cache$mapping)
  }
  
  mapping <- openxlsx::read.xlsx(mapping_file, sheet = "Mapping") %>%
    dplyr::as_tibble() %>%
    mutate(
      across(everything(), ~ ifelse(is.na(.x), NA_character_, trimws(as.character(.x)))),
      Variable_Key = normalize_mapping_key(Variable)
    ) %>%
    filter(!is.na(Variable), Variable != "") %>%
    distinct(Variable_Key, .keep_all = TRUE)
  
  mapping_cache$mapping <- mapping
  mapping
}

infer_funnel_from_name <- function(col_name) {
  clean <- normalize_mapping_key(col_name)
  
  if(grepl("lower_upper|upper_lower|upperlower|lowerupper", clean)) return("Lower_Upper")
  if(grepl("(^|_)lower($|_)", clean)) return("Lower")
  if(grepl("(^|_)upper($|_)", clean)) return("Upper")
  ""
}

infer_t1_channel_from_name <- function(col_name) {
  clean <- normalize_mapping_key(col_name)
  
  channel_rules <- c(
    "brand_nonbrand_search|brandnonbrand_search" = "Brand_Search_NonBrand_Search",
    "nonbrand_search" = "NonBrand_Search",
    "brand_search" = "Brand_Search",
    "content_amplification" = "Content_Amplification",
    "national_connected_tv" = "National_Connected_TV",
    "connected_tv|ctv" = "Connected_TV",
    "paid_social" = "Paid_Social",
    "digital_display_retargeting" = "Digital_Display_Retargeting",
    "digital_display" = "Digital_Display",
    "digital_video_digital_audio" = "Digital_Video_Digital_Audio",
    "digital_video|online_video" = "Digital_Video",
    "audio_partnerships_ooh|digital_audio_ooh" = "Digital_Audio_OOH",
    "digital_audio|audio" = "Digital_Audio",
    "demand_gen|demandgen|discovery" = "Demand_Gen",
    "youtube" = "YouTube",
    "pmax|performance_max" = "Pmax",
    "rich_media" = "Rich_Media",
    "brand_partnerships" = "Brand_Partnerships",
    "national_tv" = "National_TV",
    "native" = "Native",
    "cinema" = "Cinema",
    "ooh" = "OOH"
  )
  
  matched <- names(channel_rules)[vapply(names(channel_rules), grepl, logical(1), x = clean)]
  if(length(matched) > 0) return(unname(channel_rules[[matched[1]]]))
  
  halo_match <- regmatches(clean, regexpr("cx30|cx50_hybrid|cx50|cx70|cx90|cx5|mazda3|mx5|brand|other", clean))
  if(length(halo_match) > 0 && nzchar(halo_match)) return(paste0(toupper(halo_match), "_Halo"))
  
  "Paid_Media"
}

mapping_row_to_list <- function(row) {
  list(
    channel = row$Channel,
    category = row$Category,
    sub_category = row$`Sub-Category`,
    funnel = row$Funnel,
    sp_mapping = row$SP_Mapping,
    sp_channel = row$SP_Channel,
    sp_agg = row$SP_Agg,
    mapping_source = "FY160 mapping"
  )
}

apply_channel_mapping_overrides <- function(mapping, col_name) {
  clean <- normalize_mapping_key(col_name)
  
  if(grepl("earned_media", clean) && grepl("(^|_)sov($|_)|share_of_voice", clean)) {
    mapping$channel <- "Earned Media"
    mapping$category <- "Earned Media - SOV"
    mapping$sub_category <- "Earned Media - SOV"
    mapping$funnel <- "Earned Media - SOV"
    mapping$sp_channel <- "Earned Media - SOV"
    mapping$sp_agg <- "Average"
  }
  
  if(grepl("vm_?sinergy|vmsinergy|vm_synergy|vmsynergy", clean)) {
    mapping$channel <- "Sinergy"
    mapping$category <- "Sinergy"
    mapping$sub_category <- "Sinergy"
    mapping$funnel <- "Sinergy"
    mapping$sp_channel <- "Sinergy"
  }
  
  if(grepl("macro_economic_indicators", clean)) {
    mapping$sub_category <- "Macroeconomic"
    if(identical(mapping$funnel, "Base") || is.na(mapping$funnel) || !nzchar(mapping$funnel)) {
      mapping$funnel <- "Macroeconomic"
    }
  }
  
  mapping
}

fallback_channel_mapping <- function(col_name) {
  clean <- normalize_mapping_key(col_name)
  funnel <- infer_funnel_from_name(col_name)
  sp_mapping <- "NonMedia"
  sp_agg <- "Sum"
  
  if(grepl("paid_media_tier_1|tier_1", clean)) {
    channel <- infer_t1_channel_from_name(col_name)
    category <- "T1 Paid Media"
    sub_category <- if(grepl("halo", clean)) "T1 Paid Media Halo" else "T1 Paid Media Nameplate"
    if(identical(funnel, "")) funnel <- if(identical(sub_category, "T1 Paid Media Halo")) "T1 Paid Media Halo" else ""
    sp_mapping <- "Media"
    sp_channel <- paste("T1 Paid Media", channel)
  } else if(grepl("dealer_direct", clean)) {
    channel <- category <- sub_category <- funnel <- sp_channel <- "Dealer Direct"
  } else if(grepl("shift_digital_cap", clean)) {
    category <- sub_category <- funnel <- "Shift Digital CAP"
    channel <- infer_t1_channel_from_name(col_name)
    sp_channel <- paste("Shift Digital CAP", channel)
  } else if(grepl("vml_cap", clean)) {
    category <- sub_category <- funnel <- "VML CAP"
    channel <- infer_t1_channel_from_name(col_name)
    sp_channel <- paste("VML CAP", channel)
  } else if(grepl("vm_?sinergy|vmsinergy|vm_synergy|vmsynergy", clean)) {
    channel <- category <- sub_category <- funnel <- sp_channel <- "Sinergy"
  } else if(grepl("variable_marketing|incentive", clean)) {
    channel <- category <- sub_category <- funnel <- sp_channel <- "Variable Marketing"
  } else if(grepl("competitive.*spend|competitive_spend", clean)) {
    channel <- category <- funnel <- sp_channel <- "Competitive Spend"
    sub_category <- "Competitive Spend"
  } else if(grepl("competitive.*inventory|days_of_supply", clean)) {
    channel <- funnel <- "Competitive Inventory"
    category <- sp_channel <- "Competitive"
    sub_category <- "Competitive Inventory"
    sp_agg <- "Average"
  } else if(grepl("brand_health|brand_consideration|consideration", clean)) {
    channel <- category <- sub_category <- funnel <- sp_channel <- "Brand Consideration"
    sp_agg <- "Average"
  } else if(grepl("earned_media", clean) && grepl("(^|_)sov($|_)|share_of_voice", clean)) {
    channel <- "Earned Media"
    category <- sub_category <- funnel <- sp_channel <- "Earned Media - SOV"
    sp_agg <- "Average"
  } else if(grepl("google_trends", clean)) {
    channel <- "Earned Media"
    category <- sub_category <- "Earned Media - Google Trends"
    funnel <- sp_channel <- "Base"
  } else if(grepl("endemic_kbb|kbb", clean)) {
    channel <- category <- sub_category <- funnel <- sp_channel <- "Earned Media - KBB"
  } else if(grepl("owned_media_musa|musa", clean)) {
    channel <- category <- sub_category <- funnel <- sp_channel <- "Owned Media - MUSA"
  } else if(grepl("retail_inventory", clean)) {
    channel <- category <- sub_category <- funnel <- sp_channel <- "Retail Inventory"
    sp_agg <- "Average"
  } else if(grepl("retail_production|production", clean)) {
    channel <- category <- funnel <- sp_channel <- "Retail Production"
    sub_category <- "Retail Production"
  } else if(grepl("tariff|tarrif|exchange_rate", clean)) {
    channel <- funnel <- sp_channel <- "Tariffs"
    category <- "Base"
    sub_category <- "Tariffs"
  } else if(grepl("tax_credit|ev_credit", clean)) {
    channel <- funnel <- sp_channel <- "EV Tax Credit"
    category <- "Base"
    sub_category <- "EV Tax Credit"
  } else if(grepl("end_of_month|month_end|closingmonth|sales_mo_close", clean)) {
    channel <- sp_channel <- "Base"
    category <- "Base"
    sub_category <- funnel <- "End_of_Month_Flag"
  } else if(grepl("end_of_fiscal|fiscal_year|end_of_fy|end_of_159", clean)) {
    channel <- sp_channel <- "Base"
    category <- "Base"
    sub_category <- funnel <- "End_of_FY_Flag"
  } else {
    channel <- category <- sub_category <- funnel <- sp_channel <- "Base"
  }
  
  apply_channel_mapping_overrides(list(
    channel = channel,
    category = category,
    sub_category = sub_category,
    funnel = funnel,
    sp_mapping = sp_mapping,
    sp_channel = sp_channel,
    sp_agg = sp_agg,
    mapping_source = "fallback rules"
  ), col_name)
}

get_channel_mapping <- function(col_name) {
  mapping <- load_master_mapping()
  lookup_key <- normalize_mapping_key(col_name)
  
  if(nrow(mapping) > 0 && lookup_key %in% mapping$Variable_Key) {
    return(apply_channel_mapping_overrides(mapping_row_to_list(mapping[match(lookup_key, mapping$Variable_Key), ]), col_name))
  }
  
  fallback_channel_mapping(col_name)
}

match_spend_total <- function(var_clean, spend_lookup, spend_lookup_normalized) {
  if(length(spend_lookup) == 0) return(NA_real_)
  
  exact_spend <- as.numeric(spend_lookup[var_clean])
  if(!is.na(exact_spend)) return(exact_spend)
  
  normalized_key <- normalize_mapping_key(var_clean)
  as.numeric(spend_lookup_normalized[normalized_key])
}

read_table_sheet <- function(filepath, sheet = 1) {
  ext <- tolower(tools::file_ext(filepath))
  
  if(ext %in% c("xlsx", "xlsm", "xls")) {
    return(openxlsx::read.xlsx(filepath, sheet = sheet, detectDates = TRUE, check.names = FALSE) %>%
             dplyr::as_tibble())
  }
  
  if(ext == "csv") {
    return(readr::read_csv(filepath, show_col_types = FALSE, progress = FALSE) %>%
             dplyr::as_tibble())
  }
  
  stop("Unsupported file type: ", ext)
}

load_cftp <- function(filepath, sheet = 1, nameplate = NULL) {
  df_cftp <- read_table_sheet(filepath, sheet = sheet)
  colnames(df_cftp) <- trimws(colnames(df_cftp))
  
  required_cols <- c("Period", "Date", "Final_Nameplate", "AVG_CFTP")
  missing_cols <- setdiff(required_cols, colnames(df_cftp))
  if(length(missing_cols) > 0) {
    stop("CFTP file is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  df_cftp <- df_cftp %>%
    mutate(
      Date = parse_uploaded_date(Date, "Consumer Facing Transaction Price AVG"),
      Month = as.Date(floor_date(Date, "month")),
      Final_Nameplate = trimws(as.character(Final_Nameplate)),
      AVG_CFTP = as.numeric(gsub(",", "", as.character(AVG_CFTP)))
    ) %>%
    filter(!is.na(Month), !is.na(Final_Nameplate), Final_Nameplate != "", !is.na(AVG_CFTP))
  
  if(!is.null(nameplate) && nzchar(nameplate)) {
    df_cftp <- df_cftp %>% filter(Final_Nameplate == nameplate)
  }
  
  df_cftp <- df_cftp %>%
    group_by(Month, Final_Nameplate) %>%
    summarise(AVG_CFTP = mean(AVG_CFTP, na.rm = TRUE), .groups = "drop") %>%
    arrange(Final_Nameplate, Month)
  
  if(!is.null(nameplate) && nzchar(nameplate) && nrow(df_cftp) == 0) {
    stop("No valid CFTP rows found for Final_Nameplate: ", nameplate)
  }
  
  sheet_name <- if(tolower(tools::file_ext(filepath)) %in% c("xlsx", "xlsm", "xls")) {
    if(is.numeric(sheet)) openxlsx::getSheetNames(filepath)[sheet] else sheet
  } else {
    "csv"
  }
  
  cat("CFTP file loaded - sheet:", sheet_name, "- rows:", nrow(df_cftp), "\n")
  if(!is.null(nameplate) && nzchar(nameplate)) {
    cat("CFTP nameplate selected:", nameplate, "\n")
  }
  if(nrow(df_cftp) > 0) {
    cat("CFTP months:", paste(format(sort(unique(df_cftp$Month)), "%Y-%m"), collapse = ", "), "\n")
  }
  
  df_cftp
}

cftp_nameplate_choices <- function(filepath, sheet = 1) {
  df_cftp <- load_cftp(filepath, sheet = sheet, nameplate = NULL)
  sort(unique(df_cftp$Final_Nameplate))
}

missing_cftp_months <- function(df_med_input, cftp_data) {
  if(is.null(cftp_data) || nrow(cftp_data) == 0 || nrow(df_med_input) == 0) {
    return(character(0))
  }
  
  med_months <- sort(unique(as.Date(floor_date(df_med_input$Date, "month"))))
  cftp_months <- sort(unique(cftp_data$Month))
  format(med_months[!med_months %in% cftp_months], "%Y-%m")
}

calculate_cftp_revenue <- function(df_med_input, contrib_cols, cftp_data) {
  if(is.null(cftp_data) || nrow(cftp_data) == 0) {
    return(list(
      revenue_lookup = setNames(numeric(0), character(0)),
      covered_lookup = setNames(logical(0), character(0)),
      missing_months = character(0)
    ))
  }
  
  missing_months <- missing_cftp_months(df_med_input, cftp_data)
  
  if(length(missing_months) > 0) {
    cat("  WARNING - CFTP missing for months:",
        paste(missing_months, collapse = ", "), "\n")
  }
  
  revenue_by_var <- df_med_input %>%
    mutate(Month = as.Date(floor_date(Date, "month"))) %>%
    select(Date, Month, all_of(contrib_cols)) %>%
    pivot_longer(all_of(contrib_cols), names_to = "Variable", values_to = "Daily_Units") %>%
    left_join(cftp_data %>% select(Month, AVG_CFTP), by = "Month") %>%
    group_by(Variable) %>%
    summarise(
      Revenue = if(any(!is.na(AVG_CFTP) & Daily_Units > 0)) {
        sum(ifelse(Daily_Units > 0, Daily_Units * AVG_CFTP, NA_real_), na.rm = TRUE)
      } else {
        NA_real_
      },
      Has_CFTP_Revenue = any(!is.na(AVG_CFTP) & Daily_Units > 0),
      .groups = "drop"
    )
  
  list(
    revenue_lookup = setNames(revenue_by_var$Revenue, revenue_by_var$Variable),
    covered_lookup = setNames(revenue_by_var$Has_CFTP_Revenue, revenue_by_var$Variable),
    missing_months = missing_months
  )
}

# ═══════════════════════════════════════════════════════════════════════════
# 6. EXCEL HELPERS
# ═══════════════════════════════════════════════════════════════════════════
apply_header_style <- function(wb, sheet, row, cols, fill_hex = "1F4E79") {
  addStyle(wb, sheet,
           style = createStyle(fgFill = paste0("#", fill_hex), fontColour = "#FFFFFF",
                               textDecoration = "bold", halign = "center", valign = "center"),
           rows = row, cols = cols, gridExpand = TRUE)
}

alt_style   <- createStyle(fgFill = "#D6E4F0", halign = "center")
white_style <- createStyle(fgFill = "#FFFFFF", halign = "center")

stripe_rows <- function(wb, sheet, n_rows, start_row, n_cols) {
  if(n_rows <= 0) {
    return(invisible(NULL))
  }
  
  rows <- start_row + seq_len(n_rows)
  even_rows <- rows[rows %% 2 == 0]
  odd_rows <- rows[rows %% 2 != 0]
  
  if(length(even_rows) > 0) {
    addStyle(wb, sheet, alt_style, rows = even_rows, cols = 1:n_cols, gridExpand = TRUE)
  }
  if(length(odd_rows) > 0) {
    addStyle(wb, sheet, white_style, rows = odd_rows, cols = 1:n_cols, gridExpand = TRUE)
  }
  
  invisible(NULL)
}

# ── Summary sheet ─────────────────────────────────────────────────────────────
add_summary_sheet <- function(wb,
                              m_daily,  m_weekly,  m_monthly,
                              m_daily_grad  = NULL,
                              m_weekly_grad = NULL,
                              m_monthly_grad = NULL) {
  
  has_gradient <- !is.null(m_daily_grad)
  n_cols       <- if (has_gradient) 7 else 4
  
  addWorksheet(wb, "Summary")
  
  # ── Título ──────────────────────────────────────────────────────────────────
  writeData(wb, "Summary", "Metrics Summary", startRow = 1, startCol = 1)
  addStyle(wb, "Summary",
           createStyle(fontSize = 14, textDecoration = "bold",
                       fontColour = "#1F4E79", halign = "center"),
           rows = 1, cols = 1:n_cols, gridExpand = TRUE)
  mergeCells(wb, "Summary", cols = 1:n_cols, rows = 1)
  
  if (has_gradient) {
    # ── Fila de grupo (row 2) ────────────────────────────────────────────────
    writeData(wb, "Summary", "Original",          startRow = 2, startCol = 2)
    writeData(wb, "Summary", "With Gradient",     startRow = 2, startCol = 5)
    
    addStyle(wb, "Summary",
             createStyle(fgFill = "#2E75B6", fontColour = "#FFFFFF",
                         textDecoration = "bold", halign = "center"),
             rows = 2, cols = 2:4, gridExpand = TRUE)
    addStyle(wb, "Summary",
             createStyle(fgFill = "#375623", fontColour = "#FFFFFF",
                         textDecoration = "bold", halign = "center"),
             rows = 2, cols = 5:7, gridExpand = TRUE)
    mergeCells(wb, "Summary", cols = 2:4, rows = 2)
    mergeCells(wb, "Summary", cols = 5:7, rows = 2)
    
    # ── Headers (row 3) ──────────────────────────────────────────────────────
    headers <- data.frame(
      Metric            = "Metric",
      Daily             = "Daily",
      Weekly            = "Weekly",
      Monthly           = "Monthly",
      Daily_Gradient    = "Daily (Gradient)",
      Weekly_Gradient   = "Weekly (Gradient)",
      Monthly_Gradient  = "Monthly (Gradient)"
    )
    writeData(wb, "Summary", headers, startRow = 3, colNames = FALSE)
    apply_header_style(wb, "Summary", 3, 1:n_cols)
    
    # ── Datos ────────────────────────────────────────────────────────────────
    for (i in seq_along(m_daily)) {
      row <- i + 3
      writeData(wb, "Summary", names(m_daily)[i],           startRow = row, startCol = 1)
      writeData(wb, "Summary", round(m_daily[[i]],        3), startRow = row, startCol = 2)
      writeData(wb, "Summary", round(m_weekly[[i]],       3), startRow = row, startCol = 3)
      writeData(wb, "Summary", round(m_monthly[[i]],      3), startRow = row, startCol = 4)
      writeData(wb, "Summary", round(m_daily_grad[[i]],   3), startRow = row, startCol = 5)
      writeData(wb, "Summary", round(m_weekly_grad[[i]],  3), startRow = row, startCol = 6)
      writeData(wb, "Summary", round(m_monthly_grad[[i]], 3), startRow = row, startCol = 7)
      addStyle(wb, "Summary",
               if (row %% 2 == 0) alt_style else white_style,
               rows = row, cols = 1:n_cols, gridExpand = TRUE)
    }
    
    setColWidths(wb, "Summary", cols = 1:n_cols,
                 widths = c(18, 14, 14, 14, 18, 18, 18))
    
  } else {
    # ── Sin gradiente (comportamiento original) ──────────────────────────────
    headers <- data.frame(Metric = "Metric", Daily = "Daily",
                          Weekly = "Weekly", Monthly = "Monthly")
    writeData(wb, "Summary", headers, startRow = 3, colNames = FALSE)
    apply_header_style(wb, "Summary", 3, 1:4)
    
    for (i in seq_along(m_daily)) {
      row <- i + 3
      writeData(wb, "Summary", names(m_daily)[i],        startRow = row, startCol = 1)
      writeData(wb, "Summary", round(m_daily[[i]],   3), startRow = row, startCol = 2)
      writeData(wb, "Summary", round(m_weekly[[i]],  3), startRow = row, startCol = 3)
      writeData(wb, "Summary", round(m_monthly[[i]], 3), startRow = row, startCol = 4)
      addStyle(wb, "Summary",
               if (row %% 2 == 0) alt_style else white_style,
               rows = row, cols = 1:4, gridExpand = TRUE)
    }
    
    setColWidths(wb, "Summary", cols = 1:4, widths = c(18, 16, 16, 16))
  }
  
  cat("Summary sheet added\n")
}

# ── Metrics Over Time sheet ───────────────────────────────────────────────────
add_metrics_over_time_sheet <- function(wb, df_mot, df_mot_grad = NULL) {
  
  has_gradient <- !is.null(df_mot_grad)
  
  addWorksheet(wb, "Metrics Over Time")
  
  metric_cols <- c("MAE", "RMSE", "MAPE (%)", "SMAPE (%)", "R2", "Pearson R", "MASE")
  base_cols   <- c("Period", "N_Days")
  
  if (has_gradient) {
    # ── Unir original + gradient ─────────────────────────────────────────────
    df_combined <- df_mot %>%
      select(all_of(c(base_cols, metric_cols))) %>%
      left_join(
        df_mot_grad %>%
          select(Period, all_of(metric_cols)) %>%
          rename_with(~ paste0(.x, " (Gradient)"), all_of(metric_cols)),
        by = "Period"
      )
    
    all_cols <- colnames(df_combined)
    n_cols   <- length(all_cols)
    
    writeData(wb, "Metrics Over Time", df_combined, startRow = 2)
    apply_header_style(wb, "Metrics Over Time", 2, 1:n_cols)
    stripe_rows(wb, "Metrics Over Time", nrow(df_combined), 2, n_cols)
    
    # ── Fila de grupo (row 1) ────────────────────────────────────────────────
    writeData(wb, "Metrics Over Time", "Original",      startRow = 1, startCol = 3)
    writeData(wb, "Metrics Over Time", "With Gradient", startRow = 1, startCol = 3 + length(metric_cols))
    
    addStyle(wb, "Metrics Over Time",
             createStyle(fgFill = "#2E75B6", fontColour = "#FFFFFF",
                         textDecoration = "bold", halign = "center"),
             rows = 1, cols = 3:(2 + length(metric_cols)), gridExpand = TRUE)
    addStyle(wb, "Metrics Over Time",
             createStyle(fgFill = "#375623", fontColour = "#FFFFFF",
                         textDecoration = "bold", halign = "center"),
             rows = 1, cols = (3 + length(metric_cols)):n_cols, gridExpand = TRUE)
    mergeCells(wb, "Metrics Over Time",
               cols = 3:(2 + length(metric_cols)), rows = 1)
    mergeCells(wb, "Metrics Over Time",
               cols = (3 + length(metric_cols)):n_cols, rows = 1)
    
    setColWidths(wb, "Metrics Over Time", cols = 1:n_cols,
                 widths = c(12, 8, rep(12, length(metric_cols)), rep(16, length(metric_cols))))
    
  } else {
    # ── Sin gradiente (comportamiento original) ──────────────────────────────
    cols   <- c(base_cols, metric_cols)
    n_cols <- length(cols)
    
    writeData(wb, "Metrics Over Time", df_mot %>% select(all_of(cols)), startRow = 1)
    apply_header_style(wb, "Metrics Over Time", 1, 1:n_cols)
    stripe_rows(wb, "Metrics Over Time", nrow(df_mot), 1, n_cols)
    setColWidths(wb, "Metrics Over Time", cols = 1:n_cols,
                 widths = c(12, 8, 10, 10, 12, 12, 10, 12, 10))
  }
  
  cat("Metrics Over Time sheet added\n")
}

# ── Daily / Weekly / Monthly sheets ────────────────────────────────────────
write_granularity_sheet <- function(wb, sheet_name, metrics, df_data, previous_series = NULL) {
  addWorksheet(wb, sheet_name)
  
  has_gradient <- "Pred_Gradient" %in% colnames(df_data)
  has_previous <- !is.null(previous_series) && nrow(previous_series) > 0 && "Previous_Pred" %in% colnames(previous_series)
  
  append_previous_prediction <- function(df_export) {
    if (!has_previous) {
      return(df_export)
    }
    
    previous_export <- previous_series %>%
      transmute(
        Date = as.Date(Date),
        `Previous Model Predicted` = round(as.numeric(Previous_Pred), 2)
      )
    
    df_export %>%
      mutate(Date = as.Date(Date)) %>%
      left_join(previous_export, by = "Date") %>%
      mutate(
        `Previous Model Error` = round(`Previous Model Predicted` - Actual, 2),
        `Previous Model Abs Error` = round(abs(`Previous Model Predicted` - Actual), 2),
        `Previous Model Pct Error` = round(ifelse(Actual != 0, `Previous Model Error` / Actual * 100, NA), 2)
      )
  }
  
  writeData(wb, sheet_name, paste("Metrics Report -", sheet_name),
            startRow = 1, startCol = 1)
  addStyle(wb, sheet_name,
           createStyle(fontSize = 13, textDecoration = "bold",
                       fontColour = "#1F4E79", halign = "center"),
           rows = 1, cols = 1:6, gridExpand = TRUE)
  mergeCells(wb, sheet_name, cols = 1:6, rows = 1)
  
  writeData(wb, sheet_name, data.frame(A = "Metric", B = "Value"),
            startRow = 3, colNames = FALSE)
  apply_header_style(wb, sheet_name, 3, 1:2)
  
  for (i in seq_along(metrics)) {
    row <- i + 3
    writeData(wb, sheet_name, names(metrics)[i],      startRow = row, startCol = 1)
    writeData(wb, sheet_name, round(metrics[[i]], 3), startRow = row, startCol = 2)
    addStyle(wb, sheet_name,
             if (row %% 2 == 0) alt_style else white_style,
             rows = row, cols = 1:2, gridExpand = TRUE)
  }
  
  data_row <- 3 + length(metrics) + 2
  
  # ── Construir tabla según si hay gradiente ──────────────────────────────
  if (has_gradient) {
    df_export <- df_data %>%
      mutate(
        Date            = as.Date(Date),
        Actual          = round(Actual,        2),
        Predicted       = round(Pred,          2),
        Pred_Gradient   = round(Pred_Gradient, 2),
        Error           = round(Pred          - Actual, 2),
        Error_Gradient  = round(Pred_Gradient - Actual, 2),
        Abs_Error       = round(abs(Pred          - Actual), 2),
        Abs_Error_Grad  = round(abs(Pred_Gradient - Actual), 2),
        Pct_Error       = round(ifelse(Actual != 0, (Pred          - Actual) / Actual * 100, NA), 2),
        Pct_Error_Grad  = round(ifelse(Actual != 0, (Pred_Gradient - Actual) / Actual * 100, NA), 2)
      ) %>%
      select(Date, Actual,
             Predicted, Error, Abs_Error, Pct_Error,
             Pred_Gradient, Error_Gradient, Abs_Error_Grad, Pct_Error_Grad) %>%
      append_previous_prediction() %>%
      mutate(Date = format(Date, "%d/%m/%Y"))
    
    n_cols      <- ncol(df_export)
    col_widths  <- c(14, 14, 14, 12, 12, 12, 16, 16, 16, 16, rep(20, max(0, n_cols - 10)))
    
    writeData(wb, sheet_name, df_export, startRow = data_row)
    apply_header_style(wb, sheet_name, data_row, 1:n_cols)
    
    # Header especial: grupo Original vs Gradient
    # Fila de grupo encima del header (data_row - 1)
    group_row <- data_row - 1
    writeData(wb, sheet_name, "Original Pred",   startRow = group_row, startCol = 3)
    writeData(wb, sheet_name, "Pred + Gradient", startRow = group_row, startCol = 7)
    
    addStyle(wb, sheet_name,
             createStyle(fgFill = "#2E75B6", fontColour = "#FFFFFF",
                         textDecoration = "bold", halign = "center"),
             rows = group_row, cols = 3:6, gridExpand = TRUE)
    addStyle(wb, sheet_name,
             createStyle(fgFill = "#375623", fontColour = "#FFFFFF",
                         textDecoration = "bold", halign = "center"),
             rows = group_row, cols = 7:10, gridExpand = TRUE)
    mergeCells(wb, sheet_name, cols = 3:6,  rows = group_row)
    mergeCells(wb, sheet_name, cols = 7:10, rows = group_row)
    
    if (has_previous) {
      writeData(wb, sheet_name, "Previous Model", startRow = group_row, startCol = 11)
      addStyle(wb, sheet_name,
               createStyle(fgFill = "#7E57C2", fontColour = "#FFFFFF",
                           textDecoration = "bold", halign = "center"),
               rows = group_row, cols = 11:n_cols, gridExpand = TRUE)
      mergeCells(wb, sheet_name, cols = 11:n_cols, rows = group_row)
    }
    
  } else {
    df_export <- df_data %>%
      mutate(
        Date      = as.Date(Date),
        Actual    = round(Actual, 2),
        Predicted = round(Pred,   2),
        Error     = round(Pred - Actual, 2),
        Abs_Error = round(abs(Pred - Actual), 2),
        Pct_Error = round(ifelse(Actual != 0,
                                 (Pred - Actual) / Actual * 100, NA), 2)
      ) %>%
      select(Date, Actual, Predicted, Error, Abs_Error, Pct_Error) %>%
      append_previous_prediction() %>%
      mutate(Date = format(Date, "%d/%m/%Y"))
    
    n_cols     <- ncol(df_export)
    col_widths <- c(rep(16, min(n_cols, 6)), rep(20, max(0, n_cols - 6)))
    
    writeData(wb, sheet_name, df_export, startRow = data_row)
    apply_header_style(wb, sheet_name, data_row, 1:n_cols)
    
    if (has_previous) {
      group_row <- data_row - 1
      writeData(wb, sheet_name, "Current Model", startRow = group_row, startCol = 3)
      writeData(wb, sheet_name, "Previous Model", startRow = group_row, startCol = 7)
      addStyle(wb, sheet_name,
               createStyle(fgFill = "#2E75B6", fontColour = "#FFFFFF",
                           textDecoration = "bold", halign = "center"),
               rows = group_row, cols = 3:6, gridExpand = TRUE)
      addStyle(wb, sheet_name,
               createStyle(fgFill = "#7E57C2", fontColour = "#FFFFFF",
                           textDecoration = "bold", halign = "center"),
               rows = group_row, cols = 7:n_cols, gridExpand = TRUE)
      mergeCells(wb, sheet_name, cols = 3:6, rows = group_row)
      mergeCells(wb, sheet_name, cols = 7:n_cols, rows = group_row)
    }
  }
  
  stripe_rows(wb, sheet_name, nrow(df_export), data_row, n_cols)
  setColWidths(wb, sheet_name, cols = 1:n_cols, widths = col_widths)
}

# ═══════════════════════════════════════════════════════════════════════════
# 7. ROI / CONTRIBUTION HELPERS
# ═══════════════════════════════════════════════════════════════════════════

# ── Build contribution table ─────────────────────────────────────────────────
#
# df_pct behaviour:
#   df_pct = NULL   → % recalculated from period units: Units / sum(Units) × 100
#                     Used by: ROI sheet (filtered period)
#   df_pct = df_pct → % taken directly from model file (full model period)
#                     Used by: Full Period Contribution sheet
#
# Negatives are handled correctly in both cases:
#   Units / sum(Units) → all % sum to 100 regardless of sign
#
build_roi_table <- function(df_med_input, cftp_data,
                            df_input_filtered = NULL,
                            df_pct            = NULL) {
  
  all_cols     <- colnames(df_med_input)
  contrib_cols <- all_cols[grepl("^Contrib_", all_cols)]
  has_base     <- "Base" %in% all_cols
  
  cat("  Contrib_ columns:", length(contrib_cols),
      "| Base:", has_base, "\n")
  
  if (length(contrib_cols) == 0) stop("No Contrib_ columns found in df_med")
  if (has_base) contrib_cols <- c(contrib_cols, "Base")
  
  cftp_revenue <- calculate_cftp_revenue(df_med_input, contrib_cols, cftp_data)
  
  # ── Sum contributions over the (already filtered) period ──────────────────
  units <- df_med_input %>%
    select(all_of(contrib_cols)) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Units")
  
  # ── % Contribution ─────────────────────────────────────────────────────────
  if (!is.null(df_pct)) {
    
    # Full period: use model's official percentages from df_pct
    cat("  % Contribution: using model file (df_pct)\n")
    contrib_dict <- setNames(as.numeric(df_pct$Pct), df_pct$Variable)
    units <- units %>%
      mutate(Period_Pct = as.numeric(contrib_dict[Variable]))
    
  } else {
    
    # Filtered period: recalculate from units
    # Formula: Period_Pct = Units / sum(Units) × 100
    # → negatives kept as-is; all percentages sum to exactly 100 %
    total_units <- sum(units$Units, na.rm = TRUE)
    pos_sum     <- sum(units$Units[units$Units >  0], na.rm = TRUE)
    neg_sum     <- sum(units$Units[units$Units <  0], na.rm = TRUE)
    
    cat("  % Contribution: recalculated from period units\n")
    cat("  Total:", round(total_units, 1),
        "| Pos:", round(pos_sum, 1),
        "| Neg:", round(neg_sum, 1), "\n")
    
    if (abs(total_units) == 0)
      warning("Total units for period = 0. % Contribution will be NA.")
    
    units <- units %>%
      mutate(
        Period_Pct = if (abs(total_units) > 0) (Units / total_units) * 100
        else                         NA_real_
      )
  }
  
  # ── Spend lookup from data_input ──────────────────────────────────────────
  # Spend column names in data_input = var_clean (contrib col without Contrib_ prefix)
  spend_lookup <- setNames(numeric(0), character(0))
  spend_lookup_normalized <- setNames(numeric(0), character(0))
  
  if (!is.null(df_input_filtered)) {
    spend_cols <- df_input_filtered %>%
      select(-any_of(c("Date", "Actual", "Row"))) %>%
      select(where(is.numeric)) %>%
      colnames()
    spend_cols <- spend_cols[is_spend_column(spend_cols)]
    
    if (length(spend_cols) > 0) {
      spend_sums <- df_input_filtered %>%
        select(all_of(spend_cols)) %>%
        summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
        pivot_longer(everything(), names_to = "spend_col", values_to = "Spend_Total")
      
      spend_lookup <- setNames(spend_sums$Spend_Total, spend_sums$spend_col)
      spend_lookup_normalized <- setNames(
        spend_sums$Spend_Total,
        gsub("[^a-z0-9]+", "_", tolower(spend_sums$spend_col))
      )
      
      # Diagnostics
      var_clean_vals <- sub("^Contrib_", "", contrib_cols)
      var_clean_vals <- var_clean_vals[var_clean_vals != "Base"]
      var_normalized <- normalize_mapping_key(var_clean_vals)
      is_matched <- var_clean_vals %in% names(spend_lookup) | var_normalized %in% names(spend_lookup_normalized)
      n_match   <- sum(is_matched)
      unmatched <- var_clean_vals[!is_matched]
      
      cat("  Spend matched:", n_match, "/ Total:", length(var_clean_vals), "\n")
      if (length(unmatched) > 0)
        cat("  WARNING – no spend column for:",
            paste(unmatched, collapse = ", "), "\n")
    }
  }
  
  # ── Final table ────────────────────────────────────────────────────────────
  units %>%
    mutate(
      var_clean    = sub("^Contrib_", "", Variable),
      mapping      = lapply(Variable, get_channel_mapping),
      Channel      = sapply(mapping, `[[`, "channel"),
      Category     = sapply(mapping, `[[`, "category"),
      Sub_Category = sapply(mapping, `[[`, "sub_category"),
      Funnel       = sapply(mapping, `[[`, "funnel"),
      sort_key     = ifelse(
        Category == "Base",
        sort_order_map[["Base"]],
        coalesce(as.numeric(sort_order_map[Sub_Category]), sort_order_map[["Base"]] - 1)
      )
    ) %>%
    arrange(sort_key) %>%
    group_by(Category) %>%
    mutate(Model_Contribution = sum(Period_Pct, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      Expected_Contribution = NA_real_,
      
      # Spend: matched from data_input by var_clean name; NA if no match.
      Spend = vapply(
        var_clean,
        match_spend_total,
        numeric(1),
        spend_lookup = spend_lookup,
        spend_lookup_normalized = spend_lookup_normalized
      ),
      Has_Spend_Match = !is.na(Spend),
      
      Has_CFTP_Revenue = as.logical(cftp_revenue$covered_lookup[Variable]),
      Revenue = ifelse(
        Units > 0 & Has_Spend_Match & !is.na(Has_CFTP_Revenue) & Has_CFTP_Revenue,
        as.numeric(cftp_revenue$revenue_lookup[Variable]),
        NA_real_
      ),
      
      # ROI placeholder → overwritten by live Excel formula in write_roi_sheet
      ROI = ifelse(Units > 0 & Has_Spend_Match & !is.na(Revenue) & Spend != 0,
                   Revenue / Spend,
                   NA_real_)
    ) %>%
    select(
      Variable                = var_clean,
      Units,
      `% Contribution`        = Period_Pct,
      `Model Contribution`    = Model_Contribution,
      `Expected Contribution` = Expected_Contribution,
      Spend,                              # col 6 = F
      Revenue,                            # col 7 = G
      ROI,                                # col 8 = H  ← Excel formula
      Category,
      `Sub-Category`          = Sub_Category,
      Funnel,
      Channel
    )
}

# ── Write any ROI-style table to a worksheet ─────────────────────────────────
# ROI formula: =IFERROR(Revenue / Spend, "")
# Written only for rows with matched Spend (where Revenue is not NA)
write_roi_sheet <- function(wb, sheet_name, df_export) {
  df_export <- df_export %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df_export, startRow = 1)
  apply_header_style(wb, sheet_name, 1, 1:ncol(df_export))
  stripe_rows(wb, sheet_name, nrow(df_export), 1, ncol(df_export))
  
  # Column letter lookup
  spend_letter <- LETTERS[which(colnames(df_export) == "Spend")]    # F
  rev_letter   <- LETTERS[which(colnames(df_export) == "Revenue")]  # G
  roi_col_idx  <- which(colnames(df_export) == "ROI")               # 8
  
  for (i in seq_len(nrow(df_export))) {
    if (!is.na(df_export$Revenue[i])) {
      data_row <- i + 1L
      writeFormula(
        wb, sheet_name,
        x        = paste0('=IFERROR(ROUND(', rev_letter, data_row,
                          '/', spend_letter, data_row, ',3),\"\")'),
        startRow = data_row,
        startCol = roi_col_idx
      )
    }
  }
  
  setColWidths(wb, sheet_name,
               cols   = seq_along(ROI_COL_WIDTHS),
               widths = ROI_COL_WIDTHS)
  
  cat(" →", sheet_name, "written:", nrow(df_export), "rows\n")
}

# ── ROI sheet  (filtered period  |  % recalculated from period units) ────────
add_roi_sheet <- function(wb, df_med, df_input,
                          contrib_date_from, contrib_date_to,
                          cftp_data) {
  cat("\nBuilding ROI sheet (filtered period)...\n")
  
  df_med_f   <- df_med   %>% filter(Date >= contrib_date_from & Date <= contrib_date_to)
  df_input_f <- df_input %>% filter(Date >= contrib_date_from & Date <= contrib_date_to)
  
  cat("  Period:", as.character(contrib_date_from),
      "→", as.character(contrib_date_to), "\n")
  cat("  Rows – contrib:", nrow(df_med_f),
      "| spend:", nrow(df_input_f), "\n")
  
  # df_pct = NULL → % recalculated from filtered period units
  df_export <- build_roi_table(df_med_f, cftp_data = cftp_data,
                               df_input_filtered = df_input_f,
                               df_pct            = NULL)
  write_roi_sheet(wb, "ROI", df_export)
}

# ── Full Period Contribution  (all dates  |  % from df_pct model file) ───────
add_full_period_contrib_sheet <- function(wb, df_med, df_pct, df_input,
                                          cftp_data) {
  cat("\nBuilding Full Period Contribution sheet (all dates)...\n")
  cat("  Date range:", as.character(min(df_med$Date)),
      "→", as.character(max(df_med$Date)), "\n")
  cat("  Total rows:", nrow(df_med), "\n")
  
  # df_pct passed → % taken from model file (official full-period %)
  df_export <- build_roi_table(df_med, cftp_data = cftp_data,
                               df_input_filtered = df_input,
                               df_pct            = df_pct)
  write_roi_sheet(wb, "Full Period Contribution", df_export)
}

# ── Historical Contributions  (one row per date, wide format) ────────────────
add_historical_contrib_sheet <- function(wb, df_med) {
  cat("\nBuilding Historical Contributions sheet (by date)...\n")
  
  all_cols     <- colnames(df_med)
  contrib_cols <- all_cols[grepl("^Contrib_", all_cols)]
  has_base     <- "Base" %in% all_cols
  
  if (length(contrib_cols) == 0)
    stop("No Contrib_ columns found for Historical Contributions sheet")
  
  select_cols <- c("Date", contrib_cols)
  if (has_base) select_cols <- c(select_cols, "Base")
  
  df_hist <- df_med %>%
    select(all_of(select_cols)) %>%
    arrange(Date) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    mutate(Date = format(Date, "%d/%m/%Y"))
  
  # Remove Contrib_ prefix for cleaner column headers
  colnames(df_hist) <- sub("^Contrib_", "", colnames(df_hist))
  
  n_cols <- ncol(df_hist)
  
  addWorksheet(wb, "Historical Contributions")
  writeData(wb, "Historical Contributions", df_hist, startRow = 1)
  apply_header_style(wb, "Historical Contributions", 1, 1:n_cols)
  stripe_rows(wb, "Historical Contributions", nrow(df_hist), 1, n_cols)
  setColWidths(wb, "Historical Contributions", cols = 1:n_cols,
               widths = c(14, rep(20, n_cols - 1)))
  
  cat(" → Historical Contributions written:",
      nrow(df_hist), "rows ×", n_cols, "cols\n")
}

# ═══════════════════════════════════════════════════════════════════════════
# 8. PRE vs POST CONTRIBUTION
# ═══════════════════════════════════════════════════════════════════════════
add_pre_vs_post_sheet <- function(wb, df_med, cutoff_date) {
  cat("\nBuilding Pre vs Post sheet...\n")
  cat("  Cutoff:", as.character(cutoff_date), "\n")
  
  # ── Split periods ──────────────────────────────────────────────────────────
  df_pre  <- df_med %>% filter(Date <= cutoff_date)
  df_post <- df_med %>% filter(Date >  cutoff_date)
  
  if (nrow(df_pre)  == 0) stop("No data found before cutoff_date")
  if (nrow(df_post) == 0) stop("No data found after cutoff_date")
  
  cat("  Pre :", as.character(min(df_pre$Date)),
      "→", as.character(max(df_pre$Date)),
      "(", nrow(df_pre), "rows )\n")
  cat("  Post:", as.character(min(df_post$Date)),
      "→", as.character(max(df_post$Date)),
      "(", nrow(df_post), "rows )\n")
  
  # ── Contribution columns ───────────────────────────────────────────────────
  all_cols     <- colnames(df_med)
  contrib_cols <- all_cols[grepl("^Contrib_", all_cols)]
  has_base     <- "Base" %in% all_cols
  
  if (length(contrib_cols) == 0) stop("No Contrib_ columns found")
  if (has_base) contrib_cols <- c(contrib_cols, "Base")
  
  # ── Helper: sum a period ───────────────────────────────────────────────────
  sum_period <- function(df_period) {
    df_period %>%
      select(all_of(contrib_cols)) %>%
      summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Units")
  }
  
  pre_units  <- sum_period(df_pre)  %>% rename(Pre_Units  = Units)
  post_units <- sum_period(df_post) %>% rename(Post_Units = Units)
  
  units <- inner_join(pre_units, post_units, by = "Variable")
  
  # ── Build final table ──────────────────────────────────────────────────────
  df_result <- units %>%
    mutate(
      var_clean    = sub("^Contrib_", "", Variable),
      mapping      = lapply(Variable, get_channel_mapping),
      Channel      = sapply(mapping, `[[`, "channel"),
      Category     = sapply(mapping, `[[`, "category"),
      Sub_Category = sapply(mapping, `[[`, "sub_category"),
      Funnel       = sapply(mapping, `[[`, "funnel"),
      sort_key     = ifelse(
        Category == "Base",
        sort_order_map[["Base"]],
        coalesce(as.numeric(sort_order_map[Sub_Category]), sort_order_map[["Base"]] - 1)
      )
    ) %>%
    arrange(sort_key) %>%
    select(
      Variable       = var_clean,
      Category,
      `Sub-Category` = Sub_Category,
      Funnel,
      Channel,
      `Pre Units`    = Pre_Units,
      `Post Units`   = Post_Units
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  # ── Write to Excel ─────────────────────────────────────────────────────────
  pre_label  <- paste0("Pre  [ ", as.character(min(df_pre$Date)),
                       " → ", as.character(max(df_pre$Date)),  " ]")
  post_label <- paste0("Post [ ", as.character(min(df_post$Date)),
                       " → ", as.character(max(df_post$Date)), " ]")
  title_text <- paste("Pre vs Post Contribution  |  Cutoff:", as.character(cutoff_date),
                      " |  ", pre_label, "  |  ", post_label)
  
  addWorksheet(wb, "Pre vs Post")
  
  # Title row
  writeData(wb, "Pre vs Post", title_text, startRow = 1, startCol = 1)
  addStyle(wb, "Pre vs Post",
           createStyle(fontSize = 11, textDecoration = "bold",
                       fontColour = "#1F4E79", halign = "left"),
           rows = 1, cols = 1:ncol(df_result), gridExpand = TRUE)
  mergeCells(wb, "Pre vs Post", cols = 1:ncol(df_result), rows = 1)
  
  # Data starts at row 2
  writeData(wb, "Pre vs Post", df_result, startRow = 2)
  apply_header_style(wb, "Pre vs Post", 2, 1:ncol(df_result))
  stripe_rows(wb, "Pre vs Post", nrow(df_result), 2, ncol(df_result))
  
  # Column widths
  col_widths <- c(55, 20, 25, 25, 10, 14, 14)
  setColWidths(wb, "Pre vs Post",
               cols   = 1:length(col_widths),
               widths = col_widths)
  
  cat(" → Pre vs Post written:", nrow(df_result), "rows\n")
}

# ═══════════════════════════════════════════════════════════════════════════
# GRADIENT APPLICATION
# ═══════════════════════════════════════════════════════════════════════════
load_gradient <- function(filepath, sheet = 1) {
  
  ext <- tolower(tools::file_ext(filepath))
  
  df_grad <- if (ext == "csv") {
    read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    available_sheets <- openxlsx::getSheetNames(filepath)
    cat("Sheets disponibles en", basename(filepath), ":\n")
    cat(paste0("  [", seq_along(available_sheets), "] ", available_sheets, "\n"))
    
    openxlsx::read.xlsx(filepath, sheet = sheet, detectDates = TRUE)  # ← detectDates
  }
  
  colnames(df_grad) <- trimws(colnames(df_grad))
  
  month_col    <- colnames(df_grad)[grepl("^month$",    colnames(df_grad), ignore.case = TRUE)][1]
  gradient_col <- colnames(df_grad)[grepl("^gradient$", colnames(df_grad), ignore.case = TRUE)][1]
  
  if (is.na(month_col))    stop("No se encontró columna 'Month'")
  if (is.na(gradient_col)) stop("No se encontró columna 'Gradient'")
  
  df_grad <- df_grad %>%
    rename(Month = !!sym(month_col), Gradient = !!sym(gradient_col)) %>%
    mutate(
      # ── Maneja los 3 casos posibles ────────────────────────────────────
      Month = if (inherits(Month, "Date")) {
        Month                                          # ya es Date (detectDates funcionó)
      } else if (is.numeric(Month)) {
        as.Date(Month, origin = "1899-12-30")          # serial Excel → Date correcta
      } else {
        as.Date(as.character(Month),
                tryFormats = c("%d/%m/%Y", "%m/%d/%Y", "%Y-%m-%d"))  # character → Date
      },
      Month    = floor_date(Month, "month"),
      Gradient = as.numeric(Gradient)
    ) %>%
    filter(!is.na(Month), !is.na(Gradient)) %>%
    select(Month, Gradient)
  
  sheet_name <- if (is.numeric(sheet)) openxlsx::getSheetNames(filepath)[sheet] else sheet
  cat("Gradient file loaded — sheet:", sheet_name, "—", nrow(df_grad), "months\n")
  print(df_grad)
  
  df_grad
}

apply_gradient <- function(df_med, df_gradient) {
  
  contrib_cols <- colnames(df_med)[grepl("^Contrib_", colnames(df_med))]
  has_base     <- "Base" %in% colnames(df_med)
  if (has_base) contrib_cols <- c(contrib_cols, "Base")
  
  if (length(contrib_cols) == 0) stop("No Contrib_ columns found in df_med")
  
  # ── Crear month_key en df_med ─────────────────────────────────────────────
  df_med_keyed <- df_med %>%
    mutate(month_key = as.Date(floor_date(Date, "month")))
  
  df_gradient <- df_gradient %>%
    mutate(Month = as.Date(floor_date(Month, "month")))
  
  # ── Diagnóstico del join ──────────────────────────────────────────────────
  med_months  <- sort(unique(df_med_keyed$month_key))
  grad_months <- sort(df_gradient$Month)
  matched     <- grad_months[grad_months %in% med_months]
  
  cat("\n--- Gradient Diagnostics ---\n")
  cat("Class month_key (df_med)  :", class(med_months),  "\n")
  cat("Class Month (df_gradient) :", class(grad_months), "\n")
  cat("Months in df_med          :", paste(format(med_months,  "%Y-%m"), collapse = ", "), "\n")
  cat("Months in gradient        :", paste(format(grad_months, "%Y-%m"), collapse = ", "), "\n")
  cat("Months matched            :", length(matched), "→",
      if (length(matched) > 0) paste(format(matched, "%b %Y"), collapse = ", ") else "NINGUNO", "\n")
  
  if (length(matched) == 0) {
    warning("GRADIENT NO APLICADO: ningún mes coincide entre df_med y gradient file.")
    return(df_med)
  }
  
  # ── Join + aplicar gradiente a todas las Contrib_ ─────────────────────────
  df_out <- df_med_keyed %>%
    left_join(
      df_gradient %>% rename(month_key = Month),
      by = "month_key"
    ) %>%
    mutate(
      across(
        all_of(contrib_cols),
        ~ ifelse(!is.na(Gradient), . * Gradient, .)
      )
    ) %>%
    # ── Recalcular Pred = suma de todas las contribuciones ─────────────────
    mutate(
      Pred = rowSums(across(all_of(contrib_cols)), na.rm = TRUE)
    ) %>%
    select(-month_key, -Gradient)
  
  # ── Verificación: Pred antes vs después ───────────────────────────────────
  df_check <- df_med_keyed %>%
    filter(month_key %in% matched) %>%
    left_join(df_gradient %>% rename(month_key = Month), by = "month_key") %>%
    slice_head(n = 5) %>%
    select(Date, Gradient, Pred_Before = Pred) %>%
    left_join(
      df_out %>% select(Date, Pred_After = Pred),
      by = "Date"
    )
  
  cat("\nVerificación — Pred antes vs después (primeras 5 filas afectadas):\n")
  print(df_check)
  
  rows_affected <- sum(df_med_keyed$month_key %in% matched)
  cat("\nTotal rows affected:", rows_affected, "of", nrow(df_med), "\n")
  cat("----------------------------\n")
  
  df_out
}
