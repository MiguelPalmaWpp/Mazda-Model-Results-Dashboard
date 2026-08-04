calculate_granularity_metrics <- function(df_daily, df_weekly, df_monthly, pred_col = "Pred") {
  list(
    daily = calculate_all_metrics(df_daily, pred_col = pred_col),
    weekly = calculate_all_metrics(df_weekly, pred_col = pred_col),
    monthly = calculate_all_metrics(df_monthly, pred_col = pred_col)
  )
}

build_analysis <- function(data_loaded, cutoff_date, aggregation_method,
                            roi_from, roi_to, compare_new_period,
                            use_gradient, gradient_path, gradient_sheet,
                            cftp_path, cftp_sheet, cftp_nameplate) {
  df <- data_loaded$df
  df_med <- data_loaded$df_med
  df_med_original <- df_med
  df_pct <- data_loaded$df_pct
  df_input <- data_loaded$df_input
  gradient_applied <- FALSE
  gradient_message <- "Gradient adjustment was not applied."
  cftp_data <- load_cftp(cftp_path, sheet = cftp_sheet, nameplate = cftp_nameplate)
  cftp_message <- paste0(
    "CFTP loaded for ", cftp_nameplate,
    " (", length(unique(cftp_data$Month)), " months)"
  )

  if (isTRUE(use_gradient) && !is.null(gradient_path) && nzchar(gradient_path)) {
    df_gradient <- load_gradient(gradient_path, sheet = gradient_sheet)
    df_med <- apply_gradient(df_med, df_gradient)
    df <- df %>%
      select(Date, Actual) %>%
      inner_join(df_med_original %>% select(Date, Pred), by = "Date") %>%
      inner_join(df_med %>% select(Date, Pred_Gradient = Pred), by = "Date") %>%
      arrange(Date)
    gradient_applied <- TRUE
    gradient_message <- paste("Gradient adjustment applied using sheet:", gradient_sheet)
  }

  df_weekly <- aggregate_data(df, "week", aggregation_method)
  df_monthly <- aggregate_data(df, "month", aggregation_method)

  metrics_by_granularity <- calculate_granularity_metrics(df, df_weekly, df_monthly)
  metrics_daily <- metrics_by_granularity$daily
  metrics_weekly <- metrics_by_granularity$weekly
  metrics_monthly <- metrics_by_granularity$monthly
  metrics_over_time <- calculate_metrics_over_time(df)

  if (isTRUE(compare_new_period)) {
    df_med_roi <- df_med_original %>% filter(Date >= roi_from & Date <= roi_to)
    df_med_gradient_roi <- df_med %>% filter(Date >= roi_from & Date <= roi_to)
    df_input_roi <- df_input %>% filter(Date >= roi_from & Date <= roi_to)
    correlation_cutoff <- cutoff_date
    pre_vs_post_table <- build_pre_vs_post_table(df_med, cutoff_date)
    roi_period_label <- paste(as.character(roi_from), "to", as.character(roi_to))
  } else {
    df_med_roi <- df_med_original
    df_med_gradient_roi <- df_med
    df_input_roi <- df_input
    correlation_cutoff <- NULL
    pre_vs_post_table <- build_pre_vs_post_table(df_med, NULL)
    roi_period_label <- "Full available period"
  }
  
  cftp_missing_months <- missing_cftp_months(df_med_roi, cftp_data)
  if(length(cftp_missing_months) > 0) {
    cftp_message <- paste0(
      cftp_message,
      "; missing ROI months: ",
      paste(cftp_missing_months, collapse = ", ")
    )
  }

  roi_table <- build_roi_table(
    df_med_roi,
    cftp_data = cftp_data,
    df_input_filtered = df_input_roi,
    df_pct = NULL
  )

  full_period_table <- build_roi_table(
    df_med_original,
    cftp_data = cftp_data,
    df_input_filtered = df_input,
    df_pct = df_pct
  )

  roi_table_gradient <- if (gradient_applied) {
    build_roi_table(
      df_med_gradient_roi,
      cftp_data = cftp_data,
      df_input_filtered = df_input_roi,
      df_pct = NULL
    )
  } else {
    data.frame(Message = "Gradient adjustment was not applied.")
  }

  full_period_table_gradient <- if (gradient_applied) {
    build_roi_table(
      df_med,
      cftp_data = cftp_data,
      df_input_filtered = df_input,
      df_pct = df_pct
    )
  } else {
    data.frame(Message = "Gradient adjustment was not applied.")
  }

  metrics_gradient_by_granularity <- if (gradient_applied) {
    calculate_granularity_metrics(df, df_weekly, df_monthly, pred_col = "Pred_Gradient")
  } else {
    NULL
  }

  overview_metrics_gradient <- if (!is.null(metrics_gradient_by_granularity)) {
    bind_rows(
      metrics_to_df(metrics_gradient_by_granularity$daily, "Daily"),
      metrics_to_df(metrics_gradient_by_granularity$weekly, "Weekly"),
      metrics_to_df(metrics_gradient_by_granularity$monthly, "Monthly")
    )
  } else {
    NULL
  }

  list(
    df = df,
    df_med = df_med,
    df_med_original = df_med_original,
    df_pct = df_pct,
    df_input = df_input,
    cftp_data = cftp_data,
    cftp_nameplate = cftp_nameplate,
    cftp_message = cftp_message,
    cftp_missing_months = cftp_missing_months,
    df_weekly = df_weekly,
    df_monthly = df_monthly,
    metrics_daily = metrics_daily,
    metrics_weekly = metrics_weekly,
    metrics_monthly = metrics_monthly,
    metrics_over_time = metrics_over_time,
    overview_metrics = bind_rows(
      metrics_to_df(metrics_daily, "Daily"),
      metrics_to_df(metrics_weekly, "Weekly"),
      metrics_to_df(metrics_monthly, "Monthly")
    ),
    correlation = build_correlation_table(df, correlation_cutoff),
    roi_table = roi_table,
    roi_table_gradient = roi_table_gradient,
    roi_period_label = roi_period_label,
    full_period_table = full_period_table,
    full_period_table_gradient = full_period_table_gradient,
    historical_table = build_historical_contributions_table(df_med),
    long_format_table = build_long_format_table(
      df_med_original,
      df_input,
      if (isTRUE(gradient_applied)) df_med else NULL
    ),
    pre_vs_post_table = pre_vs_post_table,
    overview_metrics_gradient = overview_metrics_gradient,
    compare_new_period = isTRUE(compare_new_period),
    gradient_applied = gradient_applied,
    gradient_message = gradient_message
  )
}

build_excel_report <- function(analysis, cutoff_date, roi_from, roi_to) {
  wb <- createWorkbook()

  add_summary_sheet(
    wb,
    analysis$metrics_daily,
    analysis$metrics_weekly,
    analysis$metrics_monthly,
    NULL,
    NULL,
    NULL
  )

  add_metrics_over_time_sheet(wb, analysis$metrics_over_time, NULL)
  write_granularity_sheet(wb, "Daily", analysis$metrics_daily, analysis$df)
  write_granularity_sheet(wb, "Weekly", analysis$metrics_weekly, analysis$df_weekly)
  write_granularity_sheet(wb, "Monthly", analysis$metrics_monthly, analysis$df_monthly)

  roi_export_table <- if (isTRUE(analysis$gradient_applied)) {
    analysis$roi_table_gradient
  } else {
    analysis$roi_table
  }
  write_roi_sheet(wb, "ROI", roi_export_table)

  full_period_export_table <- if (isTRUE(analysis$gradient_applied)) {
    analysis$full_period_table_gradient
  } else {
    analysis$full_period_table
  }
  write_roi_sheet(wb, "Full Period Contribution", full_period_export_table)

  add_historical_contrib_sheet(wb, df_med = analysis$df_med)
  if (isTRUE(analysis$compare_new_period) &&
      any(analysis$df_med$Date <= cutoff_date, na.rm = TRUE) &&
      any(analysis$df_med$Date > cutoff_date, na.rm = TRUE)) {
    add_pre_vs_post_sheet(wb, df_med = analysis$df_med, cutoff_date = cutoff_date)
  } else {
    addWorksheet(wb, "Pre vs Post")
    writeData(wb, "Pre vs Post", round_numeric_columns(analysis$pre_vs_post_table, 3))
  }

  wb
}
