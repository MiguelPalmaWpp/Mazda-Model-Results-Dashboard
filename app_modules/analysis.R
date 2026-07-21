calculate_granularity_metrics <- function(df_daily, df_weekly, df_monthly, pred_col = "Pred") {
  list(
    daily = calculate_all_metrics(df_daily, pred_col = pred_col),
    weekly = calculate_all_metrics(df_weekly, pred_col = pred_col),
    monthly = calculate_all_metrics(df_monthly, pred_col = pred_col)
  )
}

build_analysis <- function(data_loaded, cutoff_date, revenue_per_unit, aggregation_method,
                           roi_from, roi_to, compare_new_period,
                           use_gradient, gradient_path, gradient_sheet) {
  df <- data_loaded$df
  df_med <- data_loaded$df_med
  df_med_original <- df_med
  df_pct <- data_loaded$df_pct
  df_input <- data_loaded$df_input
  gradient_applied <- FALSE
  gradient_message <- "Gradient adjustment was not applied."

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

  roi_table <- build_roi_table(
    df_med_roi,
    revenue_param = revenue_per_unit,
    df_input_filtered = df_input_roi,
    df_pct = NULL
  )

  full_period_table <- build_roi_table(
    df_med_original,
    revenue_param = revenue_per_unit,
    df_input_filtered = df_input,
    df_pct = df_pct
  )

  roi_table_gradient <- if (gradient_applied) {
    build_roi_table(
      df_med_gradient_roi,
      revenue_param = revenue_per_unit,
      df_input_filtered = df_input_roi,
      df_pct = NULL
    )
  } else {
    data.frame(Message = "Gradient adjustment was not applied.")
  }

  full_period_table_gradient <- if (gradient_applied) {
    build_roi_table(
      df_med,
      revenue_param = revenue_per_unit,
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
    pre_vs_post_table = pre_vs_post_table,
    overview_metrics_gradient = overview_metrics_gradient,
    compare_new_period = isTRUE(compare_new_period),
    gradient_applied = gradient_applied,
    gradient_message = gradient_message
  )
}

build_excel_report <- function(analysis, cutoff_date, roi_from, roi_to, revenue_per_unit) {
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

  if (isTRUE(analysis$compare_new_period)) {
    add_roi_sheet(
      wb,
      df_med = analysis$df_med,
      df_input = analysis$df_input,
      contrib_date_from = roi_from,
      contrib_date_to = roi_to,
      revenue_param = revenue_per_unit
    )
  } else {
    write_roi_sheet(
      wb,
      "ROI",
      build_roi_table(
        analysis$df_med,
        revenue_param = revenue_per_unit,
        df_input_filtered = analysis$df_input,
        df_pct = NULL
      )
    )
  }

  add_full_period_contrib_sheet(
    wb,
    df_med = analysis$df_med,
    df_pct = analysis$df_pct,
    df_input = analysis$df_input,
    revenue_param = revenue_per_unit
  )

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
