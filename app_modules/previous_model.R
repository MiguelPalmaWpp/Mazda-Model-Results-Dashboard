parse_report_date <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  
  x_chr <- trimws(as.character(x))
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x_chr,
    orders = c("ymd", "dmy", "mdy", "Ymd HMS", "dmY HMS", "mdY HMS"),
    exact = FALSE
  ))
  as.Date(parsed)
}

read_sheet_if_exists <- function(filepath, sheet) {
  sheets <- openxlsx::getSheetNames(filepath)
  matched <- sheets[tolower(sheets) == tolower(sheet)]
  if (length(matched) == 0) {
    return(NULL)
  }
  openxlsx::read.xlsx(filepath, sheet = matched[1], colNames = FALSE, detectDates = FALSE)
}

find_header_row <- function(df, required) {
  if (is.null(df) || nrow(df) == 0) {
    return(NA_integer_)
  }
  required_key <- normalize_mapping_key(required)
  for (i in seq_len(nrow(df))) {
    row_key <- normalize_mapping_key(unlist(df[i, ], use.names = FALSE))
    if (all(required_key %in% row_key)) {
      return(i)
    }
  }
  NA_integer_
}

sheet_table_from_header <- function(filepath, sheet, required) {
  df_raw <- read_sheet_if_exists(filepath, sheet)
  header_row <- find_header_row(df_raw, required)
  if (is.na(header_row)) {
    return(NULL)
  }
  
  header <- as.character(unlist(df_raw[header_row, ], use.names = FALSE))
  keep <- !is.na(header) & nzchar(trimws(header))
  if (!any(keep)) {
    return(NULL)
  }
  
  df <- df_raw[(header_row + 1):nrow(df_raw), keep, drop = FALSE]
  names(df) <- make.unique(trimws(header[keep]))
  df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  as_tibble(df)
}

previous_sheet_defaults <- function(filepath) {
  sheets <- openxlsx::getSheetNames(filepath)
  sheet_key <- normalize_mapping_key(sheets)
  
  contribution_idx <- match("historical_contributions", sheet_key)
  if (is.na(contribution_idx)) {
    contribution_idx <- match("med_contrib", sheet_key)
  }
  if (is.na(contribution_idx)) {
    contribution_idx <- 1
  }
  
  roi_idx <- match("roi", sheet_key)
  if (is.na(roi_idx)) {
    roi_idx <- contribution_idx
  }
  
  list(
    sheets = sheets,
    contribution_sheet = sheets[[contribution_idx]],
    roi_sheet = sheets[[roi_idx]]
  )
}

normalize_comparison_variable_key <- function(x) {
  clean <- normalize_mapping_key(sub("^Contrib_", "", x))
  
  case_when(
    grepl("kpi_sales_retail_sales_retail_sales.*lag_7", clean) ~ "kpi_sales_retail_sales_retail_sales_lag_7",
    grepl("time_months_time_months.*_12$|month_december|month.*december", clean) ~ "month_december",
    TRUE ~ clean
  )
}

previous_prediction_column <- function(cols) {
  keys <- normalize_mapping_key(cols)
  matched <- which(keys %in% c("pred", "prediction", "predicted"))
  if (length(matched) == 0) {
    return(NA_character_)
  }
  cols[[matched[1]]]
}

previous_date_column <- function(cols) {
  keys <- normalize_mapping_key(cols)
  matched <- which(keys == "date")
  if (length(matched) == 0) {
    return(NA_character_)
  }
  cols[[matched[1]]]
}

read_previous_contribution_sheet <- function(filepath, sheet) {
  df <- openxlsx::read.xlsx(filepath, sheet = sheet, colNames = TRUE, detectDates = FALSE) %>%
    as_tibble()
  names(df) <- trimws(names(df))
  
  date_col <- previous_date_column(names(df))
  if (is.na(date_col)) {
    stop("Previous contribution sheet must include a Date column.")
  }
  
  pred_col <- previous_prediction_column(names(df))
  excluded_keys <- c("date", "actual", "pred", "prediction", "predicted", "row")
  contribution_cols <- names(df)[!normalize_mapping_key(names(df)) %in% excluded_keys]
  numeric_contribution_cols <- contribution_cols[vapply(df[contribution_cols], function(x) {
    any(!is.na(suppressWarnings(as.numeric(x))))
  }, logical(1))]
  
  if (length(numeric_contribution_cols) == 0) {
    stop("Previous contribution sheet must include numeric contribution columns.")
  }
  
  df_numeric <- df %>%
    mutate(Date = parse_report_date(.data[[date_col]])) %>%
    mutate(across(all_of(numeric_contribution_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
    filter(!is.na(Date))
  
  if (!is.na(pred_col)) {
    df_numeric <- df_numeric %>%
      mutate(Previous_Pred = suppressWarnings(as.numeric(.data[[pred_col]])))
  } else {
    df_numeric <- df_numeric %>%
      mutate(Previous_Pred = rowSums(across(all_of(numeric_contribution_cols)), na.rm = TRUE))
  }
  
  variable_totals <- df_numeric %>%
    summarise(across(all_of(numeric_contribution_cols), ~ sum(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Previous_Units") %>%
    mutate(
      Variable = sub("^Contrib_", "", Variable),
      variable_key = normalize_comparison_variable_key(Variable)
    ) %>%
    group_by(variable_key) %>%
    summarise(
      Variable = first(Variable),
      Previous_Units = sum(Previous_Units, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Previous_Pct_Contribution = ifelse(
        sum(Previous_Units, na.rm = TRUE) != 0,
        Previous_Units / sum(Previous_Units, na.rm = TRUE) * 100,
        NA_real_
      ),
      mapping = lapply(Variable, get_channel_mapping),
      Category = sapply(mapping, `[[`, "category"),
      `Sub-Category` = sapply(mapping, `[[`, "sub_category")
    ) %>%
    select(-mapping)
  
  list(
    daily = df_numeric %>%
      select(Date, Previous_Pred) %>%
      filter(!is.na(Previous_Pred)) %>%
      arrange(Date),
    contribution_daily = df_numeric %>%
      select(Date, all_of(numeric_contribution_cols)) %>%
      arrange(Date),
    contribution = variable_totals,
    prediction_source = if (!is.na(pred_col)) pred_col else "sum of contribution columns"
  )
}

aggregate_previous_series <- function(df, freq, method = "sum") {
  agg_fn <- if (identical(method, "mean")) mean else sum
  df %>%
    mutate(Period = if (freq == "week") {
      floor_date(Date, freq, week_start = 1)
    } else {
      floor_date(Date, freq)
    }) %>%
    group_by(Period) %>%
    summarise(Previous_Pred = agg_fn(Previous_Pred, na.rm = TRUE), .groups = "drop") %>%
    rename(Date = Period) %>%
    arrange(Date)
}

previous_series_from_contributions <- function(daily, aggregation_method = "sum") {
  list(
    Daily = daily,
    Weekly = aggregate_previous_series(daily, "week", aggregation_method),
    Monthly = aggregate_previous_series(daily, "month", aggregation_method)
  )
}

normalize_previous_roi <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble(
      variable_key = character(),
      Variable = character(),
      Category = character(),
      `Sub-Category` = character(),
      Previous_Units = numeric(),
      Previous_Pct_Contribution = numeric(),
      Previous_Spend = numeric(),
      Previous_ROI = numeric()
    ))
  }
  
  names(df) <- trimws(names(df))
  units_col <- intersect(c("Units", "Contribution"), names(df))[1]
  pct_col <- intersect(c("% Contribution", "%Contribution", "%Contribution"), names(df))[1]
  has_roi <- "ROI" %in% names(df)
  has_halo_roi <- "Halo ROI" %in% names(df)
  roi_values <- rep(NA_real_, nrow(df))
  if (has_roi) {
    roi_values <- suppressWarnings(as.numeric(df[["ROI"]]))
  }
  if (has_halo_roi) {
    halo_values <- suppressWarnings(as.numeric(df[["Halo ROI"]]))
    roi_values <- coalesce(roi_values, halo_values)
  }
  
  df %>%
    transmute(
      variable_key = normalize_comparison_variable_key(.data[["Variable"]]),
      Variable = sub("^Contrib_", "", as.character(.data[["Variable"]])),
      Category = if ("Category" %in% names(df)) as.character(.data[["Category"]]) else NA_character_,
      `Sub-Category` = if ("Sub-Category" %in% names(df)) as.character(.data[["Sub-Category"]]) else NA_character_,
      Previous_Units = if (!is.na(units_col)) suppressWarnings(as.numeric(.data[[units_col]])) else NA_real_,
      Previous_Pct_Contribution = if (!is.na(pct_col)) suppressWarnings(as.numeric(.data[[pct_col]])) else NA_real_,
      Previous_Spend = if ("Spend" %in% names(df)) suppressWarnings(as.numeric(.data[["Spend"]])) else NA_real_,
      Previous_ROI = roi_values
    ) %>%
    distinct(variable_key, .keep_all = TRUE)
}

read_previous_roi <- function(filepath, sheet) {
  roi_df <- sheet_table_from_header(filepath, sheet, c("Variable"))
  normalize_previous_roi(roi_df)
}

load_previous_model_report <- function(filepath, contribution_sheet = NULL, roi_sheet = NULL, aggregation_method = "sum") {
  defaults <- previous_sheet_defaults(filepath)
  contribution_sheet <- contribution_sheet %||% defaults$contribution_sheet
  roi_sheet <- roi_sheet %||% defaults$roi_sheet
  contribution_data <- read_previous_contribution_sheet(filepath, contribution_sheet)
  
  list(
    filepath = filepath,
    filename = basename(filepath),
    contribution_sheet = contribution_sheet,
    roi_sheet = roi_sheet,
    prediction_source = contribution_data$prediction_source,
    series = previous_series_from_contributions(contribution_data$daily, aggregation_method),
    roi = read_previous_roi(filepath, roi_sheet),
    contribution_daily = contribution_data$contribution_daily,
    contribution = contribution_data$contribution
  )
}

current_prediction_col <- function(analysis) {
  if (isTRUE(analysis$gradient_applied) && "Pred_Gradient" %in% names(analysis$df)) {
    "Pred_Gradient"
  } else {
    "Pred"
  }
}

current_series_for_granularity <- function(analysis, granularity) {
  pred_col <- current_prediction_col(analysis)
  df <- switch(
    granularity,
    "Daily" = analysis$df,
    "Weekly" = analysis$df_weekly,
    "Monthly" = analysis$df_monthly,
    analysis$df
  )
  
  if (!pred_col %in% names(df)) {
    pred_col <- "Pred"
  }
  
  df %>%
    transmute(
      Date = as.Date(Date),
      Actual = as.numeric(Actual),
      Current_Pred = as.numeric(.data[[pred_col]])
    ) %>%
    filter(!is.na(Date), !is.na(Actual), !is.na(Current_Pred)) %>%
    arrange(Date)
}

comparison_joined_series <- function(analysis, previous_report, granularity) {
  previous <- previous_report$series[[granularity]]
  if (is.null(previous) || nrow(previous) == 0) {
    return(tibble())
  }
  
  current_series_for_granularity(analysis, granularity) %>%
    inner_join(previous %>% select(Date, Previous_Pred), by = "Date") %>%
    mutate(
      Current_Residual = Actual - Current_Pred,
      Previous_Residual = Actual - Previous_Pred
    ) %>%
    arrange(Date)
}

comparison_coverage_table <- function(analysis, previous_report) {
  bind_rows(lapply(c("Daily", "Weekly", "Monthly"), function(granularity) {
    current <- current_series_for_granularity(analysis, granularity)
    previous <- previous_report$series[[granularity]]
    if (is.null(previous) || nrow(previous) == 0) {
      return(tibble(
        Granularity = granularity,
        Current_Range = paste(min(current$Date), "to", max(current$Date)),
        Previous_Range = NA_character_,
        Common_Range = NA_character_,
        Current_Periods = nrow(current),
        Previous_Periods = 0L,
        Common_Periods = 0L,
        Missing_Current_Periods = 0L,
        Missing_Previous_Periods = nrow(current),
        Match_Status = "Previous series missing"
      ))
    }
    
    common_dates <- intersect(current$Date, previous$Date)
    missing_current <- setdiff(previous$Date, current$Date)
    missing_previous <- setdiff(current$Date, previous$Date)
    tibble(
      Granularity = granularity,
      Current_Range = paste(min(current$Date), "to", max(current$Date)),
      Previous_Range = paste(min(previous$Date), "to", max(previous$Date)),
      Common_Range = if (length(common_dates) > 0) paste(min(common_dates), "to", max(common_dates)) else NA_character_,
      Current_Periods = nrow(current),
      Previous_Periods = nrow(previous),
      Common_Periods = length(common_dates),
      Missing_Current_Periods = length(missing_current),
      Missing_Previous_Periods = length(missing_previous),
      Match_Status = case_when(
        length(common_dates) == 0 ~ "No overlap",
        length(missing_current) == 0 && length(missing_previous) == 0 ~ "Exact match",
        TRUE ~ "Partial match"
      )
    )
  }))
}

weekly_match_message <- function(coverage) {
  weekly <- coverage %>% filter(Granularity == "Weekly")
  if (nrow(weekly) == 0) {
    return("Weekly comparison unavailable.")
  }
  if (identical(weekly$Match_Status[1], "Exact match")) {
    return(paste("Weekly periods match exactly:", weekly$Common_Periods[1], "weeks."))
  }
  paste0(
    "Weekly periods do not fully match. Status: ", weekly$Match_Status[1],
    " | Common weeks: ", weekly$Common_Periods[1],
    " | Weeks only in previous: ", weekly$Missing_Current_Periods[1],
    " | Weeks only in current: ", weekly$Missing_Previous_Periods[1]
  )
}

comparison_metrics_table <- function(analysis, previous_report) {
  lower_is_better <- c("MAE", "RMSE", "MAPE (%)", "SMAPE (%)", "MASE")
  
  bind_rows(lapply(c("Daily", "Weekly", "Monthly"), function(granularity) {
    joined <- comparison_joined_series(analysis, previous_report, granularity)
    if (nrow(joined) < 2) {
      return(NULL)
    }
    
    current_metrics <- calculate_all_metrics(joined, pred_col = "Current_Pred")
    previous_metrics <- calculate_all_metrics(joined, pred_col = "Previous_Pred")
    
    tibble(
      Metric = names(current_metrics),
      Granularity = granularity,
      Current = as.numeric(unlist(current_metrics)),
      Previous = as.numeric(unlist(previous_metrics))
    )
  })) %>%
    mutate(
      Delta = Current - Previous,
      `% Delta` = if_else(!is.na(Previous) & Previous != 0, Delta / abs(Previous), NA_real_),
      Direction = case_when(
        Metric %in% lower_is_better & Delta < 0 ~ "Improved",
        Metric %in% lower_is_better & Delta > 0 ~ "Worse",
        !Metric %in% lower_is_better & Delta > 0 ~ "Improved",
        !Metric %in% lower_is_better & Delta < 0 ~ "Worse",
        TRUE ~ "Flat"
      )
    )
}

normalize_current_roi <- function(df) {
  if (is.null(df) || nrow(df) == 0 || "Message" %in% names(df)) {
    return(tibble(
      variable_key = character(),
      Variable = character(),
      Category = character(),
      `Sub-Category` = character(),
      Current_Units = numeric(),
      Current_Pct_Contribution = numeric(),
      Current_Spend = numeric(),
      Current_ROI = numeric()
    ))
  }
  
  df %>%
    transmute(
      variable_key = normalize_comparison_variable_key(.data[["Variable"]]),
      Variable = as.character(.data[["Variable"]]),
      Category = if ("Category" %in% names(df)) as.character(.data[["Category"]]) else NA_character_,
      `Sub-Category` = if ("Sub-Category" %in% names(df)) as.character(.data[["Sub-Category"]]) else NA_character_,
      Current_Units = if ("Units" %in% names(df)) suppressWarnings(as.numeric(.data[["Units"]])) else NA_real_,
      Current_Pct_Contribution = if ("% Contribution" %in% names(df)) suppressWarnings(as.numeric(.data[["% Contribution"]])) else NA_real_,
      Current_Spend = if ("Spend" %in% names(df)) suppressWarnings(as.numeric(.data[["Spend"]])) else NA_real_,
      Current_ROI = if ("ROI" %in% names(df)) suppressWarnings(as.numeric(.data[["ROI"]])) else NA_real_
    ) %>%
    distinct(variable_key, .keep_all = TRUE)
}

comparison_common_dates <- function(analysis, previous_report) {
  previous_dates <- as.Date(previous_report$series$Daily$Date)
  current_dates <- as.Date(analysis$df_med_original$Date)
  sort(intersect(current_dates, previous_dates))
}

previous_contribution_totals <- function(previous_report, common_dates = NULL) {
  df <- previous_report$contribution_daily
  empty_previous_contribution <- tibble(
    variable_key = character(),
    Variable = character(),
    Previous_Units = numeric(),
    Previous_Pct_Contribution = numeric(),
    Category = character(),
    `Sub-Category` = character()
  )
  
  if (is.null(df) || nrow(df) == 0) {
    return(empty_previous_contribution)
  }
  
  if (!is.null(common_dates)) {
    df <- df %>% filter(Date %in% common_dates)
  }
  
  contribution_cols <- setdiff(names(df), "Date")
  if (length(contribution_cols) == 0 || nrow(df) == 0) {
    return(empty_previous_contribution)
  }
  
  df %>%
    summarise(across(all_of(contribution_cols), ~ sum(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Previous_Units") %>%
    mutate(
      Variable = sub("^Contrib_", "", Variable),
      variable_key = normalize_comparison_variable_key(Variable)
    ) %>%
    group_by(variable_key) %>%
    summarise(
      Variable = first(Variable),
      Previous_Units = sum(Previous_Units, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Previous_Pct_Contribution = ifelse(
        sum(Previous_Units, na.rm = TRUE) != 0,
        Previous_Units / sum(Previous_Units, na.rm = TRUE) * 100,
        NA_real_
      ),
      mapping = lapply(Variable, get_channel_mapping),
      Category = sapply(mapping, `[[`, "category"),
      `Sub-Category` = sapply(mapping, `[[`, "sub_category")
    ) %>%
    select(-mapping)
}

current_comparison_source <- function(analysis, previous_report) {
  common_dates <- comparison_common_dates(analysis, previous_report)
  if (length(common_dates) == 0) {
    return(tibble())
  }
  
  df_med_source <- if (isTRUE(analysis$gradient_applied)) analysis$df_med else analysis$df_med_original
  df_med_common <- df_med_source %>% filter(Date %in% common_dates)
  df_input_common <- analysis$df_input %>% filter(Date %in% common_dates)
  
  build_roi_table(
    df_med_common,
    cftp_data = analysis$cftp_data,
    df_input_filtered = df_input_common,
    df_pct = NULL
  )
}

comparison_roi_table <- function(analysis, previous_report) {
  common_dates <- comparison_common_dates(analysis, previous_report)
  current_source <- current_comparison_source(analysis, previous_report)
  current <- normalize_current_roi(current_source)
  previous_units <- previous_contribution_totals(previous_report, common_dates)
  previous_roi <- previous_report$roi %>%
    select(variable_key, Previous_ROI, Previous_Spend)
  previous <- full_join(previous_units, previous_roi, by = "variable_key")
  
  if (nrow(current) == 0 && nrow(previous) == 0) {
    return(tibble(Message = "No ROI data available for comparison."))
  }
  
  full_join(current, previous, by = "variable_key", suffix = c("_Current", "_Previous")) %>%
    mutate(
      Variable = coalesce(Variable_Current, Variable_Previous),
      Category = coalesce(Category_Current, Category_Previous),
      `Sub-Category` = coalesce(`Sub-Category_Current`, `Sub-Category_Previous`),
      Pct_Delta_ROI = comparison_pct_delta(Current_ROI, Previous_ROI),
      sort_key = ifelse(Category == "Base", sort_order_map[["Base"]], coalesce(as.numeric(sort_order_map[`Sub-Category`]), sort_order_map[["Base"]] - 1))
    ) %>%
    arrange(sort_key, Category, `Sub-Category`, Variable) %>%
    select(
      Variable,
      Category,
      `Sub-Category`,
      `Current ROI` = Current_ROI,
      `Previous ROI` = Previous_ROI,
      `% Delta ROI` = Pct_Delta_ROI,
      `Current Units` = Current_Units,
      `Previous Units` = Previous_Units,
      `Current Spend` = Current_Spend,
      `Previous Spend` = Previous_Spend
    )
}

comparison_contribution_table <- function(analysis, previous_report) {
  common_dates <- comparison_common_dates(analysis, previous_report)
  current_source <- current_comparison_source(analysis, previous_report)
  current <- normalize_current_roi(current_source)
  previous <- previous_contribution_totals(previous_report, common_dates)
  if (nrow(current) == 0 && nrow(previous) == 0) {
    return(tibble(Message = "No contribution data available for comparison."))
  }
  
  full_join(current, previous, by = "variable_key", suffix = c("_Current", "_Previous")) %>%
    mutate(
      Category = coalesce(Category_Current, Category_Previous),
      `Sub-Category` = coalesce(`Sub-Category_Current`, `Sub-Category_Previous`),
      Current_Units = coalesce(Current_Units, 0),
      Previous_Units = coalesce(Previous_Units, 0),
      Current_Pct_Contribution = coalesce(Current_Pct_Contribution, 0),
      Previous_Pct_Contribution = coalesce(Previous_Pct_Contribution, 0)
    ) %>%
    group_by(Category, `Sub-Category`) %>%
    summarise(
      `Current Units` = sum(Current_Units, na.rm = TRUE),
      `Previous Units` = sum(Previous_Units, na.rm = TRUE),
      `Delta Units` = `Current Units` - `Previous Units`,
      `Current % Contribution` = sum(Current_Pct_Contribution, na.rm = TRUE),
      `Previous % Contribution` = sum(Previous_Pct_Contribution, na.rm = TRUE),
      `Delta % Contribution` = `Current % Contribution` - `Previous % Contribution`,
      .groups = "drop"
    ) %>%
    mutate(
      sort_key = ifelse(Category == "Base", sort_order_map[["Base"]], coalesce(as.numeric(sort_order_map[`Sub-Category`]), sort_order_map[["Base"]] - 1))
    ) %>%
    arrange(sort_key, Category, `Sub-Category`) %>%
    select(-sort_key)
}

comparison_pct_delta <- function(current, previous) {
  if_else(!is.na(previous) & previous != 0, (current - previous) / abs(previous), NA_real_)
}

comparison_variable_table <- function(comparison) {
  roi <- comparison$roi
  if (is.null(roi) || nrow(roi) == 0 || "Message" %in% names(roi)) {
    return(tibble(Message = "No variable comparison data available."))
  }
  
  roi %>%
    mutate(
      `Delta Units` = `Current Units` - `Previous Units`,
      `% Delta Units` = comparison_pct_delta(`Current Units`, `Previous Units`),
      `Comparison Status` = case_when(
        !is.na(`Current Units`) & is.na(`Previous Units`) ~ "Current Only",
        is.na(`Current Units`) & !is.na(`Previous Units`) ~ "Previous Only",
        is.na(`% Delta Units`) ~ "No % Delta",
        abs(`% Delta Units`) > 0.10 ~ "Difference > 10%",
        TRUE ~ "Within 10%"
      ),
      sort_key = ifelse(Category == "Base", sort_order_map[["Base"]], coalesce(as.numeric(sort_order_map[`Sub-Category`]), sort_order_map[["Base"]] - 1))
    ) %>%
    arrange(sort_key, Category, `Sub-Category`, Variable) %>%
    select(
      Variable,
      Category,
      `Sub-Category`,
      `Current Units`,
      `Previous Units`,
      `Delta Units`,
      `% Delta Units`,
      `Current ROI`,
      `Previous ROI`,
      `% Delta ROI`,
      `Comparison Status`
    )
}

build_previous_model_comparison <- function(analysis, previous_report) {
  if (is.null(previous_report)) {
    return(NULL)
  }
  
  comparison <- list(
    filename = previous_report$filename,
    coverage = comparison_coverage_table(analysis, previous_report),
    metrics = comparison_metrics_table(analysis, previous_report),
    roi = comparison_roi_table(analysis, previous_report),
    contribution = comparison_contribution_table(analysis, previous_report),
    series = setNames(
      lapply(c("Daily", "Weekly", "Monthly"), function(granularity) {
        comparison_joined_series(analysis, previous_report, granularity)
      }),
      c("Daily", "Weekly", "Monthly")
    )
  )
  comparison$weekly_message <- weekly_match_message(comparison$coverage)
  comparison$variable <- comparison_variable_table(comparison)
  comparison
}

empty_comparison_plot <- function(message = "No common periods available") {
  plot_ly(
    data = tibble(x = numeric(0), y = numeric(0)),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines"
  ) %>%
    layout(
      annotations = list(
        list(
          text = message,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 13, color = "#64748b")
        )
      ),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    ) %>%
    plotly_model_layout(top_margin = 24, show_legend = FALSE)
}

build_comparison_fit_plot <- function(comparison, granularity) {
  df <- comparison$series[[granularity]]
  if (is.null(df) || nrow(df) == 0) {
    return(empty_comparison_plot())
  }
  
  plot_ly(df, x = ~Date) %>%
    add_lines(y = ~Actual, name = "Actual", line = list(color = "#5B9BD5", width = 2)) %>%
    add_lines(y = ~Current_Pred, name = "Current Predicted", line = list(color = "#f39c12", width = 2)) %>%
    add_lines(y = ~Previous_Pred, name = "Previous Predicted", line = list(color = "#7E57C2", width = 2, dash = "dash")) %>%
    layout(xaxis = list(title = "Date"), yaxis = list(title = "Value")) %>%
    plotly_model_layout(top_margin = 42)
}

build_comparison_error_plot <- function(comparison, granularity) {
  df <- comparison$series[[granularity]]
  if (is.null(df) || nrow(df) == 0) {
    return(empty_comparison_plot())
  }
  
  plot_ly(df, x = ~Date) %>%
    add_lines(y = ~Current_Residual, name = "Current Residual", line = list(color = "#5B9BD5", width = 2)) %>%
    add_lines(y = ~Previous_Residual, name = "Previous Residual", line = list(color = "#7E57C2", width = 2, dash = "dash")) %>%
    layout(
      xaxis = list(title = "Date"),
      yaxis = list(title = "Actual - Predicted"),
      shapes = list(
        list(
          type = "line",
          x0 = min(df$Date, na.rm = TRUE),
          x1 = max(df$Date, na.rm = TRUE),
          y0 = 0,
          y1 = 0,
          line = list(color = "#94a3b8", width = 1)
        )
      )
    ) %>%
    plotly_model_layout(top_margin = 42)
}

write_comparison_table <- function(wb, sheet, title, data, start_row) {
  writeData(wb, sheet, title, startRow = start_row, startCol = 1)
  addStyle(
    wb, sheet,
    createStyle(textDecoration = "bold", fontSize = 13, fontColour = "#1e293b"),
    rows = start_row, cols = 1, gridExpand = TRUE
  )
  
  if (is.null(data) || nrow(data) == 0) {
    writeData(wb, sheet, data.frame(Message = "No data available."), startRow = start_row + 1)
    apply_header_style(wb, sheet, start_row + 1, 1)
    return(start_row + 4)
  }
  
  writeData(wb, sheet, round_numeric_columns(data, 3), startRow = start_row + 1)
  apply_header_style(wb, sheet, start_row + 1, 1:ncol(data))
  stripe_rows(wb, sheet, nrow(data), start_row + 1, ncol(data))
  start_row + nrow(data) + 4
}

add_model_comparison_sheet <- function(wb, comparison) {
  if (is.null(comparison)) {
    return(invisible(wb))
  }
  
  sheet <- "Model Comparison"
  addWorksheet(wb, sheet)
  writeData(wb, sheet, paste("Model Comparison - Previous:", comparison$filename), startRow = 1, startCol = 1)
  addStyle(
    wb, sheet,
    createStyle(textDecoration = "bold", fontSize = 15, fontColour = "#1e293b"),
    rows = 1, cols = 1, gridExpand = TRUE
  )
  
  writeData(wb, sheet, comparison$weekly_message, startRow = 2, startCol = 1)
  addStyle(
    wb, sheet,
    createStyle(fontColour = if (grepl("do not fully match|unavailable", comparison$weekly_message)) "#C00000" else "#2fb344", textDecoration = "bold"),
    rows = 2, cols = 1, gridExpand = TRUE
  )
  
  chart_granularity <- comparison$coverage %>%
    filter(Common_Periods > 0) %>%
    arrange(match(Granularity, c("Weekly", "Monthly", "Daily"))) %>%
    pull(Granularity) %>%
    first()
  if (is.na(chart_granularity) || length(chart_granularity) == 0) {
    chart_granularity <- "Weekly"
  }
  
  next_row <- 4
  metric_summary <- comparison$metrics %>%
    filter(Granularity == chart_granularity)
  next_row <- write_comparison_table(wb, sheet, "Metric Summary", metric_summary, next_row)
  next_row <- write_comparison_table(wb, sheet, "Variable Comparison", comparison$variable, next_row)

  setColWidths(wb, sheet, cols = 1:20, widths = c(rep(18, 11), rep(16, 9)))
  invisible(wb)
}
