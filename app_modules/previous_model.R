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
  x_num <- suppressWarnings(as.numeric(x_chr))
  is_excel_serial <- !is.na(x_num) & x_num >= 20000 & x_num <= 60000
  parsed_from_serial <- rep(as.Date(NA), length(x_chr))
  parsed_from_serial[is_excel_serial] <- as.Date(x_num[is_excel_serial], origin = "1899-12-30")
  
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x_chr,
    orders = c("ymd", "dmy", "mdy", "Ymd HMS", "dmY HMS", "mdY HMS"),
    exact = FALSE
  ))
  coalesce(parsed_from_serial, as.Date(parsed))
}

read_sheet_if_exists <- function(filepath, sheet) {
  sheets <- previous_excel_sheets(filepath)
  matched <- sheets[tolower(sheets) == tolower(sheet)]
  if (length(matched) == 0) {
    return(NULL)
  }
  openxlsx::read.xlsx(filepath, sheet = matched[1], colNames = FALSE, detectDates = FALSE)
}

previous_excel_sheets <- function(filepath) {
  if (tolower(tools::file_ext(filepath)) == "csv") {
    return("CSV")
  }
  if (requireNamespace("readxl", quietly = TRUE)) {
    return(readxl::excel_sheets(filepath))
  }
  openxlsx::getSheetNames(filepath)
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
  sheets <- previous_excel_sheets(filepath)
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

previous_long_format_sheet_default <- function(filepath) {
  if (tolower(tools::file_ext(filepath)) == "csv") {
    return("CSV")
  }
  sheets <- previous_excel_sheets(filepath)
  sheet_key <- normalize_mapping_key(sheets)
  long_idx <- match("long_format", sheet_key)
  if (is.na(long_idx)) {
    long_idx <- match("long_format_contributions", sheet_key)
  }
  if (is.na(long_idx)) {
    long_idx <- grep("long.*format|long.*contribution", sheet_key)[1]
  }
  if (is.na(long_idx) || length(long_idx) == 0) {
    long_idx <- 1
  }
  sheets[[long_idx]]
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

previous_col_by_key <- function(cols, key) {
  keys <- normalize_mapping_key(cols)
  matched <- which(keys == key)
  if (length(matched) == 0) {
    return(NA_character_)
  }
  cols[[matched[1]]]
}

previous_sheet_header <- function(filepath, sheet) {
  if (tolower(tools::file_ext(filepath)) == "csv") {
    header <- readr::read_csv(
      filepath,
      n_max = 0,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal"
    )
    return(trimws(names(header)))
  }
  
  if (requireNamespace("readxl", quietly = TRUE)) {
    header <- readxl::read_excel(filepath, sheet = sheet, n_max = 0, .name_repair = "minimal")
    return(trimws(names(header)))
  }
  
  header <- openxlsx::read.xlsx(
    filepath,
    sheet = sheet,
    rows = 1,
    colNames = FALSE,
    detectDates = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  trimws(as.character(unlist(header[1, ], use.names = FALSE)))
}

read_previous_excel_cols <- function(filepath, sheet, cols) {
  cols <- sort(unique(as.integer(cols[!is.na(cols)])))
  if (length(cols) == 0) {
    return(tibble())
  }
  
  if (tolower(tools::file_ext(filepath)) == "csv") {
    header <- previous_sheet_header(filepath, sheet)
    selected_names <- header[cols]
    df <- readr::read_csv(
      filepath,
      col_select = all_of(selected_names),
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal"
    ) %>%
      as_tibble()
    names(df) <- trimws(names(df))
    return(df)
  }
  
  if (requireNamespace("readxl", quietly = TRUE) && requireNamespace("cellranger", quietly = TRUE)) {
    min_col <- min(cols)
    max_col <- max(cols)
    df <- readxl::read_excel(
      filepath,
      sheet = sheet,
      range = cellranger::cell_cols(min_col:max_col),
      col_types = "text",
      .name_repair = "minimal"
    ) %>%
      as_tibble()
    names(df) <- trimws(names(df))
    return(df)
  }
  
  df <- openxlsx::read.xlsx(
    filepath,
    sheet = sheet,
    colNames = TRUE,
    cols = cols,
    detectDates = FALSE,
    skipEmptyRows = TRUE,
    check.names = FALSE
  ) %>%
    as_tibble()
  names(df) <- trimws(names(df))
  df
}

previous_col_index_by_key <- function(cols, key) {
  keys <- normalize_mapping_key(cols)
  matched <- which(keys == key)
  if (length(matched) == 0) {
    return(NA_integer_)
  }
  matched[1]
}

previous_long_format_nameplate_choices <- function(filepath, sheet) {
  header <- previous_sheet_header(filepath, sheet)
  nameplate_idx <- previous_col_index_by_key(header, "nameplate")
  if (is.na(nameplate_idx)) {
    return(character(0))
  }
  
  df <- read_previous_excel_cols(filepath, sheet, nameplate_idx)
  
  nameplate_col <- previous_col_by_key(names(df), "nameplate")
  if (is.na(nameplate_col)) {
    return(character(0))
  }
  
  choices <- sort(unique(trimws(as.character(df[[nameplate_col]]))))
  choices[!is.na(choices) & nzchar(choices)]
}

parse_previous_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub("[,$ ]", "", as.character(x))))
}

previous_long_format_totals <- function(df_long, common_dates = NULL) {
  empty_totals <- tibble(
    variable_key = character(),
    Variable = character(),
    Category = character(),
    `Sub-Category` = character(),
    Previous_Units = numeric(),
    Previous_Pct_Contribution = numeric(),
    Previous_Spend = numeric(),
    Previous_Revenue = numeric(),
    Previous_ROI = numeric()
  )
  
  if (is.null(df_long) || nrow(df_long) == 0) {
    return(empty_totals)
  }
  
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::as.data.table(df_long)
    if (!is.null(common_dates)) {
      dt <- dt[Date %in% as.Date(common_dates)]
    }
    if (nrow(dt) == 0) {
      return(empty_totals)
    }
    totals <- dt[, .(
      Variable = data.table::first(Variable),
      Category = {
        x <- Category[!is.na(Category) & nzchar(Category)]
        if (length(x) > 0) x[1] else NA_character_
      },
      `Sub-Category` = {
        x <- `Sub-Category`[!is.na(`Sub-Category`) & nzchar(`Sub-Category`)]
        if (length(x) > 0) x[1] else NA_character_
      },
      Previous_Units = sum(Previous_Contribution, na.rm = TRUE),
      Previous_Spend = sum(Previous_Spend, na.rm = TRUE),
      Previous_Revenue = sum(Previous_Revenue, na.rm = TRUE)
    ), by = variable_key] %>%
      as_tibble()
    
    total_units <- sum(totals$Previous_Units, na.rm = TRUE)
    return(totals %>%
      mutate(
        Previous_Pct_Contribution = ifelse(total_units != 0, Previous_Units / total_units * 100, NA_real_),
        Previous_ROI = ifelse(Previous_Units > 0 & Previous_Spend != 0, Previous_Revenue / Previous_Spend, NA_real_)
      ))
  }
  
  if (!is.null(common_dates)) {
    df_long <- df_long %>% filter(Date %in% common_dates)
  }
  if (nrow(df_long) == 0) {
    return(empty_totals)
  }
  
  df_long %>%
    group_by(variable_key) %>%
    summarise(
      Variable = first(Variable),
      Category = first(na.omit(Category)) %||% NA_character_,
      `Sub-Category` = first(na.omit(`Sub-Category`)) %||% NA_character_,
      Previous_Units = sum(Previous_Contribution, na.rm = TRUE),
      Previous_Spend = sum(Previous_Spend, na.rm = TRUE),
      Previous_Revenue = sum(Previous_Revenue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Previous_Pct_Contribution = ifelse(
        sum(Previous_Units, na.rm = TRUE) != 0,
        Previous_Units / sum(Previous_Units, na.rm = TRUE) * 100,
        NA_real_
      ),
      Previous_ROI = ifelse(Previous_Units > 0 & Previous_Spend != 0, Previous_Revenue / Previous_Spend, NA_real_)
    )
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

read_previous_long_format_sheet <- function(filepath, sheet, nameplate = NULL) {
  header <- previous_sheet_header(filepath, sheet)
  required_keys <- c("date", "nameplate", "variable", "contribution", "spend", "revenue", "category", "sub_category", "funnel", "channel")
  optional_keys <- c("contribution_gradient", "avg_mrsp", "category_v2")
  required_idx <- setNames(vapply(required_keys, function(key) previous_col_index_by_key(header, key), integer(1)), required_keys)
  optional_idx <- setNames(vapply(optional_keys, function(key) previous_col_index_by_key(header, key), integer(1)), optional_keys)
  missing_cols <- names(required_idx)[is.na(required_idx)]
  if (length(missing_cols) > 0) {
    stop("Previous long format sheet is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  selected_nameplate <- if (!is.null(nameplate) && nzchar(nameplate)) trimws(as.character(nameplate)) else NA_character_
  if (is.na(selected_nameplate) || !nzchar(selected_nameplate)) {
    stop("Select a Previous Nameplate before loading the previous long format report.")
  }
  
  selected_cols <- sort(unique(c(as.integer(required_idx), as.integer(optional_idx[!is.na(optional_idx)]))))
  df <- read_previous_excel_cols(filepath, sheet, selected_cols)
  
  col_lookup <- setNames(vapply(required_keys, function(key) previous_col_by_key(names(df), key), character(1)), required_keys)
  gradient_col <- previous_col_by_key(names(df), "contribution_gradient")
  avg_mrsp_col <- previous_col_by_key(names(df), "avg_mrsp")
  nameplate_col <- previous_col_by_key(names(df), "nameplate")
  category_v2_col <- previous_col_by_key(names(df), "category_v2")
  
  df <- df %>%
    filter(trimws(as.character(.data[[nameplate_col]])) == selected_nameplate)
  
  if (nrow(df) == 0) {
    stop("No rows found in previous long format for Nameplate: ", selected_nameplate)
  }
  
  contribution_values <- parse_previous_numeric(df[[col_lookup[["contribution"]]]])
  gradient_values <- if (!is.na(gradient_col)) parse_previous_numeric(df[[gradient_col]]) else rep(NA_real_, nrow(df))
  previous_contribution <- contribution_values
  
  avg_mrsp_values <- if (!is.na(avg_mrsp_col)) parse_previous_numeric(df[[avg_mrsp_col]]) else rep(NA_real_, nrow(df))
  revenue_values <- parse_previous_numeric(df[[col_lookup[["revenue"]]]])
  revenue_values <- coalesce(
    revenue_values,
    ifelse(!is.na(avg_mrsp_values) & previous_contribution > 0, previous_contribution * avg_mrsp_values, NA_real_)
  )
  
  df_long <- tibble(
    Date = parse_report_date(df[[col_lookup[["date"]]]]),
    Variable = sub("^Contrib_", "", trimws(as.character(df[[col_lookup[["variable"]]]]))),
    Previous_Contribution = previous_contribution,
    Previous_Spend = parse_previous_numeric(df[[col_lookup[["spend"]]]]),
    Previous_Revenue = revenue_values,
    Previous_Contribution_Gradient = gradient_values,
    Category = trimws(as.character(df[[col_lookup[["category"]]]])),
    `Sub-Category` = trimws(as.character(df[[col_lookup[["sub_category"]]]])),
    Funnel = trimws(as.character(df[[col_lookup[["funnel"]]]])),
    Channel = trimws(as.character(df[[col_lookup[["channel"]]]])),
    Nameplate = if (!is.na(nameplate_col)) trimws(as.character(df[[nameplate_col]])) else NA_character_,
    `Category V2` = if (!is.na(category_v2_col)) trimws(as.character(df[[category_v2_col]])) else NA_character_
  ) %>%
    filter(!is.na(Date), !is.na(Variable), Variable != "") %>%
    mutate(
      Previous_Contribution = replace_na(Previous_Contribution, 0),
      Previous_Spend = replace_na(Previous_Spend, 0),
      variable_key = normalize_comparison_variable_key(Variable)
    )
  
  if (nrow(df_long) == 0) {
    stop("Previous long format sheet has no usable rows after parsing Date and Variable.")
  }
  
  variable_totals <- previous_long_format_totals(df_long)
  if (requireNamespace("data.table", quietly = TRUE)) {
    daily <- data.table::as.data.table(df_long)[, .(
      Previous_Pred = sum(Previous_Contribution, na.rm = TRUE)
    ), by = Date] %>%
      as_tibble() %>%
      arrange(Date)
  } else {
    daily <- df_long %>%
      group_by(Date) %>%
      summarise(Previous_Pred = sum(Previous_Contribution, na.rm = TRUE), .groups = "drop") %>%
      arrange(Date)
  }
  
  list(
    daily = daily,
    contribution_daily = tibble(Date = sort(unique(df_long$Date))),
    long_format = df_long,
    contribution = variable_totals %>%
      select(variable_key, Variable, Previous_Units, Previous_Pct_Contribution, Category, `Sub-Category`),
    roi = variable_totals %>%
      select(variable_key, Variable, Category, `Sub-Category`, Previous_Units, Previous_Pct_Contribution, Previous_Spend, Previous_ROI),
    nameplate = if (!is.na(selected_nameplate)) selected_nameplate else NA_character_,
    prediction_source = "sum of Contribution by Date"
  )
}

aggregate_previous_series <- function(df, freq, method = "sum", week_start = 7) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::as.data.table(df)
    dt[, Period := if (freq == "week") {
      lubridate::floor_date(Date, freq, week_start = week_start)
    } else {
      lubridate::floor_date(Date, freq)
    }]
    out <- if (identical(method, "mean")) {
      dt[, .(Previous_Pred = mean(Previous_Pred, na.rm = TRUE)), by = Period]
    } else {
      dt[, .(Previous_Pred = sum(Previous_Pred, na.rm = TRUE)), by = Period]
    }
    return(out %>%
             as_tibble() %>%
             rename(Date = Period) %>%
             arrange(Date))
  }
  
  agg_fn <- if (identical(method, "mean")) mean else sum
  df %>%
    mutate(Period = if (freq == "week") {
      floor_date(Date, freq, week_start = week_start)
    } else {
      floor_date(Date, freq)
    }) %>%
    group_by(Period) %>%
    summarise(Previous_Pred = agg_fn(Previous_Pred, na.rm = TRUE), .groups = "drop") %>%
    rename(Date = Period) %>%
    arrange(Date)
}

previous_series_from_contributions <- function(daily, aggregation_method = "sum", week_start = 7) {
  list(
    Daily = daily,
    Weekly = aggregate_previous_series(daily, "week", aggregation_method, week_start = week_start),
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

load_previous_model_report <- function(filepath, mode = "excel_report",
                                       contribution_sheet = NULL, roi_sheet = NULL,
                                       long_format_sheet = NULL,
                                       long_format_nameplate = NULL,
                                       aggregation_method = "sum",
                                       weekly_grouping = "forward_from_sunday") {
  mode <- mode %||% "excel_report"
  week_start <- if (identical(weekly_grouping, "backward_to_sunday")) 1 else 7
  
  if (identical(mode, "long_format")) {
    long_format_sheet <- long_format_sheet %||% previous_long_format_sheet_default(filepath)
    long_format_data <- read_previous_long_format_sheet(filepath, long_format_sheet, nameplate = long_format_nameplate)
    
    return(list(
      filepath = filepath,
      filename = basename(filepath),
      mode = "Long Format",
      contribution_sheet = long_format_sheet,
      roi_sheet = NA_character_,
      long_format_sheet = long_format_sheet,
      long_format_nameplate = long_format_data$nameplate,
      prediction_source = long_format_data$prediction_source,
      series = previous_series_from_contributions(long_format_data$daily, aggregation_method, week_start = week_start),
      roi = long_format_data$roi,
      contribution_daily = long_format_data$contribution_daily,
      long_format = long_format_data$long_format,
      contribution = long_format_data$contribution
    ))
  }
  
  defaults <- previous_sheet_defaults(filepath)
  contribution_sheet <- contribution_sheet %||% defaults$contribution_sheet
  roi_sheet <- roi_sheet %||% defaults$roi_sheet
  contribution_data <- read_previous_contribution_sheet(filepath, contribution_sheet)
  
  list(
    filepath = filepath,
    filename = basename(filepath),
    mode = "Excel Report",
    contribution_sheet = contribution_sheet,
    roi_sheet = roi_sheet,
    long_format_sheet = NA_character_,
    long_format_nameplate = NA_character_,
    prediction_source = contribution_data$prediction_source,
    series = previous_series_from_contributions(contribution_data$daily, aggregation_method, week_start = week_start),
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
  common_dates <- intersect(as.numeric(current_dates), as.numeric(previous_dates))
  sort(as.Date(common_dates, origin = "1970-01-01"))
}

comparison_common_period_message <- function(common_dates) {
  if (length(common_dates) == 0) {
    return("Common period: No overlapping daily dates.")
  }
  paste0(
    "Common period: ", format_app_date(min(common_dates)), " to ", format_app_date(max(common_dates)),
    " | ", length(common_dates), " common days"
  )
}

previous_contribution_totals <- function(previous_report, common_dates = NULL) {
  if (identical(previous_report$mode, "Long Format") && !is.null(previous_report$long_format)) {
    return(previous_long_format_totals(previous_report$long_format, common_dates) %>%
             select(variable_key, Variable, Previous_Units, Previous_Pct_Contribution, Category, `Sub-Category`))
  }
  
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

current_comparison_source <- function(analysis, previous_report = NULL, common_dates = NULL) {
  if (is.null(common_dates)) {
    common_dates <- comparison_common_dates(analysis, previous_report)
  }
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

comparison_roi_table <- function(analysis, previous_report, common_dates = NULL,
                                 current = NULL, previous = NULL) {
  if (is.null(common_dates)) {
    common_dates <- comparison_common_dates(analysis, previous_report)
  }
  if (is.null(current)) {
    current <- normalize_current_roi(current_comparison_source(analysis, previous_report, common_dates))
  }
  if (is.null(previous)) {
    previous_units <- previous_contribution_totals(previous_report, common_dates)
    if (identical(previous_report$mode, "Long Format") && !is.null(previous_report$long_format)) {
      previous <- previous_long_format_totals(previous_report$long_format, common_dates)
    } else {
      previous_roi <- previous_report$roi %>%
        select(variable_key, Previous_ROI, Previous_Spend)
      previous <- full_join(previous_units, previous_roi, by = "variable_key")
    }
  }
  
  if (nrow(current) == 0 && nrow(previous) == 0) {
    return(tibble(Message = "No ROI data available for comparison."))
  }
  
  full_join(current, previous, by = "variable_key", suffix = c("_Current", "_Previous")) %>%
    mutate(
      Variable = coalesce(Variable_Current, Variable_Previous),
      Category = coalesce(Category_Current, Category_Previous),
      `Sub-Category` = coalesce(`Sub-Category_Current`, `Sub-Category_Previous`),
      Pct_Delta_ROI = comparison_pct_delta(Current_ROI, Previous_ROI),
      Pct_Delta_Spend = comparison_pct_delta(Current_Spend, Previous_Spend),
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
      `Previous Spend` = Previous_Spend,
      `% Delta Spend` = Pct_Delta_Spend
    )
}

comparison_contribution_table <- function(analysis, previous_report, common_dates = NULL,
                                          current = NULL, previous = NULL) {
  if (is.null(common_dates)) {
    common_dates <- comparison_common_dates(analysis, previous_report)
  }
  if (is.null(current)) {
    current <- normalize_current_roi(current_comparison_source(analysis, previous_report, common_dates))
  }
  if (is.null(previous)) {
    previous <- previous_contribution_totals(previous_report, common_dates)
  }
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
      `% Delta Units`,
      `Current Spend`,
      `Previous Spend`,
      `% Delta Spend`,
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
  
  common_dates <- comparison_common_dates(analysis, previous_report)
  current <- normalize_current_roi(current_comparison_source(analysis, previous_report, common_dates))
  previous_roi <- if (identical(previous_report$mode, "Long Format") && !is.null(previous_report$long_format)) {
    previous_long_format_totals(previous_report$long_format, common_dates)
  } else {
    previous_units <- previous_contribution_totals(previous_report, common_dates)
    full_join(
      previous_units,
      previous_report$roi %>% select(variable_key, Previous_ROI, Previous_Spend),
      by = "variable_key"
    )
  }
  previous_units <- previous_roi %>%
    select(variable_key, Variable, Previous_Units, Previous_Pct_Contribution, Category, `Sub-Category`)
  
  comparison <- list(
    filename = previous_report$filename,
    mode = previous_report$mode %||% "Excel Report",
    common_period_message = comparison_common_period_message(common_dates),
    coverage = comparison_coverage_table(analysis, previous_report),
    metrics = comparison_metrics_table(analysis, previous_report),
    roi = comparison_roi_table(analysis, previous_report, common_dates, current, previous_roi),
    contribution = comparison_contribution_table(analysis, previous_report, common_dates, current, previous_units),
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
    add_trace(y = ~Actual, name = "Actual", type = "scatter", mode = "lines", line = list(color = "#5B9BD5", width = 2)) %>%
    add_trace(y = ~Current_Pred, name = "Current Predicted", type = "scatter", mode = "lines", line = list(color = "#f39c12", width = 2)) %>%
    add_trace(y = ~Previous_Pred, name = "Previous Predicted", type = "scatter", mode = "lines", line = list(color = "#7E57C2", width = 2, dash = "dash")) %>%
    layout(xaxis = list(title = "Date"), yaxis = list(title = "Value")) %>%
    plotly_model_layout(top_margin = 42)
}

build_comparison_error_plot <- function(comparison, granularity) {
  df <- comparison$series[[granularity]]
  if (is.null(df) || nrow(df) == 0) {
    return(empty_comparison_plot())
  }
  
  plot_ly(df, x = ~Date) %>%
    add_trace(y = ~Current_Residual, name = "Current Residual", type = "scatter", mode = "lines", line = list(color = "#5B9BD5", width = 2)) %>%
    add_trace(y = ~Previous_Residual, name = "Previous Residual", type = "scatter", mode = "lines", line = list(color = "#7E57C2", width = 2, dash = "dash")) %>%
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
  writeData(wb, sheet, paste("Mode:", comparison$mode, "|", comparison$common_period_message), startRow = 3, startCol = 1)
  
  chart_granularity <- comparison$coverage %>%
    filter(Common_Periods > 0) %>%
    arrange(match(Granularity, c("Weekly", "Monthly", "Daily"))) %>%
    pull(Granularity) %>%
    first()
  if (is.na(chart_granularity) || length(chart_granularity) == 0) {
    chart_granularity <- "Weekly"
  }
  
  next_row <- 5
  metric_summary <- comparison$metrics %>%
    filter(Granularity == chart_granularity)
  next_row <- write_comparison_table(wb, sheet, "Metric Summary", metric_summary, next_row)
  next_row <- write_comparison_table(wb, sheet, "Variable Comparison", comparison$variable, next_row)

  setColWidths(wb, sheet, cols = 1:20, widths = c(rep(18, 11), rep(16, 9)))
  invisible(wb)
}
