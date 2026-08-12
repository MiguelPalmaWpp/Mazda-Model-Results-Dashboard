read_table_sheet <- function(filepath, sheet = 1) {
  ext <- tolower(tools::file_ext(filepath))

  if (ext %in% c("xlsx", "xlsm", "xls")) {
    return(openxlsx::read.xlsx(filepath, sheet = sheet, detectDates = TRUE, check.names = FALSE) %>%
             dplyr::as_tibble())
  }

  if (ext == "csv") {
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
  if (length(missing_cols) > 0) {
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

  if (!is.null(nameplate) && nzchar(nameplate)) {
    df_cftp <- df_cftp %>% filter(Final_Nameplate == nameplate)
  }

  df_cftp <- df_cftp %>%
    group_by(Month, Final_Nameplate) %>%
    summarise(AVG_CFTP = mean(AVG_CFTP, na.rm = TRUE), .groups = "drop") %>%
    arrange(Final_Nameplate, Month)

  if (!is.null(nameplate) && nzchar(nameplate) && nrow(df_cftp) == 0) {
    stop("No valid CFTP rows found for Final_Nameplate: ", nameplate)
  }

  sheet_name <- if (tolower(tools::file_ext(filepath)) %in% c("xlsx", "xlsm", "xls")) {
    if (is.numeric(sheet)) openxlsx::getSheetNames(filepath)[sheet] else sheet
  } else {
    "csv"
  }

  cat("CFTP file loaded - sheet:", sheet_name, "- rows:", nrow(df_cftp), "\n")
  if (!is.null(nameplate) && nzchar(nameplate)) {
    cat("CFTP nameplate selected:", nameplate, "\n")
  }
  if (nrow(df_cftp) > 0) {
    cat("CFTP months:", paste(format(sort(unique(df_cftp$Month)), "%Y-%m"), collapse = ", "), "\n")
  }

  df_cftp
}

cftp_nameplate_choices <- function(filepath, sheet = 1) {
  df_cftp <- load_cftp(filepath, sheet = sheet, nameplate = NULL)
  sort(unique(df_cftp$Final_Nameplate))
}

missing_cftp_months <- function(df_med_input, cftp_data) {
  if (is.null(cftp_data) || nrow(cftp_data) == 0 || nrow(df_med_input) == 0) {
    return(character(0))
  }

  med_months <- sort(unique(as.Date(floor_date(df_med_input$Date, "month"))))
  cftp_months <- sort(unique(cftp_data$Month))
  format(med_months[!med_months %in% cftp_months], "%Y-%m")
}

calculate_cftp_revenue <- function(df_med_input, contrib_cols, cftp_data) {
  if (is.null(cftp_data) || nrow(cftp_data) == 0) {
    return(list(
      revenue_lookup = setNames(numeric(0), character(0)),
      covered_lookup = setNames(logical(0), character(0)),
      missing_months = character(0)
    ))
  }

  missing_months <- missing_cftp_months(df_med_input, cftp_data)

  if (length(missing_months) > 0) {
    cat("  WARNING - CFTP missing for months:",
        paste(missing_months, collapse = ", "), "\n")
  }

  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::as.data.table(df_med_input[, c("Date", contrib_cols), drop = FALSE])
    dt[, Month := as.Date(lubridate::floor_date(Date, "month"))]
    cftp_dt <- data.table::as.data.table(cftp_data[, c("Month", "AVG_CFTP"), drop = FALSE])
    long_dt <- data.table::melt(
      dt,
      id.vars = "Month",
      measure.vars = contrib_cols,
      variable.name = "Variable",
      value.name = "Daily_Units"
    )
    long_dt[cftp_dt, AVG_CFTP := i.AVG_CFTP, on = "Month"]
    revenue_by_var <- long_dt[, .(
      Revenue = if (any(!is.na(AVG_CFTP) & Daily_Units > 0)) {
        sum(data.table::fifelse(Daily_Units > 0, Daily_Units * AVG_CFTP, NA_real_), na.rm = TRUE)
      } else {
        NA_real_
      },
      Has_CFTP_Revenue = any(!is.na(AVG_CFTP) & Daily_Units > 0)
    ), by = Variable] %>%
      as_tibble()
    
    return(list(
      revenue_lookup = setNames(revenue_by_var$Revenue, revenue_by_var$Variable),
      covered_lookup = setNames(revenue_by_var$Has_CFTP_Revenue, revenue_by_var$Variable),
      missing_months = missing_months
    ))
  }
  
  revenue_by_var <- df_med_input %>%
    mutate(Month = as.Date(floor_date(Date, "month"))) %>%
    select(Date, Month, all_of(contrib_cols)) %>%
    pivot_longer(all_of(contrib_cols), names_to = "Variable", values_to = "Daily_Units") %>%
    left_join(cftp_data %>% select(Month, AVG_CFTP), by = "Month") %>%
    group_by(Variable) %>%
    summarise(
      Revenue = if (any(!is.na(AVG_CFTP) & Daily_Units > 0)) {
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
