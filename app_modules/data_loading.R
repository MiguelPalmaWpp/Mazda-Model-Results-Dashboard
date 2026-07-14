round_numeric_columns <- function(data, digits = 3) {
  if (is.null(data) || !is.data.frame(data)) {
    return(data)
  }

  data %>%
    mutate(across(where(is.numeric), ~ round(.x, digits)))
}

read_uploaded_table <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xlsm", "xls")) {
    return(openxlsx::read.xlsx(path, detectDates = TRUE, check.names = FALSE) %>%
             dplyr::as_tibble())
  }

  if (ext == "csv") {
    return(readr::read_csv(path, show_col_types = FALSE, progress = FALSE) %>%
             dplyr::as_tibble())
  }

  stop("Unsupported file type: ", ext)
}

clean_date_table <- function(df, file_label) {
  bad_cols <- grepl("^X$|^X\\.\\d+$|^\\.\\.\\.\\d+$|^$", colnames(df))
  df <- df[, !bad_cols, drop = FALSE]

  date_idx <- which(tolower(colnames(df)) == "date")
  if (length(date_idx) == 0) {
    stop(file_label, " must include a Date column.")
  }

  colnames(df)[date_idx[1]] <- "Date"
  df$Date <- as.Date(df$Date)
  dplyr::as_tibble(df)
}

read_pct_contribution <- function(path) {
  ext <- tolower(tools::file_ext(path))

  df <- if (ext %in% c("xlsx", "xlsm", "xls")) {
    openxlsx::read.xlsx(path, colNames = FALSE, check.names = FALSE)
  } else if (ext == "csv") {
    read.csv(path, header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Unsupported percentage contribution file type: ", ext)
  }

  if (ncol(df) < 2) {
    stop("Contribution Percentages must have at least two columns: Variable and Pct.")
  }

  df[, 1:2, drop = FALSE] %>%
    setNames(c("Variable", "Pct")) %>%
    dplyr::as_tibble() %>%
    mutate(Pct = suppressWarnings(as.numeric(Pct))) %>%
    filter(!is.na(Variable), Variable != "")
}

load_model_data <- function(data_input_path, med_contrib_path, pct_contrib_path) {
  df_actual <- read_uploaded_table(data_input_path) %>%
    clean_date_table("MFF / Data Input")

  df_med <- read_uploaded_table(med_contrib_path) %>%
    clean_date_table("Contributions")

  df_pct <- read_pct_contribution(pct_contrib_path)

  kpi_candidates <- setdiff(colnames(df_actual), "Date")
  kpi_cols <- kpi_candidates[grepl("KPI", kpi_candidates, ignore.case = TRUE)]

  if (length(kpi_cols) >= 1) {
    kpi_col <- kpi_cols[1]
  } else if ("Actual" %in% kpi_candidates) {
    kpi_col <- "Actual"
  } else if (length(kpi_candidates) == 1) {
    kpi_col <- kpi_candidates[1]
  } else {
    stop(
      "Could not auto-detect the KPI column. Rename the target column to include 'KPI' ",
      "or 'Actual'. Available columns: ", paste(kpi_candidates, collapse = ", ")
    )
  }

  df_actual <- df_actual %>% rename(Actual = !!sym(kpi_col))

  pred_col <- if ("Pred" %in% colnames(df_med)) {
    "Pred"
  } else {
    colnames(df_med)[grepl("^pred$", colnames(df_med), ignore.case = TRUE)][1]
  }

  if (is.na(pred_col) || length(pred_col) == 0) {
    stop("Contributions must include a Pred column.")
  }

  contrib_cols <- colnames(df_med)[grepl("^Contrib_", colnames(df_med))]
  if (length(contrib_cols) == 0) {
    stop("Contributions must include at least one Contrib_ column.")
  }

  df <- inner_join(
    df_actual %>% select(Date, Actual),
    df_med %>% select(Date, Pred = !!sym(pred_col)),
    by = "Date"
  ) %>% arrange(Date)

  if (nrow(df) == 0) {
    stop("No matching dates were found between MFF / Data Input and Contributions.")
  }

  list(
    df = df,
    df_med = df_med,
    df_pct = df_pct,
    df_input = df_actual,
    diagnostics = list(
      kpi_column = kpi_col,
      pred_column = pred_col,
      spend_columns = setdiff(colnames(df_actual), c("Date", "Actual")),
      contribution_columns = contrib_cols,
      date_range = range(df$Date, na.rm = TRUE)
    )
  )
}

detect_uploaded_files <- function(files) {
  if (is.null(files) || nrow(files) == 0) {
    return(list(data_input = NULL, med_contrib = NULL, pct_contrib = NULL, diagnostics = character()))
  }

  names_lower <- tolower(files$name)
  pick <- function(pattern) {
    idx <- which(grepl(pattern, names_lower))
    if (length(idx) == 0) NULL else files[idx[1], , drop = FALSE]
  }

  pct <- pick("pct|percent|percentage")
  med <- pick("med_contrib|contribution|contrib")
  data_input <- pick("data_input|mff|input")

  if (!is.null(pct) && !is.null(med) && pct$name == med$name) {
    remaining <- files[files$name != pct$name, , drop = FALSE]
    idx <- which(grepl("med_contrib|contribution|contrib", tolower(remaining$name)))
    med <- if (length(idx) == 0) NULL else remaining[idx[1], , drop = FALSE]
  }

  diagnostics <- c(
    paste("MFF / Data Input:", if (is.null(data_input)) "not detected" else data_input$name),
    paste("Contributions:", if (is.null(med)) "not detected" else med$name),
    paste("Contribution Percentages:", if (is.null(pct)) "not detected" else pct$name)
  )

  list(data_input = data_input, med_contrib = med, pct_contrib = pct, diagnostics = diagnostics)
}
