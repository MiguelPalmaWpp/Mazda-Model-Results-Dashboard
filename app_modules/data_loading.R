`%||%` <- function(x, y) if (is.null(x)) y else x

round_numeric_columns <- function(data, digits = 3) {
  if (is.null(data) || !is.data.frame(data)) {
    return(data)
  }

  data %>%
    mutate(across(where(is.numeric), ~ round(.x, digits)))
}

safe_upload_name <- function(name) {
  name <- basename(name %||% "")
  name <- gsub("[^A-Za-z0-9._-]+", "_", name)
  if (!nzchar(name)) {
    name <- paste0("upload_", as.integer(Sys.time()))
  }
  name
}

materialize_upload <- function(file_row, label = "Uploaded file") {
  if (is.null(file_row) || nrow(file_row) == 0) {
    stop(label, " was not uploaded.")
  }

  source_path <- normalizePath(file_row$datapath[1], winslash = "/", mustWork = TRUE)
  original_name <- safe_upload_name(file_row$name[1])
  upload_dir <- file.path(tempdir(), "mazda_model_results_uploads")
  dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)

  target_path <- file.path(
    upload_dir,
    paste0(format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sample.int(999999, 1), "_", original_name)
  )

  if (!file.copy(source_path, target_path, overwrite = TRUE)) {
    stop("Could not prepare ", label, " for reading.")
  }

  normalizePath(target_path, winslash = "/", mustWork = TRUE)
}

assert_readable_file <- function(path, label = "File") {
  if (is.null(path) || !nzchar(path)) {
    stop(label, " path is empty.")
  }

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!file.exists(path)) {
    stop(label, " does not exist: ", path)
  }

  if (file.info(path)$size <= 0) {
    stop(label, " is empty: ", basename(path))
  }

  path
}

read_uploaded_table <- function(path) {
  path <- assert_readable_file(path, "Uploaded table")
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

parse_uploaded_date <- function(value, file_label) {
  if (inherits(value, "Date")) {
    return(value)
  }

  if (inherits(value, "POSIXct") || inherits(value, "POSIXt")) {
    return(as.Date(value))
  }

  if (is.numeric(value)) {
    parsed <- suppressWarnings(as.Date(value, origin = "1899-12-30"))
    if (sum(!is.na(parsed)) > 0) {
      return(parsed)
    }
  }

  value_chr <- trimws(as.character(value))
  value_chr[value_chr == ""] <- NA_character_

  parsed <- suppressWarnings(lubridate::parse_date_time(
    value_chr,
    orders = c(
      "ymd", "mdy", "dmy",
      "ymd HMS", "mdy HMS", "dmy HMS",
      "ymd HM", "mdy HM", "dmy HM",
      "Ymd", "mdY", "dmY",
      "Y-m-d", "m/d/Y", "d/m/Y",
      "m-d-Y", "d-m-Y"
    ),
    tz = "UTC"
  ))

  parsed_date <- as.Date(parsed)
  if (all(is.na(parsed_date))) {
    examples <- unique(stats::na.omit(value_chr))[seq_len(min(5, length(stats::na.omit(unique(value_chr)))))]
    stop(
      file_label,
      " has a Date column, but its values could not be parsed. Examples: ",
      paste(examples, collapse = ", "),
      ". Use a standard date format such as YYYY-MM-DD."
    )
  }

  parsed_date
}

clean_date_table <- function(df, file_label) {
  bad_cols <- grepl("^X$|^X\\.\\d+$|^\\.\\.\\.\\d+$|^$", colnames(df))
  df <- df[, !bad_cols, drop = FALSE]

  date_idx <- which(tolower(colnames(df)) == "date")
  if (length(date_idx) == 0) {
    stop(file_label, " must include a Date column.")
  }

  colnames(df)[date_idx[1]] <- "Date"
  df$Date <- parse_uploaded_date(df$Date, file_label)
  dplyr::as_tibble(df)
}

read_pct_contribution <- function(path) {
  path <- assert_readable_file(path, "Contribution Percentages")
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

clean_artifact_variable <- function(variable) {
  variable <- sub("^media:", "", variable)
  variable <- sub("^external:", "", variable)
  variable <- sub("^trend:", "", variable)
  variable <- gsub("[^A-Za-z0-9_]+", "_", variable)
  gsub("_+", "_", variable)
}

detect_kpi_column <- function(df, file_label = "MFF / Data Input") {
  candidates <- setdiff(colnames(df), c("Date", "Row"))
  kpi_cols <- candidates[grepl("KPI", candidates, ignore.case = TRUE)]

  if (length(kpi_cols) >= 1) {
    return(kpi_cols[1])
  }
  if ("Actual" %in% candidates) {
    return("Actual")
  }
  if (length(candidates) == 1) {
    return(candidates[1])
  }

  stop(
    "Could not auto-detect the KPI column in ", file_label,
    ". Rename the target column to include 'KPI' or 'Actual'. Available columns: ",
    paste(candidates, collapse = ", ")
  )
}

assert_required_columns <- function(df, required, file_label) {
  missing <- setdiff(required, colnames(df))
  if (length(missing) > 0) {
    stop(file_label, " must include: ", paste(missing, collapse = ", "))
  }
}

read_model_csv <- function(path, file_label) {
  path <- assert_readable_file(path, file_label)
  ext <- tolower(tools::file_ext(path))
  if (ext != "csv") {
    stop(file_label, " must be a CSV file.")
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

load_model_data_from_new_outputs <- function(mff_path, predictions_path, contributions_path,
                                             contribution_summary_path = NULL) {
  df_mff <- read_uploaded_table(mff_path) %>%
    clean_date_table("MFF / Data Input")
  kpi_col <- tryCatch(detect_kpi_column(df_mff), error = function(e) NA_character_)

  df_mff <- df_mff %>%
    mutate(Row = row_number())

  df_pred <- read_model_csv(predictions_path, "predictions.csv")
  df_contrib_raw <- read_model_csv(contributions_path, "contributions.csv")

  required_pred <- c("row", "observed", "fitted")
  assert_required_columns(df_pred, required_pred, "predictions.csv")
  assert_required_columns(df_contrib_raw, "row", "contributions.csv")

  if (any(is.na(df_pred$row)) || any(is.na(df_contrib_raw$row))) {
    stop("predictions.csv and contributions.csv row columns cannot contain missing values.")
  }
  if (anyDuplicated(df_pred$row) > 0 || anyDuplicated(df_contrib_raw$row) > 0) {
    stop("predictions.csv and contributions.csv row columns must be unique.")
  }

  pred_rows <- sort(unique(as.integer(df_pred$row)))
  contrib_rows <- sort(unique(as.integer(df_contrib_raw$row)))
  if (!identical(pred_rows, contrib_rows)) {
    stop("predictions.csv and contributions.csv do not contain the same model row indexes.")
  }

  if (max(pred_rows, na.rm = TRUE) > nrow(df_mff) || min(pred_rows, na.rm = TRUE) < 1) {
    stop(
      "The uploaded MFF does not contain enough rows to match the model output row indexes. ",
      "Upload the exact MFF used by the model run."
    )
  }

  mff_non_spend_cols <- c("Date", "Actual", "Row", kpi_col[!is.na(kpi_col)])
  date_lookup <- df_mff %>%
    select(Row, Date, everything())

  df_actual <- df_pred %>%
    transmute(Row = as.integer(row), Actual = as.numeric(observed)) %>%
    left_join(date_lookup %>% select(-any_of("Actual")), by = "Row") %>%
    select(Date, Actual, everything(), -Row, -any_of(kpi_col[!is.na(kpi_col)]))

  contrib_cols_raw <- setdiff(colnames(df_contrib_raw), "row")
  contrib_names <- paste0("Contrib_", clean_artifact_variable(contrib_cols_raw))
  contrib_clean <- sub("^Contrib_", "", contrib_names)
  spend_columns <- setdiff(colnames(df_mff), mff_non_spend_cols)
  spend_match_count <- sum(contrib_clean %in% spend_columns)

  df_med <- df_contrib_raw %>%
    mutate(Row = as.integer(row)) %>%
    select(Row, all_of(contrib_cols_raw)) %>%
    left_join(df_pred %>% transmute(Row = as.integer(row), Pred = as.numeric(fitted)), by = "Row") %>%
    left_join(date_lookup %>% select(Row, Date), by = "Row") %>%
    select(Date, Pred, all_of(contrib_cols_raw))
  colnames(df_med)[match(contrib_cols_raw, colnames(df_med))] <- contrib_names

  summary_used <- FALSE
  df_pct <- NULL
  if (!is.null(contribution_summary_path) && nzchar(contribution_summary_path)) {
    df_summary <- read_model_csv(contribution_summary_path, "contribution_summary.csv")
    assert_required_columns(df_summary, c("label", "share_total"), "contribution_summary.csv")
    df_pct <- df_summary %>%
      filter(!is.na(label), !is.na(share_total)) %>%
      transmute(
        Variable = paste0("Contrib_", clean_artifact_variable(label)),
        Pct = as.numeric(share_total) * 100
      )
    summary_used <- TRUE
  }

  df <- df_actual %>%
    select(Date, Actual) %>%
    inner_join(df_med %>% select(Date, Pred), by = "Date") %>%
    arrange(Date)

  list(
    df = df,
    df_med = df_med,
    df_pct = df_pct,
    df_input = df_actual,
    diagnostics = list(
      input_format = "New model outputs",
      kpi_column = if (is.na(kpi_col)) "observed from predictions.csv" else kpi_col,
      pred_column = "fitted",
      row_note = "Row was created from the uploaded MFF row order. The MFF must be the exact file used by the model run.",
      mff_row_count = nrow(df_mff),
      prediction_row_range = range(pred_rows, na.rm = TRUE),
      contribution_row_range = range(contrib_rows, na.rm = TRUE),
      row_match_count = length(pred_rows),
      contribution_summary_used = summary_used,
      contribution_summary_message = if (summary_used) {
        "contribution_summary.csv was used for full-period contribution percentages."
      } else {
        "contribution_summary.csv was not uploaded. Full-period percentages will be recalculated from contribution units."
      },
      spend_match_count = spend_match_count,
      spend_columns = setdiff(colnames(df_actual), c("Date", "Actual")),
      contribution_columns = contrib_names,
      date_range = range(df$Date, na.rm = TRUE)
    )
  )
}

load_model_data <- function(data_input_path, med_contrib_path, pct_contrib_path) {
  df_actual <- read_uploaded_table(data_input_path) %>%
    clean_date_table("MFF / Data Input")

  df_med <- read_uploaded_table(med_contrib_path) %>%
    clean_date_table("Contributions")

  df_pct <- read_pct_contribution(pct_contrib_path)

  kpi_col <- detect_kpi_column(df_actual)
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
    return(list(
      data_input = NULL,
      med_contrib = NULL,
      pct_contrib = NULL,
      predictions = NULL,
      contributions = NULL,
      contribution_summary = NULL,
      input_format = "missing",
      diagnostics = character()
    ))
  }

  names_lower <- tolower(files$name)
  base_lower <- tolower(tools::file_path_sans_ext(basename(files$name)))
  pick <- function(pattern, source = names_lower) {
    idx <- which(grepl(pattern, source))
    if (length(idx) == 0) NULL else files[idx[1], , drop = FALSE]
  }

  data_input <- pick("data_input|mff|input", base_lower)
  predictions <- pick("^predictions$|^out_of_sample_predictions$", base_lower)
  contribution_summary <- pick("^contribution_summary$", base_lower)
  contributions <- pick("^contributions$", base_lower)
  pct <- pick("pct|percent|percentage|pct_contrib", base_lower)
  med <- pick("med_contrib|^med.*contrib", base_lower)

  if (!is.null(pct) && !is.null(med) && pct$name == med$name) {
    remaining <- files[files$name != pct$name, , drop = FALSE]
    idx <- which(grepl("med_contrib|contribution|contrib", tolower(remaining$name)))
    med <- if (length(idx) == 0) NULL else remaining[idx[1], , drop = FALSE]
  }

  new_ready <- !is.null(data_input) && !is.null(predictions) && !is.null(contributions)
  legacy_ready <- !is.null(data_input) && !is.null(med) && !is.null(pct)
  input_format <- if (new_ready) {
    "new"
  } else if (legacy_ready) {
    "legacy"
  } else {
    "missing"
  }

  diagnostics <- c(
    paste("Detected input format:", input_format),
    paste("MFF / Data Input:", if (is.null(data_input)) "not detected" else data_input$name),
    paste("Contributions:", if (is.null(med)) "not detected" else med$name),
    paste("Contribution Percentages:", if (is.null(pct)) "not detected" else pct$name),
    paste("predictions.csv:", if (is.null(predictions)) "not detected" else predictions$name),
    paste("contributions.csv:", if (is.null(contributions)) "not detected" else contributions$name),
    paste("contribution_summary.csv:", if (is.null(contribution_summary)) "not detected" else contribution_summary$name)
  )

  list(
    data_input = data_input,
    med_contrib = med,
    pct_contrib = pct,
    predictions = predictions,
    contributions = contributions,
    contribution_summary = contribution_summary,
    input_format = input_format,
    diagnostics = diagnostics
  )
}
