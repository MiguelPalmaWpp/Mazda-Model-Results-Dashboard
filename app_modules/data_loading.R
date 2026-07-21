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

extract_uploaded_zip <- function(zip_file_row, label = "Artifacts ZIP") {
  zip_path <- materialize_upload(zip_file_row, label)
  extract_dir <- file.path(tempdir(), paste0("mazda_model_results_zip_", sample.int(999999, 1)))
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_path, exdir = extract_dir)
  normalizePath(extract_dir, winslash = "/", mustWork = TRUE)
}

find_artifact_file <- function(extract_dir, filename) {
  matches <- list.files(extract_dir, pattern = paste0("^", filename, "$"), recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(matches) == 0) {
    return(NULL)
  }
  normalizePath(matches[1], winslash = "/", mustWork = TRUE)
}

clean_artifact_variable <- function(variable) {
  variable <- sub("^media:", "", variable)
  variable <- sub("^external:", "", variable)
  variable <- sub("^trend:", "", variable)
  variable <- gsub("[^A-Za-z0-9_]+", "_", variable)
  gsub("_+", "_", variable)
}

load_model_data_from_artifacts <- function(artifacts_zip, mff_path = NULL) {
  extract_dir <- extract_uploaded_zip(artifacts_zip, "Artifacts ZIP")
  predictions_path <- find_artifact_file(extract_dir, "predictions.csv")
  contributions_path <- find_artifact_file(extract_dir, "contributions.csv")
  summary_path <- find_artifact_file(extract_dir, "contribution_summary.csv")

  if (is.null(predictions_path) || is.null(contributions_path) || is.null(summary_path)) {
    stop(
      "Artifacts ZIP must include predictions.csv, contributions.csv, and contribution_summary.csv."
    )
  }

  if (is.null(mff_path) || !nzchar(mff_path)) {
    stop(
      "Artifacts ZIP outputs use row numbers instead of dates. Upload the original MFF / Data Input file ",
      "together with artifacts.zip, or ask the model pipeline to include a Date column in predictions.csv ",
      "and contributions.csv."
    )
  }

  df_mff <- read_uploaded_table(mff_path) %>%
    clean_date_table("MFF / Data Input")
  df_pred <- readr::read_csv(predictions_path, show_col_types = FALSE, progress = FALSE)
  df_contrib_raw <- readr::read_csv(contributions_path, show_col_types = FALSE, progress = FALSE)
  df_summary <- readr::read_csv(summary_path, show_col_types = FALSE, progress = FALSE)

  required_pred <- c("row", "observed", "fitted")
  if (!all(required_pred %in% colnames(df_pred))) {
    stop("predictions.csv must include row, observed, and fitted columns.")
  }
  if (!"row" %in% colnames(df_contrib_raw)) {
    stop("contributions.csv must include a row column.")
  }
  if (max(df_pred$row, na.rm = TRUE) > nrow(df_mff) || min(df_pred$row, na.rm = TRUE) < 1) {
    stop(
      "The row indexes in predictions.csv do not match the uploaded MFF row count. ",
      "Add Date directly to predictions.csv and contributions.csv, or upload the matching MFF used by the model."
    )
  }

  date_lookup <- df_mff %>%
    mutate(.model_row = row_number()) %>%
    select(.model_row, Date, everything())

  df_actual <- df_pred %>%
    transmute(.model_row = as.integer(row), Actual = as.numeric(observed)) %>%
    left_join(date_lookup, by = ".model_row") %>%
    select(Date, Actual, everything(), -.model_row)

  contrib_cols_raw <- setdiff(colnames(df_contrib_raw), "row")
  contrib_names <- paste0("Contrib_", clean_artifact_variable(contrib_cols_raw))

  df_med <- df_contrib_raw %>%
    mutate(.model_row = as.integer(row)) %>%
    select(.model_row, all_of(contrib_cols_raw)) %>%
    left_join(df_pred %>% transmute(.model_row = as.integer(row), Pred = as.numeric(fitted)), by = ".model_row") %>%
    left_join(date_lookup %>% select(.model_row, Date), by = ".model_row") %>%
    select(Date, Pred, all_of(contrib_cols_raw))
  colnames(df_med)[match(contrib_cols_raw, colnames(df_med))] <- contrib_names

  df_pct <- df_summary %>%
    filter(!is.na(label), !is.na(share_total)) %>%
    transmute(
      Variable = paste0("Contrib_", clean_artifact_variable(label)),
      Pct = as.numeric(share_total) * 100
    )

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
      input_format = "Artifacts ZIP + MFF",
      kpi_column = "observed",
      pred_column = "fitted",
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
    return(list(data_input = NULL, med_contrib = NULL, pct_contrib = NULL, artifacts_zip = NULL, diagnostics = character()))
  }

  names_lower <- tolower(files$name)
  pick <- function(pattern) {
    idx <- which(grepl(pattern, names_lower))
    if (length(idx) == 0) NULL else files[idx[1], , drop = FALSE]
  }

  pct <- pick("pct|percent|percentage")
  med <- pick("med_contrib|contribution|contrib")
  data_input <- pick("data_input|mff|input")
  artifacts_zip <- pick("artifacts.*\\.zip$|\\.zip$")

  if (!is.null(pct) && !is.null(med) && pct$name == med$name) {
    remaining <- files[files$name != pct$name, , drop = FALSE]
    idx <- which(grepl("med_contrib|contribution|contrib", tolower(remaining$name)))
    med <- if (length(idx) == 0) NULL else remaining[idx[1], , drop = FALSE]
  }

  diagnostics <- c(
    paste("MFF / Data Input:", if (is.null(data_input)) "not detected" else data_input$name),
    paste("Contributions:", if (is.null(med)) "not detected" else med$name),
    paste("Contribution Percentages:", if (is.null(pct)) "not detected" else pct$name),
    paste("Artifacts ZIP:", if (is.null(artifacts_zip)) "not detected" else artifacts_zip$name)
  )

  list(data_input = data_input, med_contrib = med, pct_contrib = pct, artifacts_zip = artifacts_zip, diagnostics = diagnostics)
}
