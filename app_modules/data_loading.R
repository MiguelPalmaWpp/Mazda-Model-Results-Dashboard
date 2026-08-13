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

artifact_match_variable <- function(variable) {
  trimws(sub("^[^:]+:", "", variable))
}

make_contribution_names <- function(raw_names) {
  base_names <- paste0("Contrib_", artifact_match_variable(raw_names))
  make.unique(base_names, sep = "_")
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

detect_model_row_offset <- function(df_mff, df_pred, kpi_col, offsets = -2:3) {
  if (is.na(kpi_col) || !kpi_col %in% colnames(df_mff)) {
    stop(
      "Could not validate model row alignment because the KPI column was not detected in the MFF. ",
      "Rename the target column to include 'KPI' or 'Actual'."
    )
  }
  
  model_rows <- as.integer(df_pred$row)
  observed <- suppressWarnings(as.numeric(df_pred$observed))
  kpi_values <- suppressWarnings(as.numeric(df_mff[[kpi_col]]))
  
  scores <- bind_rows(lapply(offsets, function(offset) {
    mff_rows <- model_rows + offset
    valid <- !is.na(mff_rows) & mff_rows >= 1 & mff_rows <= nrow(df_mff) & is.finite(observed)
    matched_values <- rep(NA_real_, length(model_rows))
    matched_values[valid] <- kpi_values[mff_rows[valid]]
    comparable <- valid & is.finite(matched_values)
    exact_count <- sum(comparable & abs(matched_values - observed) < 1e-9)
    mae <- if (any(comparable)) mean(abs(matched_values[comparable] - observed[comparable]), na.rm = TRUE) else Inf
    corr <- if (sum(comparable) >= 2) {
      suppressWarnings(cor(matched_values[comparable], observed[comparable], use = "complete.obs"))
    } else {
      NA_real_
    }
    tibble(
      offset = offset,
      valid_count = sum(valid),
      comparable_count = sum(comparable),
      exact_count = exact_count,
      mae = mae,
      correlation = corr
    )
  }))
  
  best <- scores %>%
    arrange(desc(exact_count), mae, desc(coalesce(correlation, -Inf)), abs(offset)) %>%
    slice(1)
  
  if (nrow(best) == 0 ||
      best$comparable_count == 0 ||
      best$exact_count < max(1, floor(0.95 * nrow(df_pred)))) {
    score_text <- paste(
      apply(scores, 1, function(row) {
        paste0("offset ", row[["offset"]], ": exact ", row[["exact_count"]],
               "/", nrow(df_pred), ", MAE ", round(as.numeric(row[["mae"]]), 4))
      }),
      collapse = "; "
    )
    stop(
      "Could not safely align model output rows to the MFF using observed KPI values. ",
      "Checked offsets -2 to +3. Scores: ", score_text
    )
  }
  
  offset <- as.integer(best$offset)
  mff_rows <- model_rows + offset
  valid <- mff_rows >= 1 & mff_rows <= nrow(df_mff)
  if (!all(valid)) {
    stop(
      "Detected row offset ", offset, " but some matched MFF rows fall outside the uploaded MFF range."
    )
  }
  
  first_idx <- which.min(model_rows)
  last_idx <- which.max(model_rows)
  list(
    offset = offset,
    scores = scores,
    exact_count = as.integer(best$exact_count),
    comparable_count = as.integer(best$comparable_count),
    mae = as.numeric(best$mae),
    correlation = as.numeric(best$correlation),
    first_model_row = model_rows[first_idx],
    first_mff_row = mff_rows[first_idx],
    first_date = as.Date(df_mff$Date[mff_rows[first_idx]]),
    last_model_row = model_rows[last_idx],
    last_mff_row = mff_rows[last_idx],
    last_date = as.Date(df_mff$Date[mff_rows[last_idx]])
  )
}

format_model_row_alignment_note <- function(alignment) {
  paste0(
    "Model row offset detected: ", sprintf("%+d", alignment$offset),
    " | Row match exact count: ", alignment$exact_count, " / ", alignment$comparable_count,
    " | First matched model row: ", alignment$first_model_row,
    " -> MFF row ", alignment$first_mff_row,
    " -> ", as.character(alignment$first_date),
    " | Last matched model row: ", alignment$last_model_row,
    " -> MFF row ", alignment$last_mff_row,
    " -> ", as.character(alignment$last_date),
    if (alignment$offset != 0) {
      paste0(" | Model row indexes are offset from the uploaded MFF row order. ",
             "The app adjusted the join using detected offset ", sprintf("%+d", alignment$offset), ".")
    } else {
      ""
    }
  )
}

date_alignment_diagnostics <- function(df_mff, df_pred, kpi_col) {
  if (is.na(kpi_col) || !kpi_col %in% colnames(df_mff)) {
    return(list(
      exact_count = NA_integer_,
      comparable_count = NA_integer_,
      mae = NA_real_,
      correlation = NA_real_
    ))
  }
  
  joined <- df_pred %>%
    transmute(Date, observed = suppressWarnings(as.numeric(observed))) %>%
    inner_join(
      df_mff %>%
        transmute(Date, mff_kpi = suppressWarnings(as.numeric(.data[[kpi_col]]))),
      by = "Date"
    )
  
  comparable <- is.finite(joined$observed) & is.finite(joined$mff_kpi)
  list(
    exact_count = sum(comparable & abs(joined$observed - joined$mff_kpi) < 1e-9),
    comparable_count = sum(comparable),
    mae = if (any(comparable)) mean(abs(joined$observed[comparable] - joined$mff_kpi[comparable]), na.rm = TRUE) else NA_real_,
    correlation = if (sum(comparable) >= 2) {
      suppressWarnings(cor(joined$observed[comparable], joined$mff_kpi[comparable], use = "complete.obs"))
    } else {
      NA_real_
    }
  )
}

read_upload_preview <- function(file_row, n_max = 5) {
  path <- materialize_upload(file_row, "Uploaded file")
  ext <- tolower(tools::file_ext(path))

  df <- tryCatch({
    if (ext %in% c("xlsx", "xlsm", "xls")) {
      openxlsx::read.xlsx(path, rows = 1:(n_max + 1), detectDates = TRUE, check.names = FALSE)
    } else if (ext == "csv") {
      readr::read_csv(path, n_max = n_max, show_col_types = FALSE, progress = FALSE)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (is.null(df)) {
    return(list(cols = character(), n_cols = 0))
  }

  list(cols = colnames(df), n_cols = ncol(df))
}

classify_upload_schema <- function(file_row) {
  preview <- read_upload_preview(file_row)
  cols <- preview$cols
  cols_lower <- tolower(cols)
  base_lower <- tolower(tools::file_path_sans_ext(basename(file_row$name[1])))

  has <- function(required) all(tolower(required) %in% cols_lower)
  has_any_prefix <- function(prefix) any(grepl(prefix, cols, ignore.case = TRUE))

  if (has(c("row", "observed", "fitted")) || has(c("date", "observed", "fitted"))) {
    return(list(type = "predictions", reason = "contains observed and fitted columns with row or Date"))
  }

  if ((has(c("row", "bias")) || has(c("date", "bias"))) && !has(c("observed", "fitted"))) {
    return(list(type = "new_contributions", reason = "contains bias columns with row or Date"))
  }

  if (has(c("label", "share_total"))) {
    return(list(type = "contribution_summary", reason = "contains label and share_total columns"))
  }

  if (has(c("date", "pred")) && has_any_prefix("^Contrib_")) {
    return(list(type = "med_contrib", reason = "contains Date, Pred, and Contrib_ columns"))
  }

  if (has(c("variable", "pct")) || (!"date" %in% cols_lower && preview$n_cols == 2) || grepl("pct|percent|percentage|pct_contrib", base_lower)) {
    return(list(type = "pct_contrib", reason = "contains Variable/Pct, has two columns, or filename suggests percentages"))
  }

  if ("date" %in% cols_lower) {
    return(list(type = "data_input", reason = "contains Date and does not match a model output schema"))
  }

  list(type = "unknown", reason = "no known schema matched")
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

  pred_has_date <- "Date" %in% colnames(df_pred) || "date" %in% tolower(colnames(df_pred))
  contrib_has_date <- "Date" %in% colnames(df_contrib_raw) || "date" %in% tolower(colnames(df_contrib_raw))
  
  if (pred_has_date && contrib_has_date) {
    df_pred <- clean_date_table(df_pred, "predictions.csv")
    df_contrib_raw <- clean_date_table(df_contrib_raw, "contributions.csv")
    assert_required_columns(df_pred, c("Date", "observed", "fitted"), "predictions.csv")
    assert_required_columns(df_contrib_raw, "Date", "contributions.csv")
    
    if (anyDuplicated(df_pred$Date) > 0 || anyDuplicated(df_contrib_raw$Date) > 0) {
      stop("predictions.csv and contributions.csv Date columns must be unique.")
    }
    
    pred_dates <- sort(unique(as.Date(df_pred$Date)))
    contrib_dates <- sort(unique(as.Date(df_contrib_raw$Date)))
    if (!identical(pred_dates, contrib_dates)) {
      stop("predictions.csv and contributions.csv do not contain the same dates.")
    }
    
    if (anyDuplicated(df_mff$Date) > 0) {
      stop("The uploaded MFF has duplicate Date values, so model outputs with Date cannot be safely joined.")
    }
    
    missing_mff_dates <- setdiff(pred_dates, as.Date(df_mff$Date))
    if (length(missing_mff_dates) > 0) {
      stop(
        "The uploaded MFF is missing dates found in predictions.csv/contributions.csv. Examples: ",
        paste(head(as.character(missing_mff_dates), 5), collapse = ", ")
      )
    }
    
    date_diag <- date_alignment_diagnostics(df_mff, df_pred, kpi_col)
    mff_non_spend_cols <- c("Date", "Actual", "Row", kpi_col[!is.na(kpi_col)])
    
    df_actual <- df_pred %>%
      transmute(Date, Actual = as.numeric(observed)) %>%
      left_join(
        df_mff %>% select(Date, everything(), -any_of(c("Actual", "Row", kpi_col[!is.na(kpi_col)]))),
        by = "Date"
      ) %>%
      select(Date, Actual, everything())
    
    contrib_cols_raw <- setdiff(colnames(df_contrib_raw), "Date")
    contrib_names <- make_contribution_names(contrib_cols_raw)
    contrib_clean <- sub("^Contrib_", "", contrib_names)
    spend_columns <- setdiff(colnames(df_mff), mff_non_spend_cols)
    spend_match_count <- sum(
      contrib_clean %in% spend_columns |
        gsub("[^a-z0-9]+", "_", tolower(contrib_clean)) %in%
          gsub("[^a-z0-9]+", "_", tolower(spend_columns))
    )
    
    df_med <- df_contrib_raw %>%
      select(Date, all_of(contrib_cols_raw)) %>%
      left_join(df_pred %>% transmute(Date, Pred = as.numeric(fitted)), by = "Date") %>%
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
          Variable = make_contribution_names(label),
          Pct = as.numeric(share_total) * 100
        )
      summary_used <- TRUE
    }
    
    df <- df_actual %>%
      select(Date, Actual) %>%
      inner_join(df_med %>% select(Date, Pred), by = "Date") %>%
      arrange(Date)
    
    return(list(
      df = df,
      df_med = df_med,
      df_pct = df_pct,
      df_input = df_actual,
      diagnostics = list(
        input_format = "New model outputs with Date",
        kpi_column = if (is.na(kpi_col)) "observed from predictions.csv" else kpi_col,
        pred_column = "fitted",
        row_note = "Model outputs include Date. The app joined predictions, contributions, and MFF by Date.",
        mff_row_count = nrow(df_mff),
        prediction_date_range = range(pred_dates, na.rm = TRUE),
        contribution_date_range = range(contrib_dates, na.rm = TRUE),
        row_match_count = length(pred_dates),
        row_offset = NA_integer_,
        row_alignment_exact_count = date_diag$exact_count,
        row_alignment_comparable_count = date_diag$comparable_count,
        row_alignment_mae = date_diag$mae,
        row_alignment_correlation = date_diag$correlation,
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
    ))
  }
  
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

  alignment <- detect_model_row_offset(df_mff, df_pred, kpi_col)
  pred_mff_rows <- pred_rows + alignment$offset
  
  if (max(pred_mff_rows, na.rm = TRUE) > nrow(df_mff) || min(pred_mff_rows, na.rm = TRUE) < 1) {
    stop(
      "The uploaded MFF does not contain enough rows to match the model output row indexes. ",
      "Upload the exact MFF used by the model run."
    )
  }

  mff_non_spend_cols <- c("Date", "Actual", "Row", kpi_col[!is.na(kpi_col)])
  date_lookup <- df_mff %>%
    select(MFF_Row = Row, Date, everything())

  df_actual <- df_pred %>%
    transmute(Row = as.integer(row), MFF_Row = Row + alignment$offset, Actual = as.numeric(observed)) %>%
    left_join(date_lookup %>% select(-any_of("Actual")), by = "MFF_Row") %>%
    select(Date, Actual, everything(), -Row, -MFF_Row, -any_of(kpi_col[!is.na(kpi_col)]))

  contrib_cols_raw <- setdiff(colnames(df_contrib_raw), "row")
  contrib_names <- make_contribution_names(contrib_cols_raw)
  contrib_clean <- sub("^Contrib_", "", contrib_names)
  spend_columns <- setdiff(colnames(df_mff), mff_non_spend_cols)
  spend_match_count <- sum(
    contrib_clean %in% spend_columns |
      gsub("[^a-z0-9]+", "_", tolower(contrib_clean)) %in%
        gsub("[^a-z0-9]+", "_", tolower(spend_columns))
  )

  df_med <- df_contrib_raw %>%
    mutate(Row = as.integer(row), MFF_Row = Row + alignment$offset) %>%
    select(Row, MFF_Row, all_of(contrib_cols_raw)) %>%
    left_join(df_pred %>% transmute(Row = as.integer(row), Pred = as.numeric(fitted)), by = "Row") %>%
    left_join(date_lookup %>% select(MFF_Row, Date), by = "MFF_Row") %>%
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
        Variable = make_contribution_names(label),
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
      row_note = format_model_row_alignment_note(alignment),
      mff_row_count = nrow(df_mff),
      prediction_row_range = range(pred_rows, na.rm = TRUE),
      matched_mff_row_range = range(pred_mff_rows, na.rm = TRUE),
      contribution_row_range = range(contrib_rows, na.rm = TRUE),
      row_match_count = length(pred_rows),
      row_offset = alignment$offset,
      row_alignment_exact_count = alignment$exact_count,
      row_alignment_comparable_count = alignment$comparable_count,
      row_alignment_mae = alignment$mae,
      row_alignment_correlation = alignment$correlation,
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

  classifications <- lapply(seq_len(nrow(files)), function(i) {
    file <- files[i, , drop = FALSE]
    schema <- classify_upload_schema(file)
    list(file = file, type = schema$type, reason = schema$reason)
  })

  pick_type <- function(type) {
    matches <- classifications[vapply(classifications, \(x) identical(x$type, type), logical(1))]
    if (length(matches) == 0) NULL else matches[[1]]$file
  }

  data_input <- pick_type("data_input")
  predictions <- pick_type("predictions")
  contribution_summary <- pick_type("contribution_summary")
  contributions <- pick_type("new_contributions")
  pct <- pick_type("pct_contrib")
  med <- pick_type("med_contrib")

  new_ready <- !is.null(data_input) && !is.null(predictions) && !is.null(contributions)
  legacy_ready <- !is.null(data_input) && !is.null(med) && !is.null(pct)
  input_format <- if (new_ready) {
    "new"
  } else if (legacy_ready) {
    "legacy"
  } else {
    "missing"
  }

  schema_diagnostics <- vapply(
    classifications,
    \(x) paste0(x$file$name[1], ": ", x$type, " (", x$reason, ")"),
    character(1)
  )

  diagnostics <- c(
    paste("Detected input format:", input_format),
    paste("MFF / Data Input:", if (is.null(data_input)) "not detected" else data_input$name),
    paste("Contributions:", if (is.null(med)) "not detected" else med$name),
    paste("Contribution Percentages:", if (is.null(pct)) "not detected" else pct$name),
    paste("predictions.csv:", if (is.null(predictions)) "not detected" else predictions$name),
    paste("contributions.csv:", if (is.null(contributions)) "not detected" else contributions$name),
    paste("contribution_summary.csv:", if (is.null(contribution_summary)) "not detected" else contribution_summary$name),
    "Schema classification:",
    schema_diagnostics
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
