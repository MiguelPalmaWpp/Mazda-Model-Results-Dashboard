build_roi_table <- function(df_med_input, cftp_data,
                            df_input_filtered = NULL,
                            df_pct = NULL) {
  all_cols <- colnames(df_med_input)
  contrib_cols <- all_cols[grepl("^Contrib_", all_cols)]
  has_base <- "Base" %in% all_cols

  cat("  Contrib_ columns:", length(contrib_cols), "| Base:", has_base, "\n")

  if (length(contrib_cols) == 0) {
    stop("No Contrib_ columns found in df_med")
  }
  if (has_base) {
    contrib_cols <- c(contrib_cols, "Base")
  }

  cftp_revenue <- calculate_cftp_revenue(df_med_input, contrib_cols, cftp_data)

  units <- tibble(
    Variable = contrib_cols,
    Units = as.numeric(colSums(df_med_input[, contrib_cols, drop = FALSE], na.rm = TRUE))
  )

  if (!is.null(df_pct)) {
    cat("  % Contribution: using model file (df_pct)\n")
    contrib_dict <- setNames(as.numeric(df_pct$Pct), df_pct$Variable)
    units <- units %>%
      mutate(Period_Pct = as.numeric(contrib_dict[Variable]))
  } else {
    total_units <- sum(units$Units, na.rm = TRUE)
    pos_sum <- sum(units$Units[units$Units > 0], na.rm = TRUE)
    neg_sum <- sum(units$Units[units$Units < 0], na.rm = TRUE)

    cat("  % Contribution: recalculated from period units\n")
    cat("  Total:", round(total_units, 1),
        "| Pos:", round(pos_sum, 1),
        "| Neg:", round(neg_sum, 1), "\n")

    if (abs(total_units) == 0) {
      warning("Total units for period = 0. % Contribution will be NA.")
    }

    units <- units %>%
      mutate(
        Period_Pct = if (abs(total_units) > 0) (Units / total_units) * 100 else NA_real_
      )
  }

  spend_lookup <- setNames(numeric(0), character(0))
  spend_lookup_normalized <- setNames(numeric(0), character(0))

  if (!is.null(df_input_filtered)) {
    spend_cols <- df_input_filtered %>%
      select(-any_of(c("Date", "Actual", "Row"))) %>%
      select(where(is.numeric)) %>%
      colnames()
    spend_cols <- spend_cols[is_spend_column(spend_cols)]

    if (length(spend_cols) > 0) {
      spend_sums <- tibble(
        spend_col = spend_cols,
        Spend_Total = as.numeric(colSums(df_input_filtered[, spend_cols, drop = FALSE], na.rm = TRUE))
      )

      spend_lookup <- setNames(spend_sums$Spend_Total, spend_sums$spend_col)
      spend_lookup_normalized <- setNames(
        spend_sums$Spend_Total,
        gsub("[^a-z0-9]+", "_", tolower(spend_sums$spend_col))
      )

      var_clean_vals <- sub("^Contrib_", "", contrib_cols)
      var_clean_vals <- var_clean_vals[var_clean_vals != "Base"]
      var_normalized <- normalize_mapping_key(var_clean_vals)
      is_matched <- var_clean_vals %in% names(spend_lookup) | var_normalized %in% names(spend_lookup_normalized)
      unmatched <- var_clean_vals[!is_matched]

      cat("  Spend matched:", sum(is_matched), "/ Total:", length(var_clean_vals), "\n")
      if (length(unmatched) > 0) {
        cat("  WARNING - no spend column for:", paste(unmatched, collapse = ", "), "\n")
      }
    }
  }

  units %>%
    mutate(
      var_clean = sub("^Contrib_", "", Variable),
      mapping = lapply(Variable, get_channel_mapping),
      Channel = sapply(mapping, `[[`, "channel"),
      Category = sapply(mapping, `[[`, "category"),
      Sub_Category = sapply(mapping, `[[`, "sub_category"),
      Funnel = sapply(mapping, `[[`, "funnel"),
      sort_key = ifelse(
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
      ROI = ifelse(Units > 0 & Has_Spend_Match & !is.na(Revenue) & Spend != 0,
                   Revenue / Spend,
                   NA_real_)
    ) %>%
    select(
      Variable = var_clean,
      Units,
      `% Contribution` = Period_Pct,
      `Model Contribution` = Model_Contribution,
      `Expected Contribution` = Expected_Contribution,
      Spend,
      Revenue,
      ROI,
      Category,
      `Sub-Category` = Sub_Category,
      Funnel,
      Channel
    )
}

write_roi_sheet <- function(wb, sheet_name, df_export) {
  df_export <- df_export %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df_export, startRow = 1)
  apply_header_style(wb, sheet_name, 1, 1:ncol(df_export))
  stripe_rows(wb, sheet_name, nrow(df_export), 1, ncol(df_export))

  spend_letter <- LETTERS[which(colnames(df_export) == "Spend")]
  rev_letter <- LETTERS[which(colnames(df_export) == "Revenue")]
  roi_col_idx <- which(colnames(df_export) == "ROI")

  for (i in seq_len(nrow(df_export))) {
    if (!is.na(df_export$Revenue[i])) {
      data_row <- i + 1L
      writeFormula(
        wb, sheet_name,
        x = paste0('=IFERROR(ROUND(', rev_letter, data_row,
                   '/', spend_letter, data_row, ',3),"")'),
        startRow = data_row,
        startCol = roi_col_idx
      )
    }
  }

  setColWidths(wb, sheet_name,
               cols = seq_along(ROI_COL_WIDTHS),
               widths = ROI_COL_WIDTHS)

  cat(" ->", sheet_name, "written:", nrow(df_export), "rows\n")
}

add_roi_sheet <- function(wb, df_med, df_input,
                          contrib_date_from, contrib_date_to,
                          cftp_data) {
  cat("\nBuilding ROI sheet (filtered period)...\n")

  df_med_f <- df_med %>% filter(Date >= contrib_date_from & Date <= contrib_date_to)
  df_input_f <- df_input %>% filter(Date >= contrib_date_from & Date <= contrib_date_to)

  cat("  Period:", as.character(contrib_date_from), "->", as.character(contrib_date_to), "\n")
  cat("  Rows - contrib:", nrow(df_med_f), "| spend:", nrow(df_input_f), "\n")

  df_export <- build_roi_table(df_med_f, cftp_data = cftp_data,
                               df_input_filtered = df_input_f,
                               df_pct = NULL)
  write_roi_sheet(wb, "ROI", df_export)
}

add_full_period_contrib_sheet <- function(wb, df_med, df_pct, df_input,
                                          cftp_data) {
  cat("\nBuilding Full Period Contribution sheet (all dates)...\n")
  cat("  Date range:", as.character(min(df_med$Date)), "->", as.character(max(df_med$Date)), "\n")
  cat("  Total rows:", nrow(df_med), "\n")

  df_export <- build_roi_table(df_med, cftp_data = cftp_data,
                               df_input_filtered = df_input,
                               df_pct = df_pct)
  write_roi_sheet(wb, "Full Period Contribution", df_export)
}
