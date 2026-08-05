empty_period_value <- function(x) {
  if (length(x) == 0 || all(is.na(x)) || !all(is.finite(as.numeric(x)))) {
    NA_character_
  } else {
    as.character(x)
  }
}

build_correlation_table <- function(df, cutoff_date) {
  if (is.null(cutoff_date) || is.na(cutoff_date)) {
    return(data.frame(
      Period = "Full Period",
      Date_From = as.character(min(df$Date, na.rm = TRUE)),
      Date_To = as.character(max(df$Date, na.rm = TRUE)),
      N_Rows = nrow(df),
      Correlation = round(calc_pearson(df$Actual, df$Pred), 3)
    ))
  }

  before <- df %>% filter(Date <= cutoff_date)
  after <- df %>% filter(Date > cutoff_date)

  data.frame(
    Period = c("Before", "After"),
    Date_From = c(
      empty_period_value(suppressWarnings(min(before$Date, na.rm = TRUE))),
      empty_period_value(suppressWarnings(min(after$Date, na.rm = TRUE)))
    ),
    Date_To = c(
      empty_period_value(suppressWarnings(max(before$Date, na.rm = TRUE))),
      empty_period_value(suppressWarnings(max(after$Date, na.rm = TRUE)))
    ),
    N_Rows = c(nrow(before), nrow(after)),
    Correlation = c(
      if (nrow(before) >= 2) round(calc_pearson(before$Actual, before$Pred), 3) else NA_real_,
      if (nrow(after) >= 2) round(calc_pearson(after$Actual, after$Pred), 3) else NA_real_
    )
  )
}

build_historical_contributions_table <- function(df_med) {
  contrib_cols <- colnames(df_med)[grepl("^Contrib_", colnames(df_med))]
  if (length(contrib_cols) == 0) {
    stop("No Contrib_ columns found for Historical Contributions.")
  }

  select_cols <- c("Date", contrib_cols)
  if ("Base" %in% colnames(df_med)) {
    select_cols <- c(select_cols, "Base")
  }

  df_med %>%
    select(all_of(select_cols)) %>%
    arrange(Date) %>%
    mutate(Date = format(Date, "%Y-%m-%d")) %>%
    setNames(sub("^Contrib_", "", colnames(.)))
}

build_long_format_table <- function(df_med_original, df_input, df_med_gradient = NULL, cftp_data = NULL) {
  contrib_cols <- colnames(df_med_original)[grepl("^Contrib_", colnames(df_med_original))]
  if ("Base" %in% colnames(df_med_original)) {
    contrib_cols <- c(contrib_cols, "Base")
  }
  if (length(contrib_cols) == 0) {
    stop("No Contrib_ columns found for Long Format export.")
  }
  
  variable_lookup <- tibble(
    contrib_col = contrib_cols,
    variable = trimws(sub("^Contrib_", "", contrib_cols)),
    variable_key = normalize_mapping_key(variable)
  )

  df_contrib_long <- df_med_original %>%
    select(Date, all_of(contrib_cols)) %>%
    pivot_longer(
      cols = -Date,
      names_to = "contrib_col",
      values_to = "contribution"
    ) %>%
    left_join(variable_lookup, by = "contrib_col") %>%
    select(Date, variable, contribution)

  if (!is.null(df_med_gradient)) {
    gradient_cols <- intersect(contrib_cols, colnames(df_med_gradient))
    df_gradient_long <- df_med_gradient %>%
      select(Date, all_of(gradient_cols)) %>%
      pivot_longer(
        cols = -Date,
        names_to = "contrib_col",
        values_to = "contribution_gradient"
      ) %>%
      left_join(variable_lookup, by = "contrib_col") %>%
      select(Date, variable, contribution_gradient)
  } else {
    df_gradient_long <- NULL
  }

  spend_cols <- setdiff(colnames(df_input), c("Date", "Actual", "Row"))
  spend_cols <- spend_cols[is_spend_column(spend_cols)]
  spend_lookup <- setNames(spend_cols, spend_cols)
  spend_lookup_normalized <- setNames(spend_cols, normalize_mapping_key(spend_cols))
  spend_match <- variable_lookup %>%
    mutate(
      spend_col = ifelse(
        variable %in% names(spend_lookup),
        spend_lookup[variable],
        spend_lookup_normalized[variable_key]
      )
    ) %>%
    filter(!is.na(spend_col), spend_col %in% colnames(df_input)) %>%
    distinct(variable, spend_col)

  if (nrow(spend_match) > 0) {
    df_spend_long <- df_input %>%
      select(Date, all_of(unique(spend_match$spend_col))) %>%
      pivot_longer(
        cols = -Date,
        names_to = "spend_col",
        values_to = "spend"
      ) %>%
      inner_join(spend_match, by = "spend_col") %>%
      select(Date, variable, spend)
  } else {
    df_spend_long <- NULL
  }

  df_long <- df_contrib_long
  
  if (!is.null(df_spend_long)) {
    df_long <- df_long %>% left_join(df_spend_long, by = c("Date", "variable"))
  } else {
    df_long <- df_long %>% mutate(spend = NA_real_)
  }
  
  if (!is.null(df_gradient_long)) {
    df_long <- df_long %>% left_join(df_gradient_long, by = c("Date", "variable"))
  } else {
    df_long <- df_long %>% mutate(contribution_gradient = contribution)
  }
  
  if (!is.null(cftp_data) && nrow(cftp_data) > 0) {
    df_cftp_long <- cftp_data %>%
      select(Month, CFTP = AVG_CFTP) %>%
      distinct(Month, .keep_all = TRUE)
    
    df_long <- df_long %>%
      mutate(Month = floor_date(as.Date(Date), "month")) %>%
      left_join(df_cftp_long, by = "Month") %>%
      select(-Month)
  } else {
    df_long <- df_long %>% mutate(CFTP = NA_real_)
  }
  
  df_long %>%
    mutate(
      contribution = replace_na(contribution, 0),
      spend = replace_na(spend, 0),
      CFTP = as.numeric(CFTP),
      contribution_gradient = coalesce(contribution_gradient, contribution)
    ) %>%
    select(Date, variable, contribution, spend, CFTP, contribution_gradient) %>%
    arrange(Date, variable) %>%
    round_numeric_columns(3)
}

build_pre_vs_post_table <- function(df_med, cutoff_date) {
  if (is.null(cutoff_date) || is.na(cutoff_date)) {
    return(data.frame(Message = "Pre vs Post is disabled. Enable Compare New Period to calculate this table."))
  }

  df_pre <- df_med %>% filter(Date <= cutoff_date)
  df_post <- df_med %>% filter(Date > cutoff_date)

  if (nrow(df_pre) == 0) {
    return(data.frame(Message = "No data was found before the cutoff date. Choose a cutoff inside the available date range."))
  }

  if (nrow(df_post) == 0) {
    return(data.frame(Message = "No data was found after the cutoff date. Choose a cutoff before the last available date."))
  }

  contrib_cols <- colnames(df_med)[grepl("^Contrib_", colnames(df_med))]
  if ("Base" %in% colnames(df_med)) {
    contrib_cols <- c(contrib_cols, "Base")
  }
  if (length(contrib_cols) == 0) stop("No Contrib_ columns found.")

  sum_period <- function(df_period) {
    df_period %>%
      select(all_of(contrib_cols)) %>%
      summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Units")
  }

  inner_join(
    sum_period(df_pre) %>% rename(Pre_Units = Units),
    sum_period(df_post) %>% rename(Post_Units = Units),
    by = "Variable"
  ) %>%
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
    select(
      Variable = var_clean,
      Category,
      `Sub-Category` = Sub_Category,
      Funnel,
      Channel,
      `Pre Units` = Pre_Units,
      `Post Units` = Post_Units
    )
}

metrics_to_df <- function(metrics, granularity) {
  tibble(
    Granularity = granularity,
    Metric = names(metrics),
    Value = as.numeric(unlist(metrics))
  )
}
