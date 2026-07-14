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

build_long_format_table <- function(df_med, df_input) {
  contrib_cols <- colnames(df_med)[grepl("^Contrib_", colnames(df_med))]
  if ("Base" %in% colnames(df_med)) {
    contrib_cols <- c(contrib_cols, "Base")
  }
  if (length(contrib_cols) == 0) {
    stop("No Contrib_ columns found for Long Format export.")
  }

  df_contrib_long <- df_med %>%
    select(Date, all_of(contrib_cols)) %>%
    pivot_longer(
      cols = -Date,
      names_to = "variable",
      values_to = "contribution"
    ) %>%
    mutate(variable = trimws(sub("^Contrib_", "", variable)))

  spend_cols <- setdiff(colnames(df_input), c("Date", "Actual"))
  spend_cols <- intersect(spend_cols, unique(df_contrib_long$variable))

  if (length(spend_cols) > 0) {
    df_spend_long <- df_input %>%
      select(Date, all_of(spend_cols)) %>%
      pivot_longer(
        cols = -Date,
        names_to = "variable",
        values_to = "spend"
      )
  } else {
    df_spend_long <- df_contrib_long %>%
      distinct(Date, variable) %>%
      mutate(spend = NA_real_)
  }

  df_contrib_long %>%
    full_join(df_spend_long, by = c("Date", "variable")) %>%
    mutate(
      contribution = replace_na(contribution, 0),
      spend = replace_na(spend, 0),
      mapping = lapply(paste0("Contrib_", variable), get_channel_mapping),
      Channel = sapply(mapping, `[[`, "channel"),
      Category = sapply(mapping, `[[`, "category"),
      `Sub-Category` = sapply(mapping, `[[`, "sub_category"),
      Funnel = sapply(mapping, `[[`, "funnel")
    ) %>%
    select(Date, variable, contribution, spend, Channel, Category, `Sub-Category`, Funnel) %>%
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
      sort_key = coalesce(as.numeric(sort_order_map[Sub_Category]), 11L)
    ) %>%
    arrange(sort_key) %>%
    select(
      Variable = var_clean,
      Channel,
      Category,
      `Sub-Category` = Sub_Category,
      Funnel,
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
