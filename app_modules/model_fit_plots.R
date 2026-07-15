build_fit_plot <- function(df, title) {
  resid_sd <- sd(df$Actual - df$Pred, na.rm = TRUE)
  df_plot <- df %>%
    mutate(
      CI_Upper = Pred + 1.96 * resid_sd,
      CI_Lower = Pred - 1.96 * resid_sd
    )

  date_brk <- get_date_breaks(df$Date)
  min_val <- min(df$Actual, df$Pred, na.rm = TRUE) * 0.95
  max_val <- max(df$Actual, df$Pred, na.rm = TRUE) * 1.05

  p1 <- ggplot(df_plot, aes(x = Date)) +
    geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), fill = "#D95F02", alpha = 0.12) +
    geom_ribbon(aes(ymin = pmin(Actual, Pred), ymax = pmax(Actual, Pred)), fill = "gray40", alpha = 0.08) +
    geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
    geom_line(aes(y = Pred, color = "Predicted"), linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("Actual" = "#1B9E77", "Predicted" = "#D95F02")) +
    scale_x_date(date_labels = "%b %Y", date_breaks = date_brk) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
    labs(title = "Time Series", x = "Date", y = "Value", color = "")

  p2 <- ggplot(df, aes(x = Actual, y = Pred)) +
    geom_point(color = "#7570B3", alpha = 0.65) +
    geom_abline(slope = 1, intercept = 0, color = "#B22222", linetype = "dashed", linewidth = 1) +
    geom_smooth(method = "lm", se = FALSE, color = "#1F78B4", linewidth = 1) +
    xlim(min_val, max_val) +
    ylim(min_val, max_val) +
    theme_minimal(base_size = 12) +
    labs(title = "Scatter Plot", x = "Actual", y = "Predicted")

  arrangeGrob(p1, p2, nrow = 2, top = paste("Actual vs Predicted -", title))
}

model_fit_data <- function(analysis, granularity) {
  switch(
    granularity,
    "Daily" = analysis$df,
    "Weekly" = analysis$df_weekly,
    "Monthly" = analysis$df_monthly,
    analysis$df
  )
}

theme_model_fit <- function() {
  theme_minimal(base_size = 12, base_family = "Segoe UI") +
    theme(
      plot.title = element_text(face = "bold", color = "#1e293b", size = 16, margin = margin(b = 6)),
      plot.subtitle = element_text(color = "#64748b", size = 11, margin = margin(b = 12)),
      axis.title = element_text(color = "#1e293b", size = 11, face = "bold"),
      axis.text = element_text(color = "#334155", size = 10),
      panel.grid.major = element_line(color = "#e8eef5", linewidth = 0.45),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(color = "#334155", size = 11),
      plot.margin = margin(14, 22, 14, 14)
    )
}

plotly_model_layout <- function(p, top_margin = 78, show_legend = TRUE) {
  p %>%
    layout(
      hovermode = "closest",
      showlegend = show_legend,
      legend = list(
        orientation = "h",
        x = 0,
        y = 1.04,
        xanchor = "left",
        yanchor = "bottom",
        font = list(size = 12, color = "#334155")
      ),
      margin = list(l = 58, r = 24, t = top_margin, b = 48),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(
        titlefont = list(size = 12, color = "#1e293b"),
        tickfont = list(size = 11, color = "#334155"),
        gridcolor = "#e8eef5",
        zerolinecolor = "#e8eef5"
      ),
      yaxis = list(
        titlefont = list(size = 12, color = "#1e293b"),
        tickfont = list(size = 11, color = "#334155"),
        gridcolor = "#e8eef5",
        zerolinecolor = "#e8eef5"
      )
    ) %>%
    config(
      displaylogo = FALSE,
      responsive = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines")
    )
}

build_fit_timeseries_plot <- function(df, title) {
  metrics <- calculate_all_metrics(df)
  subtitle <- paste0(
    "R2 ", round(metrics$R2, 3),
    " | Pearson R ", round(metrics$`Pearson R`, 3),
    " | MAPE ", round(metrics$`MAPE (%)`, 3), "%"
  )

  df_plot <- df %>%
    mutate(
      Residual = Actual - Pred,
      Date = as.Date(Date)
    ) %>%
    select(Date, Actual, Pred, Residual) %>%
    pivot_longer(
      cols = c(Actual, Pred),
      names_to = "Series",
      values_to = "Value"
    ) %>%
    mutate(Series = recode(Series, "Pred" = "Predicted"))

  p <- ggplot(df_plot, aes(x = Date, y = Value, color = Series, group = Series)) +
    geom_line(
      linewidth = 1.15,
      alpha = 0.95
    ) +
    scale_color_manual(values = c("Actual" = "#5B9BD5", "Predicted" = "#f39c12")) +
    theme_model_fit() +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Date",
      y = "Value",
      color = ""
    )

  ggplotly(p, tooltip = c("x", "y", "colour")) %>%
    plotly_model_layout(top_margin = 58)
}

build_fit_scatter_plot <- function(df, title) {
  df_plot <- df %>%
    mutate(
      Date = as.Date(Date),
      Residual = Actual - Pred,
      Abs_Error = abs(Residual)
    )

  min_val <- min(df_plot$Actual, df_plot$Pred, na.rm = TRUE) * 0.95
  max_val <- max(df_plot$Actual, df_plot$Pred, na.rm = TRUE) * 1.05
  trend_fit <- lm(Pred ~ Actual, data = df_plot)
  trend_df <- data.frame(Actual = c(min_val, max_val))
  trend_df$Pred <- predict(trend_fit, newdata = trend_df)

  metrics <- calculate_all_metrics(df_plot)
  subtitle <- paste0(
    "R2: ", round(metrics$R2, 3),
    " | Pearson R: ", round(metrics$`Pearson R`, 3),
    " | MAPE: ", round(metrics$`MAPE (%)`, 3), "%"
  )

  p <- ggplot(df_plot, aes(x = Actual, y = Pred)) +
    geom_abline(slope = 1, intercept = 0, color = "#94a3b8", linetype = "dashed", linewidth = 0.85) +
    geom_line(
      data = trend_df,
      aes(x = Actual, y = Pred),
      inherit.aes = FALSE,
      color = "#4a87c0",
      linewidth = 0.95
    ) +
    geom_point(
      color = "#5B9BD5",
      alpha = 0.64,
      size = 2.3
    ) +
    coord_cartesian(xlim = c(min_val, max_val), ylim = c(min_val, max_val)) +
    theme_model_fit() +
    theme(legend.position = "none") +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Actual",
      y = "Predicted"
    )

  ggplotly(p, tooltip = c("x", "y")) %>%
    plotly_model_layout(top_margin = 38, show_legend = FALSE)
}

build_residuals_plot <- function(df, title) {
  df_res <- df %>% mutate(Residual = Actual - Pred)
  mean_res <- mean(df_res$Residual, na.rm = TRUE)
  date_brk <- get_date_breaks(df$Date)

  p1 <- ggplot(df_res, aes(x = Date, y = Residual, fill = Residual >= 0)) +
    geom_col(alpha = 0.75) +
    scale_fill_manual(values = c("TRUE" = "#1B9E77", "FALSE" = "#D95F02"), guide = "none") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    geom_hline(yintercept = mean_res, color = "#E6AB02", linewidth = 0.8, linetype = "dashed") +
    scale_x_date(date_labels = "%b %Y", date_breaks = date_brk) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Residuals Over Time", x = "Date", y = "Residual")

  p2 <- ggplot(df_res, aes(x = Residual)) +
    geom_histogram(bins = 30, fill = "#7570B3", color = "white", alpha = 0.8) +
    geom_vline(xintercept = 0, color = "#B22222", linewidth = 0.8, linetype = "dashed") +
    geom_vline(xintercept = mean_res, color = "#E6AB02", linewidth = 0.8, linetype = "dashed") +
    theme_minimal(base_size = 12) +
    labs(title = "Residuals Distribution", x = "Residual Value", y = "Frequency")

  arrangeGrob(p1, p2, nrow = 2, top = paste("Residuals Analysis -", title))
}
