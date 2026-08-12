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
  has_gradient <- "Pred_Gradient" %in% colnames(df)
  metrics_gradient <- if (has_gradient) {
    calculate_all_metrics(df, pred_col = "Pred_Gradient")
  } else {
    NULL
  }
  subtitle <- paste0(
    "R2 ", round(metrics$R2, 3),
    " | Pearson R ", round(metrics$`Pearson R`, 3),
    " | MAPE ", round(metrics$`MAPE (%)`, 3), "%"
  )
  if (!is.null(metrics_gradient)) {
    subtitle <- paste0(
      subtitle,
      "<br>Gradient R2 ", round(metrics_gradient$R2, 3),
      " | Pearson R ", round(metrics_gradient$`Pearson R`, 3),
      " | MAPE ", round(metrics_gradient$`MAPE (%)`, 3), "%"
    )
  }

  df_plot <- df %>%
    mutate(
      Date = as.Date(Date),
      Actual_Label = round(Actual, 3),
      Pred_Label = round(Pred, 3)
    )

  if (has_gradient) {
    df_plot <- df_plot %>%
      mutate(Pred_Gradient_Label = round(Pred_Gradient, 3))
  }

  p <- plot_ly(df_plot, x = ~Date) %>%
    add_lines(
      y = ~Actual,
      name = "Actual",
      type = "scatter",
      mode = "lines",
      line = list(color = "#5B9BD5", width = 2),
      hovertemplate = paste(
        "Date: %{x|%Y-%m-%d}",
        "<br>Series: Actual",
        "<br>Value: %{customdata:.3f}",
        "<extra></extra>"
      ),
      customdata = ~Actual_Label
    ) %>%
    add_lines(
      y = ~Pred,
      name = "Predicted",
      type = "scatter",
      mode = "lines",
      line = list(color = "#f39c12", width = 2),
      hovertemplate = paste(
        "Date: %{x|%Y-%m-%d}",
        "<br>Series: Predicted",
        "<br>Value: %{customdata:.3f}",
        "<extra></extra>"
      ),
      customdata = ~Pred_Label
    )

  if (has_gradient) {
    p <- p %>%
      add_lines(
        y = ~Pred_Gradient,
        name = "Predicted Gradient",
        type = "scatter",
        mode = "lines",
        line = list(color = "#2fb344", width = 2),
        hovertemplate = paste(
          "Date: %{x|%Y-%m-%d}",
          "<br>Series: Predicted Gradient",
          "<br>Value: %{customdata:.3f}",
          "<extra></extra>"
        ),
        customdata = ~Pred_Gradient_Label
      )
  }

  p %>%
    layout(
      annotations = list(
        list(
          text = subtitle,
          x = 1,
          y = 1.12,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "right",
          align = "right",
          font = list(size = 11, color = "#64748b")
        )
      ),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Value")
    ) %>%
    plotly_model_layout(top_margin = 76)
}

build_fit_scatter_plot <- function(df, title) {
  has_gradient <- "Pred_Gradient" %in% colnames(df)
  df_plot <- df %>%
    mutate(
      Date = as.Date(Date),
      Residual = Actual - Pred,
      Abs_Error = abs(Residual),
      Actual_Label = round(Actual, 3),
      Pred_Label = round(Pred, 3),
      Residual_Label = round(Residual, 3)
    )

  if (has_gradient) {
    df_plot <- df_plot %>%
      mutate(
        Residual_Gradient = Actual - Pred_Gradient,
        Pred_Gradient_Label = round(Pred_Gradient, 3),
        Residual_Gradient_Label = round(Residual_Gradient, 3)
      )
  }

  prediction_values <- c(df_plot$Actual, df_plot$Pred)
  if (has_gradient) {
    prediction_values <- c(prediction_values, df_plot$Pred_Gradient)
  }
  min_val <- min(prediction_values, na.rm = TRUE) * 0.95
  max_val <- max(prediction_values, na.rm = TRUE) * 1.05
  trend_df <- data.frame(Actual = numeric(0), Pred = numeric(0))
  if (nrow(df_plot) >= 2 && length(unique(df_plot$Actual)) >= 2) {
    trend_fit <- lm(Pred ~ Actual, data = df_plot)
    trend_df <- data.frame(Actual = c(min_val, max_val))
    trend_df$Pred <- predict(trend_fit, newdata = trend_df)
  }

  metrics <- calculate_all_metrics(df_plot)
  metrics_gradient <- if (has_gradient) {
    calculate_all_metrics(df_plot, pred_col = "Pred_Gradient")
  } else {
    NULL
  }
  subtitle <- paste0(
    "R2: ", round(metrics$R2, 3),
    " | Pearson R: ", round(metrics$`Pearson R`, 3),
    " | MAPE: ", round(metrics$`MAPE (%)`, 3), "%"
  )
  if (!is.null(metrics_gradient)) {
    subtitle <- paste0(
      subtitle,
      "<br>Gradient R2: ", round(metrics_gradient$R2, 3),
      " | Pearson R: ", round(metrics_gradient$`Pearson R`, 3),
      " | MAPE: ", round(metrics_gradient$`MAPE (%)`, 3), "%"
    )
  }

  p <- plot_ly(
    x = c(min_val, max_val),
    y = c(min_val, max_val),
    type = "scatter",
    mode = "lines",
    name = "Perfect Fit",
    line = list(color = "#94a3b8", width = 1.5, dash = "dash"),
    hoverinfo = "skip",
    showlegend = FALSE
  ) %>%
    add_markers(
      data = df_plot,
      x = ~Actual,
      y = ~Pred,
      name = "Observed",
      marker = list(color = "#5B9BD5", size = 7, opacity = 0.64),
      customdata = ~paste(Actual_Label, Pred_Label, Residual_Label, sep = "|"),
      hovertemplate = paste(
        "Actual: %{x:.3f}",
        "<br>Predicted: %{y:.3f}",
        "<extra></extra>"
      ),
      showlegend = has_gradient
    )

  if (has_gradient) {
    p <- p %>%
      add_markers(
        data = df_plot,
        x = ~Actual,
        y = ~Pred_Gradient,
        name = "Observed Gradient",
        marker = list(color = "#2fb344", size = 7, opacity = 0.54, symbol = "diamond"),
        hovertemplate = paste(
          "Actual: %{x:.3f}",
          "<br>Predicted Gradient: %{y:.3f}",
          "<extra></extra>"
        ),
        showlegend = TRUE
      )
  }

  if (nrow(trend_df) > 0) {
    p <- p %>%
      add_lines(
        data = trend_df,
        x = ~Actual,
        y = ~Pred,
        name = "Trend",
        type = "scatter",
        mode = "lines",
        line = list(color = "#4a87c0", width = 2),
        hoverinfo = "skip",
        showlegend = FALSE
      )
  }

  p %>%
    layout(
      annotations = list(
        list(
          text = subtitle,
          x = 1,
          y = 1.08,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "right",
          align = "right",
          font = list(size = 11, color = "#64748b")
        )
      ),
      xaxis = list(title = "Actual", range = c(min_val, max_val)),
      yaxis = list(title = "Predicted", range = c(min_val, max_val))
    ) %>%
    plotly_model_layout(top_margin = if (has_gradient) 62 else 38, show_legend = has_gradient)
}

build_error_behavior_plot <- function(df, title) {
  has_gradient <- "Pred_Gradient" %in% colnames(df)
  df_error <- df %>%
    mutate(
      Date = as.Date(Date),
      Residual = Actual - Pred,
      Abs_Error = abs(Residual),
      Residual_Label = round(Residual, 3),
      Abs_Error_Label = round(Abs_Error, 3),
      Error_Direction = if_else(Residual >= 0, "Under Prediction", "Over Prediction")
    )
  
  if (has_gradient) {
    df_error <- df_error %>%
      mutate(
        Residual_Gradient = Actual - Pred_Gradient,
        Abs_Error_Gradient = abs(Residual_Gradient),
        Residual_Gradient_Label = round(Residual_Gradient, 3),
        Abs_Error_Gradient_Label = round(Abs_Error_Gradient, 3)
      )
  }
  
  mean_residual <- mean(df_error$Residual, na.rm = TRUE)
  median_abs_error <- median(df_error$Abs_Error, na.rm = TRUE)
  subtitle <- paste0(
    "Mean residual: ", round(mean_residual, 3),
    " | Median absolute error: ", round(median_abs_error, 3)
  )
  
  if (has_gradient) {
    subtitle <- paste0(
      subtitle,
      "<br>Gradient mean residual: ", round(mean(df_error$Residual_Gradient, na.rm = TRUE), 3),
      " | Gradient median absolute error: ", round(median(df_error$Abs_Error_Gradient, na.rm = TRUE), 3)
    )
  }
  
  bar_colors <- ifelse(df_error$Residual >= 0, "#5B9BD5", "#E15241")
  
  p_time <- plot_ly(df_error, x = ~Date) %>%
    add_bars(
      y = ~Residual,
      name = "Residual",
      marker = list(color = bar_colors),
      customdata = ~paste(Residual_Label, Error_Direction, sep = "|"),
      hovertemplate = paste(
        "Date: %{x|%Y-%m-%d}",
        "<br>Residual: %{y:.3f}",
        "<br>%{customdata}",
        "<extra></extra>"
      )
    ) %>%
    add_lines(
      y = rep(mean_residual, nrow(df_error)),
      name = "Mean Residual",
      type = "scatter",
      mode = "lines",
      line = list(color = "#f39c12", width = 2, dash = "dash"),
      hoverinfo = "skip"
    ) %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Actual - Predicted"),
      shapes = list(
        list(
          type = "line",
          x0 = min(df_error$Date, na.rm = TRUE),
          x1 = max(df_error$Date, na.rm = TRUE),
          y0 = 0,
          y1 = 0,
          line = list(color = "#94a3b8", width = 1)
        )
      )
    )
  
  if (has_gradient) {
    p_time <- p_time %>%
      add_lines(
        y = ~Residual_Gradient,
        name = "Gradient Residual",
        type = "scatter",
        mode = "lines",
        line = list(color = "#2fb344", width = 2),
        hovertemplate = paste(
          "Date: %{x|%Y-%m-%d}",
          "<br>Gradient residual: %{y:.3f}",
          "<extra></extra>"
        )
      )
  }
  
  p_dist <- plot_ly(df_error) %>%
    add_histogram(
      x = ~Residual,
      name = "Residual Distribution",
      marker = list(color = "#5B9BD5", line = list(color = "#ffffff", width = 1)),
      opacity = 0.78,
      hovertemplate = paste(
        "Residual range: %{x}",
        "<br>Count: %{y}",
        "<extra></extra>"
      )
    ) %>%
    layout(
      xaxis = list(title = "Residual"),
      yaxis = list(title = "Count"),
      bargap = 0.05,
      shapes = list(
        list(
          type = "line",
          x0 = 0,
          x1 = 0,
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(color = "#E15241", width = 1.5, dash = "dash")
        )
      )
    )
  
  plotly::subplot(
    p_time,
    p_dist,
    nrows = 2,
    shareX = FALSE,
    titleY = TRUE,
    margin = 0.08,
    heights = c(0.58, 0.42)
  ) %>%
    layout(
      annotations = list(
        list(
          text = subtitle,
          x = 1,
          y = 1.1,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "right",
          align = "right",
          font = list(size = 11, color = "#64748b")
        )
      )
    ) %>%
    plotly_model_layout(top_margin = if (has_gradient) 72 else 48, show_legend = has_gradient)
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
