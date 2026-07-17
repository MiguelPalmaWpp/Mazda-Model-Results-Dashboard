source("app_modules/ui_components.R", local = TRUE)

server <- function(input, output, session) {
  output$gradient_sheet_ui <- renderUI({
    req(input$gradient_file)
    ext <- tolower(tools::file_ext(input$gradient_file$name))

    if (ext %in% c("xlsx", "xlsm", "xls")) {
      sheets <- openxlsx::getSheetNames(input$gradient_file$datapath)
      selectInput("gradient_sheet", "Gradient Sheet", choices = sheets, selected = sheets[1])
    } else {
      textInput("gradient_sheet", "Gradient Sheet", value = "1")
    }
  })

  selected_files <- reactive({
    detect_uploaded_files(input$all_files)
  })

  output$file_status <- renderUI({
    files <- selected_files()

    file_row <- function(label, file) {
      ok <- !is.null(file)
      tags$div(
        class = if (ok) "qa-summary qa-summary-ok" else "qa-summary qa-summary-pending",
        tags$div(
          class = "qa-summary-main",
          tags$span(class = "qa-summary-icon", if (ok) HTML("&#10003;") else HTML("&#8226;")),
          tags$span(
            class = "qa-summary-text",
            tags$strong(label),
            tags$br(),
            if (ok) file$name else "Waiting for file"
          )
        )
      )
    }

    ready <- !is.null(files$data_input) && !is.null(files$med_contrib) && !is.null(files$pct_contrib)

    tags$div(
      class = "sidebar-file-status",
      file_row("MFF / Data Input", files$data_input),
      file_row("Contributions", files$med_contrib),
      file_row("Contribution Percentages", files$pct_contrib),
      tags$div(
        class = if (ready) "qa-summary qa-summary-ok" else "qa-summary qa-summary-warning",
        tags$div(
          class = "qa-summary-main",
          tags$span(class = "qa-summary-text",
                    if (ready) "All required files are loaded. Click Run Analysis."
                    else "Upload all three required files before running the analysis.")
        )
      )
    )
  })

  data_loaded <- eventReactive(input$run_analysis, {
    tryCatch({
      files <- selected_files()
      validate(
        need(!is.null(files$data_input), "Upload the MFF / Data Input file."),
        need(!is.null(files$med_contrib), "Upload the Contributions file."),
        need(!is.null(files$pct_contrib), "Upload the Contribution Percentages file.")
      )

      load_model_data(
        data_input_path = files$data_input$datapath,
        med_contrib_path = files$med_contrib$datapath,
        pct_contrib_path = files$pct_contrib$datapath
      )
    }, error = function(e) {
      showNotification(paste("Data loading failed:", conditionMessage(e)), type = "error", duration = 12)
      stop(e)
    })
  })

  analysis <- eventReactive(input$run_analysis, {
    tryCatch({
      loaded <- data_loaded()

      if (isTRUE(input$compare_new_period)) {
        validate(
          need(length(input$roi_range) == 2, "Select a valid ROI contribution date range."),
          need(input$roi_range[1] <= input$roi_range[2], "The ROI start date must be before the ROI end date.")
        )
      }

      gradient_path <- if (isTRUE(input$use_gradient) && !is.null(input$gradient_file)) {
        input$gradient_file$datapath
      } else {
        NULL
      }

      gradient_sheet <- if (!is.null(input$gradient_sheet) && nzchar(input$gradient_sheet)) {
        input$gradient_sheet
      } else {
        1
      }

      result <- build_analysis(
        data_loaded = loaded,
        cutoff_date = input$cutoff_date,
        revenue_per_unit = input$revenue_per_unit,
        aggregation_method = input$aggregation_method,
        roi_from = if (isTRUE(input$compare_new_period)) input$roi_range[1] else NULL,
        roi_to = if (isTRUE(input$compare_new_period)) input$roi_range[2] else NULL,
        compare_new_period = input$compare_new_period,
        use_gradient = input$use_gradient,
        gradient_path = gradient_path,
        gradient_sheet = gradient_sheet
      )

      showNotification("Analysis completed successfully.", type = "message", duration = 6)
      result
    }, error = function(e) {
      showNotification(paste("Analysis failed:", conditionMessage(e)), type = "error", duration = 12)
      stop(e)
    })
  })

  output$overview_summary <- renderUI({
    loaded <- data_loaded()
    result <- analysis()

    tile <- function(label, value, note = NULL) {
      tags$div(
        class = "overview-tile",
        tags$span(class = "overview-tile-label", label),
        tags$span(class = "overview-tile-value", value),
        if (!is.null(note)) tags$span(class = "overview-tile-note", note)
      )
    }

    status <- function(label, value) {
      tags$div(
        class = "overview-status",
        tags$strong(label),
        tags$span(value)
      )
    }

    tagList(
      tags$div(
        class = "overview-grid",
        tile(
          "Date Range",
          paste(as.character(min(result$df$Date)), "to", as.character(max(result$df$Date))),
          paste(format(nrow(result$df), big.mark = ","), "matched model rows")
        ),
        tile("MFF Rows", format(nrow(loaded$df_input), big.mark = ","), "Source input records"),
        tile("Contribution Rows", format(nrow(loaded$df_med), big.mark = ","), "Contribution records"),
        tile("Variables", format(length(loaded$diagnostics$contribution_columns), big.mark = ","), "Contribution columns")
      ),
      tags$div(
        class = "overview-status-row",
        status("ROI Period", result$roi_period_label),
        status("New Period Comparison", if (isTRUE(result$compare_new_period)) "Enabled" else "Disabled"),
        status("Gradient Status", result$gradient_message)
      )
    )
  })

  output$overview_metrics <- renderDT({
    metrics <- analysis()$overview_metrics %>%
      mutate(Value = round(Value, 2)) %>%
      tidyr::pivot_wider(
        names_from = Granularity,
        values_from = Value
      ) %>%
      select(Metric, Daily, Weekly, Monthly)

    metrics_matrix_table(metrics)
  })

  output$fit_timeseries <- renderPlotly({
    result <- analysis()
    granularity <- input$fit_granularity %||% "Daily"
    build_fit_timeseries_plot(model_fit_data(result, granularity), granularity)
  })

  output$fit_scatter <- renderPlotly({
    result <- analysis()
    granularity <- input$fit_granularity %||% "Daily"
    build_fit_scatter_plot(model_fit_data(result, granularity), granularity)
  })

  output$metrics_over_time <- renderDT({
    dt_table(analysis()$metrics_over_time, page_length = 12)
  })

  output$roi_table <- renderDT({
    dt_table(analysis()$roi_table, page_length = 15)
  })

  output$full_period_table <- renderDT({
    dt_table(analysis()$full_period_table, page_length = 15)
  })

  output$historical_table <- renderDT({
    dt_table(head(analysis()$historical_table, 100), page_length = 10)
  })

  output$pre_vs_post_table <- renderDT({
    dt_table(analysis()$pre_vs_post_table, page_length = 15)
  })

  output$diagnostics <- renderText({
    files <- selected_files()
    loaded <- data_loaded()
    result <- analysis()
    diag <- loaded$diagnostics

    paste(
      c(
        "File Detection",
        files$diagnostics,
        "",
        "Detected Columns",
        paste("KPI column:", diag$kpi_column),
        paste("Prediction column:", diag$pred_column),
        paste("Spend columns:", length(diag$spend_columns)),
        paste("Contribution columns:", length(diag$contribution_columns)),
        "",
        "Date Range",
        paste(as.character(diag$date_range[1]), "to", as.character(diag$date_range[2])),
        "",
        "Correlation Split",
        capture.output(print(result$correlation))
      ),
      collapse = "\n"
    )
  })

  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("mazda_model_results_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- build_excel_report(
        analysis = analysis(),
        cutoff_date = input$cutoff_date,
        roi_from = if (isTRUE(input$compare_new_period)) input$roi_range[1] else NULL,
        roi_to = if (isTRUE(input$compare_new_period)) input$roi_range[2] else NULL,
        revenue_per_unit = input$revenue_per_unit
      )
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$download_correlation <- downloadHandler(
    filename = function() {
      paste0("correlation_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(analysis()$correlation, file)
    }
  )

  output$download_long_format <- downloadHandler(
    filename = function() {
      paste0("long_format_contributions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      result <- analysis()
      readr::write_csv(
        build_long_format_table(
          result$df_med_original,
          result$df_input,
          if (isTRUE(result$gradient_applied)) result$df_med else NULL
        ),
        file
      )
    }
  )
}
