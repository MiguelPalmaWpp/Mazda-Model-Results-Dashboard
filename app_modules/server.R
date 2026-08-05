server <- function(input, output, session) {
  full_period_tab_inserted <- reactiveVal(FALSE)
  pre_vs_post_tab_inserted <- reactiveVal(FALSE)

  observe({
    if (isTRUE(input$compare_new_period) && !isTRUE(full_period_tab_inserted())) {
      insertTab(
        inputId = "main_tabs",
        target = "ROI",
        position = "after",
        select = FALSE,
        tabPanel(
          "Full Period Contribution",
          card(
            "Full Model Period",
            uiOutput("full_period_version_switch"),
            DTOutput("full_period_table")
          )
        )
      )
      full_period_tab_inserted(TRUE)
    } else if (!isTRUE(input$compare_new_period) && isTRUE(full_period_tab_inserted())) {
      removeTab(inputId = "main_tabs", target = "Full Period Contribution")
      full_period_tab_inserted(FALSE)
    }
    
    if (isTRUE(input$compare_new_period) && !isTRUE(pre_vs_post_tab_inserted())) {
      insertTab(
        inputId = "main_tabs",
        target = "Historical Contributions",
        position = "after",
        select = FALSE,
        tabPanel("Pre vs Post", card("Pre vs Post Contribution", DTOutput("pre_vs_post_table")))
      )
      pre_vs_post_tab_inserted(TRUE)
    } else if (!isTRUE(input$compare_new_period) && isTRUE(pre_vs_post_tab_inserted())) {
      removeTab(inputId = "main_tabs", target = "Pre vs Post")
      pre_vs_post_tab_inserted(FALSE)
    }
  })
  
  observeEvent(input$reset_analysis_files, {
    session$sendCustomMessage("resetFileInput", list(id = "all_files"))
    showNotification("Model input files were reset.", type = "message", duration = 4)
  })

  output$gradient_sheet_ui <- renderUI({
    req(input$gradient_file)
    ext <- tolower(tools::file_ext(input$gradient_file$name))

    if (ext %in% c("xlsx", "xlsm", "xls")) {
      gradient_path <- materialize_upload(input$gradient_file, "Gradient file")
      sheets <- openxlsx::getSheetNames(gradient_path)
      selectizeInput(
        "gradient_sheet",
        "Gradient Sheet",
        choices = sheets,
        selected = sheets[1],
        options = list(dropdownParent = "body")
      )
    } else {
      textInput("gradient_sheet", "Gradient Sheet", value = "1")
    }
  })
  
  output$cftp_sheet_ui <- renderUI({
    req(input$cftp_file)
    ext <- tolower(tools::file_ext(input$cftp_file$name))
    
    if (ext %in% c("xlsx", "xlsm", "xls")) {
      cftp_path <- materialize_upload(input$cftp_file, "Consumer Facing Transaction Price AVG")
      sheets <- openxlsx::getSheetNames(cftp_path)
      selectizeInput(
        "cftp_sheet",
        "CFTP Sheet",
        choices = sheets,
        selected = sheets[1],
        options = list(dropdownParent = "body")
      )
    } else {
      textInput("cftp_sheet", "CFTP Sheet", value = "1")
    }
  })
  
  cftp_nameplates <- reactive({
    req(input$cftp_file)
    cftp_path <- materialize_upload(input$cftp_file, "Consumer Facing Transaction Price AVG")
    cftp_sheet <- if (!is.null(input$cftp_sheet) && nzchar(input$cftp_sheet)) input$cftp_sheet else 1
    cftp_nameplate_choices(cftp_path, sheet = cftp_sheet)
  })
  
  output$cftp_nameplate_ui <- renderUI({
    req(input$cftp_file)
    choices <- cftp_nameplates()
    
    validate(need(length(choices) > 0, "No Final_Nameplate values found in the CFTP file."))
    
    selectizeInput(
      "cftp_nameplate",
      "Final Nameplate",
      choices = choices,
      selected = choices[1],
      options = list(dropdownParent = "body", maxOptions = 1000)
    )
  })
  
  previous_model_sheets <- reactive({
    req(input$previous_model_report)
    previous_path <- materialize_upload(input$previous_model_report, "Previous Model Report")
    previous_sheet_defaults(previous_path)
  })
  
  output$previous_contribution_sheet_ui <- renderUI({
    req(input$previous_model_report)
    defaults <- previous_model_sheets()
    selectizeInput(
      "previous_contribution_sheet",
      "Previous Contribution Sheet",
      choices = defaults$sheets,
      selected = defaults$contribution_sheet,
      options = list(dropdownParent = "body")
    )
  })
  
  output$previous_roi_sheet_ui <- renderUI({
    req(input$previous_model_report)
    defaults <- previous_model_sheets()
    selectizeInput(
      "previous_roi_sheet",
      "Previous ROI Sheet",
      choices = defaults$sheets,
      selected = defaults$roi_sheet,
      options = list(dropdownParent = "body")
    )
  })
  
  previous_model_report <- reactive({
    if (is.null(input$previous_model_report)) {
      return(NULL)
    }
    
    tryCatch({
      defaults <- previous_model_sheets()
      contribution_sheet <- input$previous_contribution_sheet %||% defaults$contribution_sheet
      roi_sheet <- input$previous_roi_sheet %||% defaults$roi_sheet
      load_previous_model_report(
        materialize_upload(input$previous_model_report, "Previous Model Report"),
        contribution_sheet = contribution_sheet,
        roi_sheet = roi_sheet,
        aggregation_method = input$aggregation_method %||% "sum"
      )
    }, error = function(e) {
      showNotification(paste("Previous model report could not be loaded:", conditionMessage(e)), type = "error", duration = 12)
      NULL
    })
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
    optional_file_row <- function(label, file) {
      if (!is.null(file)) {
        return(file_row(label, file))
      }
      tags$div(
        class = "qa-summary qa-summary-pending",
        tags$div(
          class = "qa-summary-main",
          tags$span(class = "qa-summary-icon", HTML("&#8226;")),
          tags$span(
            class = "qa-summary-text",
            tags$strong(label),
            tags$br(),
            "Optional - percentages will be recalculated if omitted"
          )
        )
      )
    }

    ready <- files$input_format %in% c("legacy", "new")
    format_label <- if (identical(files$input_format, "new")) {
      "New output format detected"
    } else if (identical(files$input_format, "legacy")) {
      "Legacy format detected"
    } else {
      "Missing files"
    }

    tags$div(
      class = "sidebar-file-status",
      file_row("MFF / Data Input", files$data_input),
      if (identical(files$input_format, "new") || !is.null(files$predictions)) file_row("predictions.csv", files$predictions),
      if (identical(files$input_format, "new") || !is.null(files$contributions)) file_row("contributions.csv", files$contributions),
      if (identical(files$input_format, "new") || !is.null(files$contribution_summary)) optional_file_row("contribution_summary.csv", files$contribution_summary),
      if (!identical(files$input_format, "new")) file_row("Contributions", files$med_contrib),
      if (!identical(files$input_format, "new")) file_row("Contribution Percentages", files$pct_contrib),
      tags$div(
        class = if (ready) "qa-summary qa-summary-ok" else "qa-summary qa-summary-warning",
        tags$div(
          class = "qa-summary-main",
          tags$span(class = "qa-summary-text",
                    if (ready) paste(format_label, "- click Run Analysis.")
                    else "Upload a complete legacy set or new output set before running the analysis.")
        )
      )
    )
  })

  data_loaded <- eventReactive(input$run_analysis, {
    tryCatch({
      files <- selected_files()
      if (identical(files$input_format, "new")) {
        validate(
          need(!is.null(files$data_input), "Upload the MFF / Data Input file."),
          need(!is.null(files$predictions), "Upload predictions.csv."),
          need(!is.null(files$contributions), "Upload contributions.csv.")
        )

        return(load_model_data_from_new_outputs(
          mff_path = materialize_upload(files$data_input, "MFF / Data Input"),
          predictions_path = materialize_upload(files$predictions, "predictions.csv"),
          contributions_path = materialize_upload(files$contributions, "contributions.csv"),
          contribution_summary_path = if (!is.null(files$contribution_summary)) {
            materialize_upload(files$contribution_summary, "contribution_summary.csv")
          } else {
            NULL
          }
        ))
      }

      validate(
        need(!is.null(files$data_input), "Upload the MFF / Data Input file."),
        need(!is.null(files$med_contrib), "Upload the Contributions file."),
        need(!is.null(files$pct_contrib), "Upload the Contribution Percentages file.")
      )

      load_model_data(
        data_input_path = materialize_upload(files$data_input, "MFF / Data Input"),
        med_contrib_path = materialize_upload(files$med_contrib, "Contributions"),
        pct_contrib_path = materialize_upload(files$pct_contrib, "Contribution Percentages")
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
      
      validate(
        need(!is.null(input$cftp_file), "Upload the Consumer Facing Transaction Price AVG file."),
        need(!is.null(input$cftp_nameplate) && nzchar(input$cftp_nameplate), "Select a Final Nameplate for CFTP revenue.")
      )

      gradient_path <- if (isTRUE(input$use_gradient) && !is.null(input$gradient_file)) {
        materialize_upload(input$gradient_file, "Gradient file")
      } else {
        NULL
      }

      gradient_sheet <- if (!is.null(input$gradient_sheet) && nzchar(input$gradient_sheet)) {
        input$gradient_sheet
      } else {
        1
      }
      
      cftp_path <- materialize_upload(input$cftp_file, "Consumer Facing Transaction Price AVG")
      cftp_sheet <- if (!is.null(input$cftp_sheet) && nzchar(input$cftp_sheet)) {
        input$cftp_sheet
      } else {
        1
      }

      result <- build_analysis(
        data_loaded = loaded,
        cutoff_date = input$cutoff_date,
        aggregation_method = input$aggregation_method,
        roi_from = if (isTRUE(input$compare_new_period)) input$roi_range[1] else NULL,
        roi_to = if (isTRUE(input$compare_new_period)) input$roi_range[2] else NULL,
        compare_new_period = input$compare_new_period,
        use_gradient = input$use_gradient,
        gradient_path = gradient_path,
        gradient_sheet = gradient_sheet,
        cftp_path = cftp_path,
        cftp_sheet = cftp_sheet,
        cftp_nameplate = input$cftp_nameplate
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
        status("CFTP", result$cftp_message),
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
  }, server = FALSE)

  output$overview_gradient_metrics_card <- renderUI({
    result <- analysis()
    if (!isTRUE(result$gradient_applied)) {
      return(NULL)
    }

    card("Model Metrics with Gradient", DTOutput("overview_metrics_gradient"))
  })
  
  previous_model_comparison <- reactive({
    previous_report <- previous_model_report()
    if (is.null(previous_report)) {
      return(NULL)
    }
    
    result <- tryCatch(analysis(), error = function(e) NULL)
    if (is.null(result)) {
      return(NULL)
    }
    
    tryCatch({
      build_previous_model_comparison(result, previous_report)
    }, error = function(e) {
      showNotification(paste("Previous model comparison failed:", conditionMessage(e)), type = "error", duration = 12)
      NULL
    })
  })
  
  output$has_previous_comparison <- reactive({
    !is.null(previous_model_comparison())
  })
  outputOptions(output, "has_previous_comparison", suspendWhenHidden = FALSE)

  output$overview_metrics_gradient <- renderDT({
    result <- analysis()
    validate(need(isTRUE(result$gradient_applied), "Gradient adjustment was not applied."))

    metrics <- result$overview_metrics_gradient %>%
      mutate(Value = round(Value, 2)) %>%
      tidyr::pivot_wider(
        names_from = Granularity,
        values_from = Value
      ) %>%
      select(Metric, Daily, Weekly, Monthly)

    metrics_matrix_table(metrics)
  }, server = FALSE)

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
  
  output$fit_error_behavior <- renderPlotly({
    result <- analysis()
    granularity <- input$fit_granularity %||% "Daily"
    build_error_behavior_plot(model_fit_data(result, granularity), granularity)
  })
  
  output$previous_model_state <- renderUI({
    if (is.null(input$previous_model_report)) {
      return(tags$div(class = "comparison-empty", "Upload a previous model report to compare."))
    }
    
    comparison <- previous_model_comparison()
    if (is.null(comparison)) {
      return(tags$div(class = "comparison-empty", "Run the analysis to compare against the previous model report."))
    }
    
    previous_report <- previous_model_report()
    tags$div(
      class = "comparison-empty",
      paste(
        "Comparing current model against:", comparison$filename,
        "| Contribution sheet:", previous_report$contribution_sheet,
        "| ROI sheet:", previous_report$roi_sheet,
        "| Previous prediction:", previous_report$prediction_source,
        "|", comparison$weekly_message
      )
    )
  })
  
  output$previous_metric_summary_table <- renderDT({
    comparison <- previous_model_comparison()
    validate(need(!is.null(comparison), "Upload a previous model report to compare."))
    granularity <- input$previous_compare_granularity %||% "Weekly"
    dt_table(comparison$metrics %>% filter(Granularity == granularity), page_length = 10)
  }, server = FALSE)
  
  output$previous_variable_table <- renderDT({
    comparison <- previous_model_comparison()
    validate(need(!is.null(comparison), "Upload a previous model report to compare."))
    dt_table(comparison$variable, page_length = 25)
  }, server = FALSE)
  
  output$previous_fit_plot <- renderPlotly({
    comparison <- previous_model_comparison()
    validate(need(!is.null(comparison), "Upload a previous model report to compare."))
    granularity <- input$previous_compare_granularity %||% "Weekly"
    build_comparison_fit_plot(comparison, granularity)
  })
  
  output$previous_error_plot <- renderPlotly({
    comparison <- previous_model_comparison()
    validate(need(!is.null(comparison), "Upload a previous model report to compare."))
    granularity <- input$previous_compare_granularity %||% "Weekly"
    build_comparison_error_plot(comparison, granularity)
  })
  
  output$roi_version_switch <- renderUI({
    result <- analysis()
    if (!isTRUE(result$gradient_applied)) {
      return(NULL)
    }

    tags$div(
      class = "table-version-switch ds-pill-group",
      radioButtons(
        "roi_version",
        "Version",
        choices = c("No Gradient" = "base", "Gradient" = "gradient"),
        selected = "base",
        inline = TRUE
      )
    )
  })

  output$roi_table <- renderDT({
    result <- analysis()
    table_data <- if (isTRUE(result$gradient_applied) && identical(input$roi_version, "gradient")) {
      result$roi_table_gradient
    } else {
      result$roi_table
    }

    dt_table(table_data, page_length = 15)
  }, server = FALSE)

  output$full_period_version_switch <- renderUI({
    result <- analysis()
    if (!isTRUE(result$gradient_applied)) {
      return(NULL)
    }

    tags$div(
      class = "table-version-switch ds-pill-group",
      radioButtons(
        "full_period_version",
        "Version",
        choices = c("No Gradient" = "base", "Gradient" = "gradient"),
        selected = "base",
        inline = TRUE
      )
    )
  })

  output$full_period_table <- renderDT({
    result <- analysis()
    validate(need(isTRUE(result$compare_new_period), "Enable Compare New Period to view Full Period Contribution."))
    table_data <- if (isTRUE(result$gradient_applied) && identical(input$full_period_version, "gradient")) {
      result$full_period_table_gradient
    } else {
      result$full_period_table
    }

    dt_table(table_data, page_length = 15)
  }, server = FALSE)

  output$historical_table <- renderDT({
    dt_table(head(analysis()$historical_table, 100), page_length = 10)
  }, server = FALSE)

  output$pre_vs_post_table <- renderDT({
    result <- analysis()
    validate(need(isTRUE(result$compare_new_period), "Enable Compare New Period to view Pre vs Post."))
    dt_table(result$pre_vs_post_table, page_length = 15)
  }, server = FALSE)

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
        "CFTP",
        result$cftp_message,
        paste("CFTP months:", paste(format(sort(unique(result$cftp_data$Month)), "%Y-%m"), collapse = ", ")),
        paste(
          "Missing ROI CFTP months:",
          if (length(result$cftp_missing_months) > 0) paste(result$cftp_missing_months, collapse = ", ") else "None"
        ),
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
        previous_comparison = previous_model_comparison()
      )
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_long_format <- downloadHandler(
    filename = function() {
      paste0("long_format_contributions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(analysis()$long_format_table, file)
    }
  )

}
