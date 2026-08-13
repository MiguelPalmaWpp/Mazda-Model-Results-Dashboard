ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "custom.js"),
    tags$style(HTML("
      .mazda-app-shell { padding: 22px 28px 32px; }
      .mazda-sidebar { display: grid; gap: 16px; }
      .mazda-content { min-width: 0; }
      .mazda-layout { display: grid; gap: 20px; grid-template-columns: 360px minmax(0, 1fr); align-items: start; }
      .mazda-sidebar .card { margin-bottom: 0; overflow: visible; }
      .mazda-sidebar .card-header { padding: 10px 14px 8px; font-size: 13px; }
      .mazda-sidebar .card-body { padding: 12px 14px; }
      .sidebar-card-inputs .card-body { min-height: 245px; max-height: 315px; overflow-y: auto; }
      .sidebar-card-settings,
      .sidebar-card-settings .card-body { overflow: visible !important; position: relative; z-index: 30; }
      .sidebar-card-settings .card-body { min-height: 165px; }
      .sidebar-card-gradient .card-body { min-height: 86px; max-height: 210px; overflow-y: auto; }
      .sidebar-card-previous .card-body { min-height: 260px; max-height: 430px; overflow-y: auto; }
      .sidebar-card-export .card-body { min-height: 148px; }
      .mazda-sidebar .form-group { margin-bottom: 10px; }
      .mazda-sidebar label { font-size: 12px; margin-bottom: 4px; }
      .mazda-sidebar .shiny-input-container,
      .mazda-sidebar .input-group,
      .mazda-sidebar .selectize-control { width: 100% !important; max-width: 100%; min-width: 0; }
      .mazda-sidebar .form-control,
      .mazda-sidebar .selectize-input { min-height: 30px !important; padding: 5px 8px !important; font-size: 12px !important; }
      .mazda-sidebar .input-group .form-control,
      .mazda-sidebar input.form-control[readonly] {
        min-width: 0;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .mazda-sidebar .selectize-input {
        align-items: center;
        display: flex !important;
        flex-wrap: nowrap !important;
        max-width: 100%;
        overflow: hidden;
      }
      .mazda-sidebar .selectize-input .item {
        min-width: 0;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .selectize-dropdown {
        z-index: 100000 !important;
        max-width: min(360px, calc(100vw - 24px)) !important;
      }
      .selectize-dropdown-content {
        max-height: 220px !important;
      }
      .mazda-sidebar .btn { font-size: 11.5px; padding: 5px 10px; }
      .mazda-reset-btn { width: 100%; margin: 4px 0 8px; }
      .sidebar-step-title {
        align-items: center;
        display: inline-flex;
        gap: 8px;
      }
      .sidebar-step-number {
        align-items: center;
        background: #5B9BD5;
        border-radius: 999px;
        color: #fff;
        display: inline-flex;
        font-size: 10px;
        font-weight: 800;
        height: 20px;
        justify-content: center;
        line-height: 1;
        width: 20px;
      }
      .sidebar-file-status { display: grid; gap: 6px; margin-top: 8px; }
      .sidebar-file-status .qa-summary { margin-bottom: 0; padding: 7px 9px; border-radius: 6px; }
      .sidebar-file-status .qa-summary-text { font-size: 10.5px; line-height: 1.15; }
      .sidebar-file-status .qa-summary-icon { font-size: 11px; }
      .mazda-main-tabs .tab-content { padding-top: 18px; }
      .mazda-main-tabs .nav-tabs {
        border-bottom: 2px solid #dee2e6;
        display: flex;
        flex-wrap: wrap;
        gap: 4px 6px;
        overflow: visible;
      }
      .mazda-main-tabs .nav-tabs > li { float: none; max-width: 100%; }
      .mazda-main-tabs .nav-tabs > li > a {
        color: #6c757d !important;
        font-size: 13.5px;
        font-weight: 500;
        border-radius: 0 !important;
        max-width: 220px;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .mazda-main-tabs .nav-tabs > li.active > a,
      .mazda-main-tabs .nav-tabs > li.active > a:focus,
      .mazda-main-tabs .nav-tabs > li.active > a:hover {
        color: #5B9BD5 !important;
        border: 0;
        border-bottom: 3px solid #5B9BD5;
        background: transparent;
        font-weight: 600;
      }
      .mazda-downloads .btn { width: 100%; margin-bottom: 8px; }
      .mazda-run-btn { width: 100%; margin-top: 4px; }
      .table { font-size: 12.5px; }
      .table-version-switch {
        align-items: center;
        display: flex;
        justify-content: flex-start;
        margin: 2px 0 14px;
      }
      .table-version-switch .form-group {
        margin-bottom: 0;
      }
      .table-version-switch label.control-label {
        color: #64748b;
        font-size: 11.5px;
        font-weight: 700;
        margin-right: 10px;
        text-transform: uppercase;
      }
      .model-fit-card .card-body { padding-top: 18px; }
      .model-fit-card .js-plotly-plot { width: 100% !important; }
      .model-fit-grid {
        display: grid;
        gap: 16px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
      .comparison-empty {
        background: #f8fbff;
        border: 1px solid #dbe7f3;
        border-left: 4px solid #5B9BD5;
        border-radius: 7px;
        color: #475569;
        font-size: 13px;
        padding: 14px 16px;
      }
      .dataTables_wrapper { font-size: 12px; }
      .dt-toolbar {
        align-items: center;
        display: flex;
        gap: 12px;
        justify-content: space-between;
        margin-bottom: 12px;
      }
      .dt-search-wrap .dataTables_filter { margin: 0; }
      .dt-search-wrap .dataTables_filter label { margin: 0; }
      .dt-search-wrap .dataTables_filter input {
        border: 1px solid #cfdae8 !important;
        border-radius: 7px !important;
        color: #1e293b;
        font-size: 12px;
        min-height: 32px;
        padding: 6px 10px !important;
        width: 220px !important;
      }
      table.mazda-dt {
        border-collapse: separate !important;
        border-spacing: 0 !important;
        width: 100% !important;
      }
      table.mazda-dt thead th {
        background: #f8fbff !important;
        border-bottom: 2px solid #5B9BD5 !important;
        color: #1e293b !important;
        font-size: 12px !important;
        font-weight: 750 !important;
        padding: 9px 10px !important;
        white-space: nowrap;
      }
      table.mazda-dt tbody td {
        border-bottom: 1px solid #edf3f9 !important;
        color: #243447;
        font-size: 12px;
        padding: 8px 10px !important;
        vertical-align: middle;
        white-space: nowrap;
      }
      table.mazda-dt tbody tr:hover td { background: #f8fbff !important; }
      table.mazda-dt td.dt-right,
      table.mazda-dt th.dt-right {
        font-variant-numeric: tabular-nums;
        text-align: right !important;
      }
      .dt-footer {
        align-items: center;
        display: flex;
        justify-content: space-between;
        margin-top: 10px;
      }
      .dt-info-wrap .dataTables_info {
        color: #64748b;
        font-size: 11.5px;
        padding-top: 0 !important;
      }
      table.metrics-matrix {
        border-collapse: separate !important;
        border-spacing: 0 !important;
        width: 100% !important;
      }
      table.metrics-matrix thead th {
        background: #f8fbff !important;
        border-bottom: 2px solid #5B9BD5 !important;
        color: #1e293b !important;
        font-size: 12px !important;
        font-weight: 700 !important;
        padding: 10px 12px !important;
      }
      table.metrics-matrix tbody td {
        border-bottom: 1px solid #edf3f9 !important;
        color: #243447;
        font-size: 12.5px;
        padding: 9px 12px !important;
      }
      table.metrics-matrix tbody tr:hover td {
        background: #f8fbff !important;
      }
      table.metrics-matrix tbody td:first-child {
        color: #1e293b;
        font-weight: 650;
      }
      .overview-grid {
        display: grid;
        gap: 12px;
        grid-template-columns: repeat(4, minmax(0, 1fr));
      }
      .overview-tile {
        background: #ffffff;
        border: 1px solid #dbe7f3;
        border-left: 4px solid #5B9BD5;
        border-radius: 7px;
        padding: 12px 14px;
        min-height: 82px;
      }
      .overview-tile-label {
        color: #64748b;
        display: block;
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.02em;
        text-transform: uppercase;
      }
      .overview-tile-value {
        color: #1e293b;
        display: block;
        font-size: 19px;
        font-weight: 700;
        line-height: 1.15;
        margin-top: 7px;
      }
      .overview-tile-note {
        color: #64748b;
        display: block;
        font-size: 11.5px;
        margin-top: 4px;
      }
      .overview-status-row {
        display: grid;
        gap: 10px;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        margin-top: 12px;
      }
      .overview-status {
        background: #f8fbff;
        border: 1px solid #dbe7f3;
        border-radius: 7px;
        padding: 10px 12px;
      }
      .overview-status strong {
        color: #1e293b;
        display: block;
        font-size: 12px;
      }
      .overview-status span {
        color: #64748b;
        display: block;
        font-size: 11.5px;
        margin-top: 3px;
      }
      .roi-method-panel {
        background: #f8fbff;
        border: 1px solid #dbe7f3;
        border-left: 4px solid #5B9BD5;
        border-radius: 7px;
        display: grid;
        gap: 10px;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        margin-bottom: 14px;
        padding: 12px 14px;
      }
      .roi-method-item {
        color: #475569;
        font-size: 12px;
        line-height: 1.35;
        min-width: 0;
      }
      .roi-method-kicker {
        color: #1e293b;
        display: block;
        font-size: 11px;
        font-weight: 800;
        letter-spacing: 0.02em;
        margin-bottom: 3px;
        text-transform: uppercase;
      }
      @media (max-width: 1100px) {
        .mazda-layout { grid-template-columns: 1fr; }
        .model-fit-grid { grid-template-columns: 1fr; }
        .overview-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        .overview-status-row { grid-template-columns: 1fr; }
        .roi-method-panel { grid-template-columns: 1fr; }
      }
      @media (max-width: 640px) {
        .overview-grid { grid-template-columns: 1fr; }
      }
    "))
  ),
  tags$header(
    class = "wpp-app-header",
    tags$div(
      class = "wpp-header-brand",
      tags$img(src = "img/logo.png", alt = "Logo")
    ),
    tags$div(
      class = "navbar-center-block",
      tags$span(class = "app-main-title", "Mazda Model Results Dashboard"),
      tags$span(class = "app-subtitle", "By Advanced Analytics Colombia")
    ),
    tags$div(
      class = "wpp-header-right",
      tags$img(src = "img/logo.png", alt = "")
    )
  ),
  tags$main(
    class = "mazda-app-shell",
    tags$div(
      class = "mazda-layout",
      tags$aside(
        class = "mazda-sidebar",
        card(
          step_title("1", "Model Files"),
          fileInput(
            "all_files",
            "Upload MFF and model output files",
            multiple = TRUE,
            accept = c(".csv", ".xlsx", ".xlsm", ".xls")
          ),
          actionButton("reset_analysis_files", "Reset Model Files", class = "btn-outline-secondary mazda-reset-btn"),
          uiOutput("file_status"),
          class = "sidebar-card-inputs"
        ),
        card(
          step_title("2", "Revenue and Period"),
          checkboxInput("compare_new_period", "Compare New Period", value = FALSE),
          conditionalPanel(
            "input.compare_new_period",
            dateInput("cutoff_date", "Cutoff Date", value = DEFAULT_CUTOFF_DATE),
            dateRangeInput(
              "roi_range",
              "ROI Contribution Date Range",
              start = DEFAULT_ROI_FROM,
              end = DEFAULT_ROI_TO
            )
          ),
          fileInput(
            "cftp_file",
            "Consumer Facing Transaction Price AVG",
            accept = c(".csv", ".xlsx", ".xlsm", ".xls")
          ),
          uiOutput("cftp_sheet_ui"),
          uiOutput("cftp_nameplate_ui"),
          selectizeInput(
            "aggregation_method",
            "Aggregation Method",
            choices = c("sum", "mean"),
            selected = "sum",
            options = list(dropdownParent = "body")
          ),
          selectizeInput(
            "weekly_grouping",
            "Weekly Grouping",
            choices = c(
              "Forward from Sunday (Sun-Sat)" = "forward_from_sunday",
              "Backward to Sunday (Mon-Sun)" = "backward_to_sunday"
            ),
            selected = "forward_from_sunday",
            options = list(dropdownParent = "body")
          ),
          class = "sidebar-card-settings"
        ),
        card(
          step_title("3", "Gradient Adjustment"),
          checkboxInput("use_gradient", "Apply Gradient Adjustment", value = FALSE),
          conditionalPanel(
            "input.use_gradient",
            fileInput("gradient_file", "Gradient File", accept = c(".csv", ".xlsx", ".xlsm", ".xls")),
            uiOutput("gradient_sheet_ui")
          ),
          class = "sidebar-card-gradient"
        ),
        card(
          step_title("4", "Previous Model Report"),
          radioButtons(
            "previous_model_mode",
            "Previous Model Format",
            choices = c("Excel Report" = "excel_report", "Long Format" = "long_format"),
            selected = "excel_report"
          ),
          uiOutput("previous_model_upload_ui"),
          uiOutput("previous_model_options_ui"),
          class = "sidebar-card-previous"
        ),
        card(
          step_title("5", "Run and Export"),
          actionButton("run_analysis", "Run Analysis", class = "btn-primary mazda-run-btn"),
          tags$hr(),
                  tags$div(
                    class = "mazda-downloads",
                    downloadButton("download_excel", "Download Excel Report"),
                    downloadButton("download_long_format", "Download Long Format CSV")
                  ),
                  class = "sidebar-card-export"
                )
      ),
      tags$section(
        class = "mazda-content",
        tags$div(
          class = "mazda-main-tabs",
          tabsetPanel(
            id = "main_tabs",
            tabPanel(
              "Overview",
              card("Executive Summary", uiOutput("overview_summary")),
              card("Model Metrics", DTOutput("overview_metrics")),
              uiOutput("overview_gradient_metrics_card")
            ),
            tabPanel(
              "Model Fit",
              tags$div(
                class = "model-fit-card",
                card(
                  "Model Fit",
                  tags$div(
                    class = "ds-pill-group",
                    radioButtons(
                      "fit_granularity",
                      "Granularity",
                      choices = c("Daily", "Weekly", "Monthly"),
                      selected = "Daily",
                      inline = TRUE
                    )
                  ),
                  plotlyOutput("fit_timeseries", height = "430px")
                )
              ),
              tags$div(
                class = "model-fit-grid",
                tags$div(
                  class = "model-fit-card",
                  card("Actual vs Predicted Scatter", plotlyOutput("fit_scatter", height = "410px"))
                ),
                tags$div(
                  class = "model-fit-card",
                  card("Error Behavior", plotlyOutput("fit_error_behavior", height = "410px"))
                )
              )
            ),
            tabPanel(
              "ROI",
              card(
                "Filtered Period ROI",
                roi_method_panel(),
                uiOutput("roi_version_switch"),
                DTOutput("roi_table")
              )
            ),
            tabPanel(
              "Compare Previous Model",
              uiOutput("previous_model_state"),
              conditionalPanel(
                "output.has_previous_comparison",
                card(
                  "Metric Summary",
                  tags$div(
                    class = "ds-pill-group",
                    radioButtons(
                      "previous_compare_granularity",
                      "Granularity",
                      choices = c("Daily", "Weekly", "Monthly"),
                      selected = "Weekly",
                      inline = TRUE
                    )
                  ),
                  DTOutput("previous_metric_summary_table")
                ),
                card("Variable Comparison", DTOutput("previous_variable_table")),
                tags$div(
                  class = "model-fit-grid",
                  tags$div(
                    class = "model-fit-card",
                    card("Actual vs Predicted", plotlyOutput("previous_fit_plot", height = "410px"))
                  ),
                  tags$div(
                    class = "model-fit-card",
                    card("Residual Comparison", plotlyOutput("previous_error_plot", height = "410px"))
                  )
                )
              )
            ),
            tabPanel("Historical Contributions", card("Historical Contributions Preview", DTOutput("historical_table"))),
            tabPanel("Diagnostics", card("Diagnostics", verbatimTextOutput("diagnostics")))
          )
        )
      )
    )
  )
)
