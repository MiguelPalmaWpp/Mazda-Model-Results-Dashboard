ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "custom.js"),
    tags$style(HTML("
      .mazda-app-shell { padding: 22px 28px 32px; }
      .mazda-sidebar { display: grid; gap: 16px; }
      .mazda-content { min-width: 0; }
      .mazda-layout { display: grid; gap: 20px; grid-template-columns: 340px minmax(0, 1fr); }
      .mazda-sidebar .card { margin-bottom: 0; overflow: hidden; }
      .mazda-sidebar .card-header { padding: 10px 14px 8px; font-size: 13px; }
      .mazda-sidebar .card-body { padding: 12px 14px; }
      .sidebar-card-inputs .card-body { min-height: 245px; max-height: 315px; overflow-y: auto; }
      .sidebar-card-settings .card-body { min-height: 165px; }
      .sidebar-card-gradient .card-body { min-height: 86px; max-height: 210px; overflow-y: auto; }
      .sidebar-card-export .card-body { min-height: 148px; }
      .mazda-sidebar .form-group { margin-bottom: 10px; }
      .mazda-sidebar label { font-size: 12px; margin-bottom: 4px; }
      .mazda-sidebar .form-control,
      .mazda-sidebar .selectize-input { min-height: 30px !important; padding: 5px 8px !important; font-size: 12px !important; }
      .mazda-sidebar .btn { font-size: 11.5px; padding: 5px 10px; }
      .sidebar-file-status { display: grid; gap: 6px; margin-top: 8px; }
      .sidebar-file-status .qa-summary { margin-bottom: 0; padding: 7px 9px; border-radius: 6px; }
      .sidebar-file-status .qa-summary-text { font-size: 10.5px; line-height: 1.15; }
      .sidebar-file-status .qa-summary-icon { font-size: 11px; }
      .mazda-main-tabs .tab-content { padding-top: 18px; }
      .mazda-main-tabs .nav-tabs { border-bottom: 2px solid #dee2e6; }
      .mazda-main-tabs .nav-tabs > li > a {
        color: #6c757d !important;
        font-size: 13.5px;
        font-weight: 500;
        border-radius: 0 !important;
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
      .dataTables_wrapper { font-size: 12px; }
      .dt-toolbar {
        align-items: center;
        display: flex;
        gap: 12px;
        justify-content: space-between;
        margin-bottom: 12px;
      }
      .dt-buttons-wrap .dt-buttons { display: flex; gap: 6px; }
      .dt-buttons .btn,
      .dt-button,
      .btn-dt {
        background: #ffffff !important;
        border: 1px solid #5B9BD5 !important;
        border-radius: 6px !important;
        color: #3f7db8 !important;
        font-size: 11px !important;
        font-weight: 650 !important;
        padding: 5px 10px !important;
      }
      .dt-buttons .btn:hover,
      .dt-button:hover,
      .btn-dt:hover {
        background: #eef6ff !important;
        color: #2f679e !important;
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
      @media (max-width: 1100px) {
        .mazda-layout { grid-template-columns: 1fr; }
        .overview-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        .overview-status-row { grid-template-columns: 1fr; }
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
          "Input Files",
          fileInput(
            "all_files",
            "Upload MFF and model output files",
            multiple = TRUE,
            accept = c(".csv", ".xlsx", ".xlsm", ".xls")
          ),
          uiOutput("file_status"),
          class = "sidebar-card-inputs"
        ),
        card(
          "Analysis Settings",
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
          numericInput("revenue_per_unit", "Revenue per Unit", value = DEFAULT_REVENUE_PER_UNIT, min = 0),
          selectInput("aggregation_method", "Aggregation Method", choices = c("sum", "mean"), selected = "sum"),
          class = "sidebar-card-settings"
        ),
        card(
          "Gradient Adjustment",
          checkboxInput("use_gradient", "Apply Gradient Adjustment", value = FALSE),
          conditionalPanel(
            "input.use_gradient",
            fileInput("gradient_file", "Gradient File", accept = c(".csv", ".xlsx", ".xlsm", ".xls")),
            uiOutput("gradient_sheet_ui")
          ),
          class = "sidebar-card-gradient"
        ),
        card(
          "Run and Export",
          actionButton("run_analysis", "Run Analysis", class = "btn-primary mazda-run-btn"),
          tags$hr(),
                  tags$div(
                    class = "mazda-downloads",
                    downloadButton("download_excel", "Download Excel Report"),
                    downloadButton("download_correlation", "Download Correlation CSV"),
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
                class = "model-fit-card",
                card("Actual vs Predicted Scatter", plotlyOutput("fit_scatter", height = "410px"))
              )
            ),
            tabPanel("Metrics Over Time", card("Monthly Metrics", DTOutput("metrics_over_time"))),
            tabPanel(
              "ROI",
              card(
                "Filtered Period ROI",
                uiOutput("roi_version_switch"),
                DTOutput("roi_table")
              )
            ),
            tabPanel("Historical Contributions", card("Historical Contributions Preview", DTOutput("historical_table"))),
            tabPanel("Pre vs Post", card("Pre vs Post Contribution", DTOutput("pre_vs_post_table"))),
            tabPanel("Diagnostics", card("Diagnostics", verbatimTextOutput("diagnostics")))
          )
        )
      )
    )
  )
)
