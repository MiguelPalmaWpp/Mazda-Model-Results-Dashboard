# Mazda Model Results Dashboard

English-only Shiny application for reviewing Mazda model results, model fit, contribution, ROI, and report exports.

## Inputs

The app uses one multi-file upload and automatically detects the required files by filename.

Legacy format:

- `MFF / Data Input`: CSV or Excel file with `Date`, KPI/Actual, and spend columns.
- `Contributions`: CSV or Excel file with `Date`, `Pred`, and `Contrib_` columns.
- `Contribution Percentages`: CSV or Excel file with variable and percentage columns.

New model output format:

- `MFF / Data Input`: CSV or Excel file with `Date`, KPI/Actual, and spend columns.
- `predictions.csv`: CSV file with `row`, `observed`, and `fitted`.
- `contributions.csv`: CSV file with `row` and model contribution columns.
- `contribution_summary.csv`: optional CSV file with `label` and `share_total`.

For the new format, Shiny creates an internal `Row` from the uploaded MFF row order
and joins it to the model output `row` column. The MFF must be the exact file used by
the model run.

Optional:

- `Gradient File`: CSV or Excel file with `Month` and `Gradient`.

## Outputs

- Excel model results report.
- Correlation CSV.
- Long Format CSV.
- Interactive model-fit charts.
- DT tables for metrics, ROI, contribution, diagnostics, and previews.

## Local Run

From the project root:

```r
shiny::runApp()
```

Or from a terminal:

```powershell
Rscript run_app.R
```

The local helper script runs the app at:

```text
http://127.0.0.1:3838
```

## Repository Structure

```text
.
|-- app.R
|-- functions.R
|-- app_modules/
|   |-- config.R
|   |-- data_loading.R
|   |-- model_fit_plots.R
|   |-- report_tables.R
|   |-- analysis.R
|   |-- ui_components.R
|   |-- ui.R
|   |-- server.R
|   `-- README.md
|-- DESCRIPTION
|-- manifest.json
|-- Mazda-Model-Results-Dashboard.Rproj
|-- run_app.R
|-- www/
|   |-- custom.js
|   |-- styles.css
|   `-- img/logo.png
`-- README.md
```

## Module Map

- `app.R`: app entry point, package loading, module sourcing, and `shinyApp(ui, server)`.
- `functions.R`: reusable Excel, ROI, gradient, and legacy model report helpers.
- `app_modules/config.R`: constants and app configuration.
- `app_modules/data_loading.R`: upload parsing, file detection, and model input loading.
- `app_modules/model_fit_plots.R`: Plotly and ggplot model-fit charts.
- `app_modules/report_tables.R`: DT tables and app report table builders.
- `app_modules/analysis.R`: main analysis orchestration and workbook builder.
- `app_modules/ui_components.R`: reusable UI and table helpers.
- `app_modules/ui.R`: Shiny UI layout.
- `app_modules/server.R`: Shiny server logic, reactivity, rendering, and downloads.

## Posit Connect Deployment

### Option 1: Deploy From RStudio

Open the project folder in RStudio, then publish the Shiny app to Posit Connect. The app root is this folder.

### Option 2: Deploy From R Console

```r
rsconnect::deployApp(
  appDir = ".",
  appName = "mazda-model-results-dashboard"
)
```

### Option 3: Deploy From Git

In Posit Connect, create content from Git and point it to this repository. Use the repository root as the app directory.

## Notes

- Do not commit uploaded model files, generated reports, logs, or local deployment account files.
- The app is configured to accept uploads up to 300 MB.
- Weekly metrics are grouped to Monday using `week_start = 1`.
- Displayed numeric outputs are limited to a maximum of 3 decimals.
