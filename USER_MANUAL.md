# Mazda Model Results Dashboard - User Manual

## 1. Overview

The Mazda Model Results Dashboard is a Shiny application used to review model performance, contributions, ROI, diagnostics, and exportable model result reports.

Use this application to:

-   Upload Mazda model output files.
-   Review model fit at daily, weekly, and monthly levels.
-   Analyze ROI using spend, model contribution units, and monthly Consumer Facing Transaction Price data.
-   Compare the current model against a previous model report.
-   Compare pre/post periods when needed.
-   Apply optional gradient adjustments.
-   Export Excel and long-format contribution files.

## 2. Required Input Files

The app supports two model output formats.

### Legacy Format

Upload these files in the **Upload MFF and model output files** control:

-   **MFF / Data Input**: CSV or Excel file with `Date`, actual KPI, and spend columns.
-   **Contributions**: CSV or Excel file with `Date`, `Pred`, and `Contrib_` columns.
-   **Contribution Percentages**: CSV or Excel file with contribution percentage values.

### New Model Output Format

Upload these files in the **Upload MFF and model output files** control:

-   **MFF / Data Input**: CSV or Excel file with `Date`, actual KPI, and spend columns.
-   **predictions.csv**: must contain `row`, `observed`, and `fitted`.
-   **contributions.csv**: must contain `row` and model contribution columns.
-   **contribution_summary.csv**: optional file with `label` and `share_total`.

For the new format, the MFF must be the exact file used by the model run. The app validates model output `row` values against the MFF KPI using `predictions.csv$observed`, detects any constant row offset, and reports the matched MFF row range in Diagnostics.

## 3. CFTP Input for ROI

ROI revenue is calculated using a separate **Consumer Facing Transaction Price AVG** file.

Upload this file in the **Analysis Settings** panel.

The file must contain these columns:

-   `Period`
-   `Date`
-   `Final_Nameplate`
-   `AVG_CFTP`

The app uses `Date` to determine the month and joins monthly CFTP values to daily model contribution units.

After uploading the CFTP file:

1.  Select the CFTP sheet if the file is Excel.
2.  Select the `Final_Nameplate` to use for the analysis.
3.  Run the analysis.

If a model month is missing from the selected CFTP nameplate, revenue and ROI will remain blank for rows that cannot be calculated, and the missing months will appear in Diagnostics.

## 4. Optional Gradient Adjustment

Use **Apply Gradient Adjustment** only when you need to adjust contribution values by month.

The gradient file must include:

-   `Month`
-   `Gradient`

If the file is Excel, select the correct sheet after upload.

When gradient is applied, the app shows switch controls so you can compare:

-   No Gradient
-   Gradient

## 5. Optional Previous Model Comparison

Use **Previous Model Report** when you want to compare the current run against an earlier model output.

Choose the previous model format:

-   `Excel Report`: a current app report with `Summary`, `ROI`, and optionally `Full Period Contribution`, or a legacy metrics report with `Daily`, `Weekly`, `Monthly`, `ROI`, and `pre_vs_post`.
-   `Long Format`: an Excel sheet or CSV file with daily rows by variable.

The upload control changes with the selected format, so only the relevant previous model file type is shown.

For `Excel Report`, the app detects and lets you edit:

-   `Previous Contribution Sheet`: usually `Historical Contributions` or `med_contrib`.
-   `Previous ROI Sheet`: usually `ROI`.

For `Long Format`, use a file that contains `Date`, `Nameplate`, `Variable`, `Contribution`, `Spend`, `Revenue`, `Category`, `Sub-Category`, `Funnel`, and `Channel`. Optional columns such as `AVG_MRSP`, `FY`, and `Quarter` are used when available. `Contribution` is always used for previous model comparison; `Contribution_gradient` is not used to rebuild previous prediction or ROI. For Excel, select the sheet first. For CSV, sheet selection is skipped. The app reads only the `Nameplate` column before analysis, shows a `Previous Nameplate` dropdown, and then processes only the selected nameplate when you click **Run Analysis**.

For large previous long-format files, CSV is recommended because it loads much faster than Excel.

The comparison rebuilds previous prediction from daily contributions and uses the current model's `Actual` values on common dates. In long format mode, previous units, spend, revenue, and ROI are recalculated on the same common dates used for current ROI.

## 6. Running the Analysis

1.  Upload the model files.
2.  Upload the CFTP file.
3.  Select the CFTP sheet and final nameplate.
4.  Optional: enable **Compare New Period**.
5.  Optional: enable **Apply Gradient Adjustment**.
6.  Optional: upload a **Previous Model Report**.
7.  Choose the aggregation method: `sum` or `mean`.
8.  Click **Run Analysis**.

The app will load the files, calculate metrics, build contribution and ROI tables, and enable downloads.

## 7. Tabs and Outputs

### Overview

Shows the executive summary, date range, input row counts, number of variables, ROI period, CFTP status, gradient status, and model metrics.

### Model Fit

Shows:

-   Time series chart for actual vs predicted values.
-   Scatter plot for actual vs predicted values.
-   Error behavior chart with residuals over time and residual distribution.
-   Granularity options: Daily, Weekly, Monthly. Weekly periods start on Sunday.

### ROI

Shows the ROI table for the selected ROI period.

Important ROI columns:

-   `Units`: total contribution units for the variable.
-   `% Contribution`: contribution share for the period.
-   `Model Contribution`: summed by `Category`.
-   `Spend`: matched spend from the MFF / Data Input.
-   `Revenue`: calculated from positive daily contribution units and monthly CFTP.
-   `ROI`: calculated as `Revenue / Spend`.
-   `Category`, `Sub-Category`, `Funnel`, `Channel`: classification fields from mapping rules.

Rows in `Category = Base` are always sorted last.

### Compare Previous Model

Shows comparison outputs when a previous model report is uploaded:

-   metric summary for the selected granularity;
-   variable comparison with current/previous units, spend, and ROI;
-   comparison status based on whether unit difference is within or above 10%;
-   explicit Weekly match status when weeks do not fully align.

### Historical Contributions

Shows a preview of historical contribution values by date.

### Pre vs Post

Shows contribution units before and after the selected cutoff date when **Compare New Period** is enabled.

### Full Period Contribution

Shows full-period contribution and ROI only when **Compare New Period** is enabled. If **Compare New Period** is disabled, the `ROI` tab already represents the full available period.

### Diagnostics

Shows file detection details, detected columns, date ranges, CFTP information, missing CFTP months, and correlation split output.

## 8. ROI Calculation

ROI is calculated in these steps:

1.  The app reads the CFTP file and filters it to the selected `Final_Nameplate`.
2.  CFTP `Date` is converted to month.
3.  Daily contribution columns are converted to long format.
4.  Each daily contribution row is matched to the selected monthly `AVG_CFTP`.
5.  Daily revenue is calculated only for positive daily units:

``` text
Daily Revenue = Daily Units * AVG_CFTP
```

6.  Revenue is summed by variable.
7.  Spend is summed from the matching spend column in the MFF / Data Input.
8.  ROI is calculated only when `Units > 0`, revenue exists, spend exists, and spend is not zero:

``` text
ROI = Revenue / Spend
```

Negative contribution rows do not calculate revenue or ROI.

## 9. Excel Report

### Download Excel Report

Creates an Excel workbook with model results, metrics, ROI, full-period contribution, historical contributions, pre/post tables, and model comparison when a previous report is uploaded.

When a previous report is uploaded, the existing `Daily`, `Weekly`, and `Monthly` sheets add `Previous Model` columns next to the current prediction columns, so actual vs current prediction vs previous prediction can be reviewed in the same place.

The export reuses the already calculated ROI tables from **Run Analysis** to reduce download time.

### Download Long Format CSV

Exports the precomputed long-format contribution table for downstream analysis. This includes contribution, spend, monthly CFTP, and gradient-adjusted contribution fields when available.

## 10. Troubleshooting

### The app does not detect my model files

Check that the files match either the legacy format or the new output format. For the new format, `predictions.csv` and `contributions.csv` must contain a valid `row` column.

### CFTP nameplates do not appear

Confirm the CFTP file has the required columns:

-   `Period`
-   `Date`
-   `Final_Nameplate`
-   `AVG_CFTP`

Also confirm that `AVG_CFTP` contains numeric values.

### Revenue or ROI is blank

Revenue and ROI may be blank when:

-   The row has negative or zero contribution units.
-   There is no matching spend column.
-   The selected CFTP nameplate does not have data for the required month.
-   Spend is zero or missing.

### Base appears in the ROI table

Base variables can appear in contribution tables, but `Category = Base` is sorted last.

### Excel export is slow

Large model files can still take time to write, especially if many sheets and rows are included. Run Analysis first and wait for completion before exporting. For previous model long-format comparisons, export the previous long format as CSV when possible; the app can read only the required columns and cache the processed result after Run Analysis.

## 11. Recommended Workflow

1.  Upload all required model files.
2.  Upload the CFTP file.
3.  Select the correct CFTP sheet and final nameplate.
4.  Review Diagnostics after running the analysis.
5.  Check the ROI table for missing revenue or spend.
6.  Review Model Fit charts.
7.  Download the Excel report for final review or sharing.
