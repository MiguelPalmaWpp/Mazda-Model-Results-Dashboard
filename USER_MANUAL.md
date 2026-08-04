# Mazda Model Results Dashboard - User Manual

## 1. Overview

The Mazda Model Results Dashboard is a Shiny application used to review model performance, contributions, ROI, diagnostics, and exportable model result reports.

Use this application to:

-   Upload Mazda model output files.
-   Review model fit at daily, weekly, and monthly levels.
-   Analyze ROI using spend, model contribution units, and monthly Consumer Facing Transaction Price data.
-   Compare pre/post periods when needed.
-   Apply optional gradient adjustments.
-   Export Excel, correlation, and long-format contribution files.

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

For the new format, the MFF must be the exact file used by the model run. The app matches model output rows back to the uploaded MFF row order.

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

## 5. Running the Analysis

1.  Upload the model files.
2.  Upload the CFTP file.
3.  Select the CFTP sheet and final nameplate.
4.  Optional: enable **Compare New Period**.
5.  Optional: enable **Apply Gradient Adjustment**.
6.  Choose the aggregation method: `sum` or `mean`.
7.  Click **Run Analysis**.

The app will load the files, calculate metrics, build contribution and ROI tables, and enable downloads.

## 6. Tabs and Outputs

### Overview

Shows the executive summary, date range, input row counts, number of variables, ROI period, CFTP status, gradient status, and model metrics.

### Model Fit

Shows:

-   Time series chart for actual vs predicted values.
-   Scatter plot for actual vs predicted values.
-   Granularity options: Daily, Weekly, Monthly.

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

### Historical Contributions

Shows a preview of historical contribution values by date.

### Pre vs Post

Shows contribution units before and after the selected cutoff date when **Compare New Period** is enabled.

### Diagnostics

Shows file detection details, detected columns, date ranges, CFTP information, missing CFTP months, and correlation split output.

### Metrics Over Time

Shows monthly model performance metrics.

## 7. ROI Calculation

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

## 8. Excel Report

### Download Excel Report

Creates an Excel workbook with model results, metrics, ROI, full-period contribution, historical contributions, and pre/post tables.

The export reuses the already calculated ROI tables from **Run Analysis** to reduce download time.

### Download Long Format CSV

Exports the precomputed long-format contribution table for downstream analysis. This includes contribution, spend, and gradient-adjusted contribution fields when available.

## 9. Troubleshooting

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

Large model files can still take time to write, especially if many sheets and rows are included. Run Analysis first and wait for completion before exporting.

## 10. Recommended Workflow

1.  Upload all required model files.
2.  Upload the CFTP file.
3.  Select the correct CFTP sheet and final nameplate.
4.  Review Diagnostics after running the analysis.
5.  Check the ROI table for missing revenue or spend.
6.  Review Model Fit charts.
7.  Download the Excel report for final review or sharing.
