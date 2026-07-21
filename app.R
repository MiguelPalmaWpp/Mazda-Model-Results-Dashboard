library(shiny)
library(dplyr)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(readr)
library(tidyr)
library(gridExtra)
library(plotly)
library(DT)

options(shiny.maxRequestSize = 300 * 1024^2)

app_env <- environment()
`%||%` <- function(x, y) if (is.null(x)) y else x

resolve_app_root <- function() {
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(source_file) && length(source_file) > 0 && nzchar(source_file)) {
    return(dirname(normalizePath(source_file, winslash = "/", mustWork = TRUE)))
  }

  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

app_root <- resolve_app_root()
app_path <- function(...) file.path(app_root, ...)

source(app_path("functions.R"), local = app_env)
source(app_path("app_modules", "config.R"), local = app_env)
source(app_path("app_modules", "data_loading.R"), local = app_env)
source(app_path("app_modules", "model_fit_plots.R"), local = app_env)
source(app_path("app_modules", "report_tables.R"), local = app_env)
source(app_path("app_modules", "analysis.R"), local = app_env)
source(app_path("app_modules", "ui_components.R"), local = app_env)
source(app_path("app_modules", "ui.R"), local = app_env)
source(app_path("app_modules", "server.R"), local = app_env)

shinyApp(ui, server)
