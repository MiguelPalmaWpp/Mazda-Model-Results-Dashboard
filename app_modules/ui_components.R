card <- function(title, ..., class = NULL) {
  tags$div(
    class = paste("card", class),
    tags$div(class = "card-header", title),
    tags$div(class = "card-body", ...)
  )
}

dt_table <- function(data, page_length = 10, scroll_x = TRUE) {
  data <- round_numeric_columns(data, 3)

  datatable(
    data,
    rownames = FALSE,
    filter = "top",
    extensions = "Buttons",
    options = list(
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel"),
      pageLength = page_length,
      lengthMenu = c(5, 10, 25, 50, 100),
      scrollX = scroll_x,
      autoWidth = TRUE,
      initComplete = JS("dtBlueCallback")
    ),
    class = "compact stripe hover"
  )
}

metrics_matrix_table <- function(data) {
  data <- round_numeric_columns(data, 3)

  datatable(
    data,
    rownames = FALSE,
    options = list(
      dom = "t",
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      info = FALSE,
      autoWidth = TRUE,
      columnDefs = list(
        list(className = "dt-left", targets = 0),
        list(className = "dt-right", targets = 1:3)
      ),
      initComplete = JS("dtBlueCallback")
    ),
    class = "compact stripe hover metrics-matrix"
  )
}
