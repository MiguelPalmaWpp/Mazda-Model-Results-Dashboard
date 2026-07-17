card <- function(title, ..., class = NULL) {
  tags$div(
    class = paste("card", class),
    tags$div(class = "card-header", title),
    tags$div(class = "card-body", ...)
  )
}

dt_table <- function(data, page_length = 10, scroll_x = TRUE) {
  data <- round_numeric_columns(data, 2)
  numeric_cols <- which(vapply(data, is.numeric, logical(1)))

  table <- datatable(
    data,
    rownames = FALSE,
    filter = "top",
    extensions = "Buttons",
    options = list(
      dom = "<'dt-toolbar'<'dt-buttons-wrap'B><'dt-search-wrap'f>>rt<'dt-footer'<'dt-info-wrap'i><'dt-page-wrap'p>>",
      buttons = list(
        list(extend = "copy", className = "btn-dt"),
        list(extend = "csv", className = "btn-dt"),
        list(extend = "excel", className = "btn-dt")
      ),
      pageLength = page_length,
      lengthMenu = c(5, 10, 25, 50, 100),
      scrollX = scroll_x,
      autoWidth = TRUE,
      columnDefs = list(
        list(className = "dt-right", targets = numeric_cols - 1),
        list(className = "dt-left", targets = setdiff(seq_along(data), numeric_cols) - 1)
      ),
      initComplete = JS("dtBlueCallback"),
      language = list(
        search = "",
        searchPlaceholder = "Search table",
        info = "Showing _START_ to _END_ of _TOTAL_ rows",
        paginate = list(previous = "Previous", `next` = "Next")
      )
    ),
    class = "compact stripe hover row-border order-column mazda-dt"
  )

  if (length(numeric_cols) > 0) {
    table <- formatRound(table, columns = numeric_cols, digits = 2, mark = ",")
  }

  table
}

metrics_matrix_table <- function(data) {
  data <- round_numeric_columns(data, 2)
  numeric_cols <- which(vapply(data, is.numeric, logical(1)))

  table <- datatable(
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
    class = "compact stripe hover metrics-matrix mazda-dt"
  )

  if (length(numeric_cols) > 0) {
    table <- formatRound(table, columns = numeric_cols, digits = 2, mark = ",")
  }

  table
}
