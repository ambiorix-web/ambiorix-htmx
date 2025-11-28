box::use(
  htmltools[tags, tagList],
  .. / .. / helpers / to_json[to_json],
)

#' Generate html to create a datatable
#'
#' Meant to be used for serverside rendering of
#' [datatables](https://datatables.net/)
#'
#' @param col_names Character vector. Column names of the data.
#' @param table_id String. Table id.
#' @param table_class String. Bootstrap classes to apply to the table.
#' @param ... Named `key=value` pairs. Table options. Don't include 'columns'.
#' See [datatable options](https://datatables.net/manual/options).
#' @param header_row_class String. Classes to apply to the header row.
#' @param header_cell_class String. Classes to apply to the header cells.
#' @return An object of class `shiny.tag`
#' @export
datatable <- \(
  ...,
  col_names,
  table_id,
  table_class = NULL,
  header_row_class = NULL,
  header_cell_class = NULL
) {
  tagList(
    tags$div(
      class = "table-responsive",
      tags$table(
        id = table_id,
        class = paste("table", table_class),
        tags$thead(
          class = header_row_class,
          tags$tr(
            lapply(col_names, \(col_name) {
              tags$th(
                scope = "col",
                class = header_cell_class,
                tags$span(class = "card-title", col_name)
              )
            })
          )
        ),
        tags$tbody()
      )
    ),
    tags$script(
      datatable_script(
        id = table_id,
        col_names = col_names,
        ...
      )
    )
  )
}

#' Make a script to convert an html table to datatable
#'
#' @param id String. Table id.
#' @param col_names Character vector. Column names of the data.
#' @param ... Named `key=value` pairs. Table options. Don't include 'columns'.
#' See [datatable options](https://datatables.net/manual/options).
#' @examples
#' script <- datatable_script(
#'   id = "datatable",
#'   col_names = names(iris),
#'   processing = TRUE,
#'   serverSide = TRUE,
#'   ajax = "/api/data"
#' )
#'
#' cat(script, "\n")
#' @return String. JavaScript code.
#' @export
datatable_script <- \(id, col_names, ...) {
  columns <- lapply(
    X = col_names,
    FUN = \(name) {
      list(
        data = list(name)
      )
    }
  )

  opts <- list(
    ...,
    columns = columns
  ) |>
    to_json()

  sprintf(
    "
    $(document).ready(function() {
      $('#%s')
      .on('draw.dt', function () {
        $('ul.pagination').addClass('pagination-sm');
      })
      .DataTable(%s);
    });
    ",
    id,
    opts
  )
}

#' My custom datatable options
#'
#' A list.
#'
#' @export
datatable_options <- list(
  table_class = "table-hover table-bordered table-sm w-100",
  processing = TRUE,
  serverSide = TRUE,
  searchDelay = 1500,
  # disable sorting of cols:
  ordering = FALSE,
  columnDefs = list(
    list(
      # handle missing values:
      defaultContent = "",
      # avoid cell wrap:
      className = "dt-nowrap",
      targets = "_all"
    )
  ),
  # enable horizontal scrolling:
  scrollX = TRUE
)
