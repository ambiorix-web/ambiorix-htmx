box::use(
  ambiorix[Ambiorix],
  babynames[babynames],
  htmltools[tags, tagList],
)

babynames <- as.data.frame(babynames)

#' Filter rows from `babynames` with pagination
#'
#' @param page Integer. Page number to retrieve. Default
#' is 1.
#' @param page_length Integer. Number of rows per page. Default
#' is 15.
#' @return Named list:
#' - `filtered`: data.frame object containing the rows for
#' the requested page.
#' - `next_page`: The next page number as an integer, or `NULL`
#' if the current page is the last one.
#' @examples
#' # default:
#' filter_babynames()
#'
#' # retrieve the second page of results, with 10 rows per page:
#' filter_babynames(page = 2L, page_length = 10L)
#'
#' # attempt to retrieve a page beyond the available data:
#' filter_babynames(page = 1000000)
#' @export
filter_babynames <- \(page = 1L, page_length = 15L) {
  n <- nrow(babynames)

  start_idx <- (page - 1L) * page_length + 1L
  if (start_idx > n) {
    empty <- list(
      filtered = babynames[integer(), ],
      next_page = NULL
    )
    return(empty)
  }

  end_idx <- min(page * page_length, n)
  is_last_page <- identical(end_idx, n)
  next_page <- if (!is_last_page) page + 1L

  row_idx <- seq(from = start_idx, to = end_idx, by = 1L)

  list(
    filtered = babynames[row_idx, ],
    next_page = next_page
  )
}

#' Create a generic HTML page
#'
#' @param head [htmltools::tagList()] Tag list containing
#' items to include in the head of the HTML document.
#' @param body [htmltools::tagList()] Tag list containing
#' items to include in the body of the HTML document.
#' @return [htmltools::tags$html]
#' @export
html_page <- \(head = NULL, body = NULL) {
  tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      tags$meta(
        name = "color-scheme",
        content = "light dark"
      ),
      tags$link(
        rel = "stylesheet",
        href = "/assets/pico-2.0.6.min.css"
      ),
      head
    ),
    tags$body(body)
  )
}

#' Create an HTML table
#'
#' @param data data.frame object to use.
#' @return [htmltools::tags$table]
#' @export
html_table <- \(data) {
  rows <- nrow(data)
  cols <- ncol(data)
  row_names <- rownames(data)
  data <- as.list(data)

  table_head <- tags$thead(
    tags$tr(
      lapply(
        X = c("#", names(data)),
        FUN = \(col_name) {
          tags$th(
            scope = "col",
            col_name
          )
        }
      )
    )
  )

  table_records <- lapply(
    X = seq_len(rows),
    FUN = \(row_idx) {
      cell_data <- lapply(
        X = seq_len(cols),
        FUN = \(col_idx) {
          tags$td(data[[col_idx]][[row_idx]])
        }
      )

      tags$tr(
        tags$th(
          scope = "row",
          row_names[[row_idx]]
        ),
        cell_data
      )
    }
  )

  table_body <- tags$tbody(table_records)

  tags$table(
    table_head,
    table_body
  )
}

#' Home page
#'
#' @export
home_page <- \() {
  first_fifteen <- filter_babynames()$filtered
  table <- html_table(data = first_fifteen)

  body <- tagList(
    tags$header(
      class = "container",
      tags$hgroup(
        tags$h2("Infinite Scroll"),
        tags$p("Get Inspired")
      )
    ),
    tags$main(
      class = "container",
      table
    ),
    tags$footer(
      class = "container",
      tags$p("courtesy of ambiorix + htmx🚀")
    )
  )

  html_page(body = body)
}

#' Handle GET at '/'
#'
#' @export
home_get <- \(req, res) {
  res$send(home_page())
}

#' Global error handler for app
#'
#' @param req The request object.
#' @param res The response object.
#' @param error The error object. See [stop()].
#' @export
error_handler <- \(req, res, error) {
  message(conditionMessage(error))
  res$status <- 500L
  res$send("Internal Server Error :(")
}

Ambiorix$new(port = 5000L)$
  set_error(error_handler)$
  static("public", "assets")$
  get("/", home_get)$
  start()
