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
        href = "/assets/styles.css"
      ),
      tags$link(
        rel = "stylesheet",
        href = "/assets/pico-2.0.6.min.css"
      ),
      tags$script(src = "/assets/htmx-2.0.4.min.js"),
      head
    ),
    tags$body(body)
  )
}

#' Create an HTML table
#'
#' @param data data.frame object to use.
#' @param next_page Integer. The next page number.
#' Passed to `hx-get` of the last table row.
#' Default is NULL.
#' @param type Type of data to return. Either
#' "full" (default) to return the full html table,
#' or "records" to return the rows alone.
#' @return [htmltools::tags]
#' @export
html_table <- \(
  data,
  next_page = NULL,
  type = c("full", "records")
) {
  type <- match.arg(arg = type)
  nrows <- nrow(data)
  ncols <- ncol(data)
  row_names <- rownames(data)
  data <- as.list(data)


  table_records <- lapply(
    X = seq_len(nrows),
    FUN = \(row_idx) {
      cell_data <- lapply(
        X = seq_len(ncols),
        FUN = \(col_idx) {
          tags$td(
            data[[col_idx]][[row_idx]]
          )
        }
      )

      is_last_row <- identical(row_idx, nrows) && !is.null(next_page)
      hx_get <- if (is_last_row) paste0("/babynames?page=", next_page)

      tags$tr(
        `hx-get` = hx_get,
        `hx-trigger` = "revealed",
        `hx-swap` = "afterend",
        `hx-indicator` = "#babynames-loading-spinner",
        tags$th(
          scope = "row",
          row_names[[row_idx]]
        ),
        cell_data
      )
    }
  )

  if (identical(type, "records")) {
    return(table_records)
  }

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

  table_body <- tags$tbody(table_records)

  table <- tags$table(
    class = "striped",
    table_head,
    table_body
  )

  loading_spinner <- tags$div(
    id = "babynames-loading-spinner",
    class = "htmx-indicator",
    `aria-busy` = "true"
  )

  tags$article(
    tags$header("Babynames"),
    tags$main(table),
    tags$footer(loading_spinner)
  )
}

#' Home page
#'
#' @export
home_page <- \() {
  first_fifteen <- filter_babynames()
  table <- html_table(
    data = first_fifteen$filtered,
    next_page = first_fifteen$next_page
  )

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
      tags$p("ambiorix + htmx🚀")
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

#' Handle GET at '/babynames'
#'
#' @export
get_babynames <- \(req, res) {
  page <- as.integer(req$query$page)
  data <- filter_babynames(page = page)
  html <- tagList(
    html_table(
      data = data$filtered,
      next_page = data$next_page,
      type = "records"
    )
  )

  # simulate a short delay so that spinner can be seen, lol:
  Sys.sleep(0.5)

  res$send(html)
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
  get("/babynames", get_babynames)$
  start()
