box::use(
  htmltools[
    tags,
    tagList,
  ],
  datasets[mtcars],
  ambiorix[Ambiorix],
)

# put the data in good shape:
nms <- colnames(mtcars)
mtcars$model <- rownames(mtcars)
new_col_order <- c("model", nms)
mtcars <- mtcars[, new_col_order]

#' Generic html page
#'
#' @param head_tags [htmltools::tagList()] containing tags
#' to include in the head of the html document.
#' @param body [htmltools::tagList()] containing tags to
#' include in the body of the html document.
#' @export
page <- \(head_tags = NULL, body = NULL) {
  tags$html(
    tags$head(
      head_tags,
      tags$script(src = "/assets/htmx-2.0.4.min.js")
    ),
    tags$body(
      body
    )
  )
}

#' Home page
#'
#' @export
home_page <- \() {
  input <- tags$input(
    type = "search",
    name = "search",
    placeholder = "Search car model...",
    `hx-get` = "/search",
    `hx-trigger` = "input changed delay:500ms, keyup[key=='Enter'], load",
    `hx-target` = "#search-results"
  )

  table <- tags$table(
    tags$thead(
      tags$tr(
        lapply(names(mtcars), tags$th)
      )
    ),
    tags$tbody(id = "search-results")
  )

  out <- tagList(
    tags$h3("Search mtcars"),
    input,
    table
  )

  head_tags <- tagList(
    tags$title("Home")
  )

  page(head_tags = head_tags, body = out)
}

#' Create html table rows
#'
#' @param data data.frame or list object to use.
#' @return [htmltools::tagList()]
#' @export
create_table_rows <- \(data) {
  data <- as.list(data)

  if (identical(length(data), 0L)) {
    return(tagList())
  }

  n <- length(data[[1L]])

  rows <- lapply(
    X = seq_len(n),
    FUN = \(row_idx) {
      record <- lapply(
        X = seq_along(data),
        FUN = \(col_idx) {
          cell_value <- data[[col_idx]][[row_idx]]
          tags$td(cell_value)
        }
      )

      tags$tr(record)
    }
  )

  tagList(rows)
}

#' Search for car model
#'
#' @param pattern String. Pattern to search for
#' in the column 'model' of mtcars.
#' Defaults to `NULL`.
#' @export
search_mtcars <- \(pattern = NULL) {
  if (is.null(pattern)) {
    return(mtcars)
  }

  mtcars[
    grepl(
      pattern = pattern,
      x = mtcars[["model"]],
      ignore.case = TRUE
    ),
  ]
}

#' Handle GET at '/'
#'
#' @export
home_get <- \(req, res) {
  res$send(home_page())
}

#' Handle GET at '/search'
#'
#' @export
search_get <- \(req, res) {
  data <- search_mtcars(pattern = req$query$search)
  html <- create_table_rows(data = data)
  res$send(html)
}

#' Error handler for app
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

Ambiorix$new(port = 8000L)$
  set_error(error_handler)$
  static("public", "assets")$
  get("/", home_get)$
  get("/search", search_get)$
  start()
