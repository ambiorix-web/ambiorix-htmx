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

#' Is app running in prod?
#'
#' @return Logical.
in_prod <- function() {
  identical(
    Sys.getenv("APP_ENV"),
    "prod"
  )
}

#' Create an anchor tag's href attribute
#'
#' @description
#' Generates the `href` for an anchor (`<a>`) tag. If the application is
#' running in a production environment, the given `path` is prefixed to
#' the `href` to ensure the correct base URL is used.
#'
#' @param href String /// Required. `href` attribute of an anchor tag (e.g., "/about").
#' @param base_path String /// Optional. Base path on which the app is deployed. eg.,
#' if the app is deployed at `https://try.ambiorix.dev/infinite-scroll`,
#' the environment variable `APP_BASE_PATH` should be set to `/infinite-scroll`.
#' The default value is obtained from the `APP_BASE_PATH` environment variable,
#' or it can be passed directly.
#'
#' @return String. The complete `href` for the anchor tag.
#'
#' @examples
#' # In production, this may return "/infinite-scroll/about":
#' create_href("/about")
#'
#' @export
create_href <- function(
  href,
  base_path = Sys.getenv("APP_BASE_PATH")
) {
  if (in_prod()) {
    href <- paste0(base_path, href)
  }

  href
}

#' Generic html page
#'
#' @param head_tags [htmltools::tagList()] /// Optional. Tags
#' to include in the head of the html document.
#' @param body [htmltools::tagList()] /// Optional. Tags to
#' include in the body of the html document.
#' @export
page <- function(
  head_tags = NULL,
  body = NULL
) {
  tags$html(
    tags$head(
      head_tags,
      tags$script(
        src = create_href(href = "/assets/htmx-2.0.4.min.js")
      )
    ),
    tags$body(
      body
    )
  )
}

#' Home page
#'
#' @export
home_page <- function() {
  input <- tags$input(
    type = "search",
    name = "search",
    placeholder = "Search car model...",
    `hx-get` = create_href(href = "/search"),
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
#' @param data data.frame or list object /// Required.
#' @return [htmltools::tagList()]
#' @export
create_table_rows <- function(data) {
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
#' @param pattern String /// Optional. Pattern to search for
#' in the column 'model' of mtcars.
#' Defaults to `NULL`.
#' @export
search_mtcars <- function(pattern = NULL) {
  if (is.null(pattern) || is.na(pattern)) {
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

app <- Ambiorix$new(port = 8000L)
app$static("public", "assets")

app$get("/", home_get)
app$get("/search", search_get)

app$start()
