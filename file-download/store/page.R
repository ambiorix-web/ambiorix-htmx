box::use(
  htmltools[
    tags,
    HTML,
    tagList,
  ],
  .. /
    helpers /
    create_href[
      create_href,
    ],
)

#' Generic UI page
#'
#' A generic bootstrap UI page.
#'
#' @param ... [htmltools::tags] Passed to the HTML
#' document body.
#' @param title String. Browser title.
#'
#' @return [htmltools::tagList]
#'
#' @export
page <- function(..., title = "File Download") {
  tagList(
    HTML("<!doctype html>"),
    tags$html(
      lang = "en",
      tags$head(
        tags$meta(charset = "utf-8"),
        tags$meta(
          name = "viewport",
          content = "width=device-width, initial-scale=1"
        ),
        tags$title(title),
        tags$link(
          rel = "icon",
          type = "image/x-icon",
          href = create_href(href = "/static/r-logo.png")
        ),
        tags$link(
          rel = "stylesheet",
          href = create_href(
            href = "/static/bootstrap-5.3.3-dist/bootstrap.min.css"
          )
        ),
        tags$link(
          rel = "stylesheet",
          href = create_href(href = "/static/styles.css")
        ),
        tags$link(
          rel = "stylesheet",
          href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
        ),
        tags$script(
          type = "text/javascript",
          src = create_href(href = "/static/htmx-1.9.12.min.js")
        ),
        tags$link(
          href = "https://cdn.datatables.net/v/bs5/jq-3.7.0/dt-1.13.8/datatables.min.css",
          rel = "stylesheet"
        ),
        tags$script(
          src = "https://cdn.datatables.net/v/bs5/jq-3.7.0/dt-1.13.8/datatables.min.js"
        )
      ),
      tags$body(
        class = "spectral-regular",
        `hx-encoding` = "multipart/form-data",
        tags$div(
          class = "d-flex flex-row min-vh-100 min-vw-100",
          ...
        ),
        tags$script(
          type = "text/javascript",
          src = create_href(
            href = "/static/bootstrap-5.3.3-dist/bootstrap.bundle.min.js"
          )
        )
      )
    )
  )
}
